use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressingMode {
    Implied,
    Immediate8,
    Absolute,
    AbsoluteLong,
}

#[derive(Debug, Clone, Copy)]
pub enum OperandShape {
    None,
    Immediate(i64),
    Address {
        literal: Option<u32>,
        force_far: bool,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Encoding {
    pub opcode: u8,
    pub mode: AddressingMode,
}

#[derive(Debug, Error)]
pub enum EncodeError {
    #[error("unknown mnemonic '{mnemonic}'")]
    UnknownMnemonic { mnemonic: String },
    #[error("mnemonic '{mnemonic}' does not accept this operand")]
    InvalidOperand { mnemonic: String },
    #[error("mnemonic '{mnemonic}' has no long form")]
    NoLongForm { mnemonic: String },
    #[error("immediate value out of range for 8-bit operand")]
    ImmediateOutOfRange,
}

pub fn select_encoding(mnemonic: &str, operand: OperandShape) -> Result<Encoding, EncodeError> {
    let lower = mnemonic.to_ascii_lowercase();
    match lower.as_str() {
        "nop" => expect_none(mnemonic, operand, 0xEA),
        "rts" => expect_none(mnemonic, operand, 0x60),
        "rtl" => expect_none(mnemonic, operand, 0x6B),
        "jsr" => expect_address(mnemonic, operand, false, 0x20, 0x00),
        "jsl" => expect_address(mnemonic, operand, true, 0x00, 0x22),
        "lda" => match operand {
            OperandShape::Immediate(value) => {
                if (0..=0xFF).contains(&value) {
                    Ok(Encoding {
                        opcode: 0xA9,
                        mode: AddressingMode::Immediate8,
                    })
                } else {
                    Err(EncodeError::ImmediateOutOfRange)
                }
            }
            OperandShape::Address { literal, force_far } => {
                if force_far {
                    return Ok(Encoding {
                        opcode: 0xAF,
                        mode: AddressingMode::AbsoluteLong,
                    });
                }
                if literal.is_some_and(|value| value > 0xFFFF) {
                    return Ok(Encoding {
                        opcode: 0xAF,
                        mode: AddressingMode::AbsoluteLong,
                    });
                }
                Ok(Encoding {
                    opcode: 0xAD,
                    mode: AddressingMode::Absolute,
                })
            }
            OperandShape::None => Err(EncodeError::InvalidOperand {
                mnemonic: mnemonic.to_string(),
            }),
        },
        _ => Err(EncodeError::UnknownMnemonic {
            mnemonic: mnemonic.to_string(),
        }),
    }
}

pub fn operand_width(mode: AddressingMode) -> usize {
    match mode {
        AddressingMode::Implied => 0,
        AddressingMode::Immediate8 => 1,
        AddressingMode::Absolute => 2,
        AddressingMode::AbsoluteLong => 3,
    }
}

fn expect_none(mnemonic: &str, operand: OperandShape, opcode: u8) -> Result<Encoding, EncodeError> {
    match operand {
        OperandShape::None => Ok(Encoding {
            opcode,
            mode: AddressingMode::Implied,
        }),
        OperandShape::Address {
            force_far: true, ..
        } => Err(EncodeError::NoLongForm {
            mnemonic: mnemonic.to_string(),
        }),
        _ => Err(EncodeError::InvalidOperand {
            mnemonic: mnemonic.to_string(),
        }),
    }
}

fn expect_address(
    mnemonic: &str,
    operand: OperandShape,
    expects_far: bool,
    abs_opcode: u8,
    long_opcode: u8,
) -> Result<Encoding, EncodeError> {
    match operand {
        OperandShape::Address {
            force_far: true, ..
        } if !expects_far => Err(EncodeError::NoLongForm {
            mnemonic: mnemonic.to_string(),
        }),
        OperandShape::Address {
            force_far: false, ..
        } if expects_far => Ok(Encoding {
            opcode: long_opcode,
            mode: AddressingMode::AbsoluteLong,
        }),
        OperandShape::Address {
            force_far: true, ..
        } if expects_far => Ok(Encoding {
            opcode: long_opcode,
            mode: AddressingMode::AbsoluteLong,
        }),
        OperandShape::Address {
            force_far: false, ..
        } => Ok(Encoding {
            opcode: abs_opcode,
            mode: AddressingMode::Absolute,
        }),
        _ => Err(EncodeError::InvalidOperand {
            mnemonic: mnemonic.to_string(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn far_is_rejected_for_nop() {
        let err = select_encoding(
            "nop",
            OperandShape::Address {
                literal: Some(1),
                force_far: true,
            },
        )
        .expect_err("must fail");
        assert!(matches!(err, EncodeError::NoLongForm { .. }));
    }
}
