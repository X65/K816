use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddressingMode {
    Implied,
    Accumulator,
    Immediate8,
    Immediate16,
    ImmediateM,
    ImmediateX,
    DirectPage,
    DirectPageX,
    DirectPageY,
    DirectPageIndirect,
    DirectPageIndirectLong,
    DirectPageIndexedIndirectX,
    DirectPageIndirectIndexedY,
    DirectPageIndirectLongIndexedY,
    StackRelative,
    StackRelativeIndirectIndexedY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    AbsoluteLong,
    AbsoluteLongX,
    AbsoluteIndirect,
    AbsoluteIndexedIndirectX,
    AbsoluteIndirectLong,
    Relative8,
    Relative16,
    BlockMove,
}

#[derive(Debug, Clone, Copy)]
pub enum OperandShape {
    None,
    Immediate(i64),
    Address {
        literal: Option<u32>,
        force_far: bool,
        mode: AddressOperandMode,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressOperandMode {
    Direct { index: Option<IndexRegister> },
    Indirect,
    IndexedIndirectX,
    IndirectIndexedY,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexRegister {
    X,
    Y,
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
    #[error("immediate value out of range for the selected operand width")]
    ImmediateOutOfRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpcodeDescriptor {
    pub mnemonic: &'static str,
    pub mode: AddressingMode,
}

#[derive(Debug)]
pub struct DecodedInstruction<'a> {
    pub opcode: u8,
    pub mnemonic: &'static str,
    pub mode: AddressingMode,
    pub operand: &'a [u8],
}

impl DecodedInstruction<'_> {
    pub fn len(&self) -> usize {
        1 + self.operand.len()
    }
}

#[derive(Debug, Error)]
pub enum DecodeError {
    #[error("cannot decode instruction from empty byte slice")]
    EmptyInput,
    #[error(
        "truncated instruction for opcode {opcode:#04X}: expected {expected} operand bytes, got {actual}"
    )]
    Truncated {
        opcode: u8,
        expected: usize,
        actual: usize,
    },
}

macro_rules! op {
    ($mnemonic:literal, $mode:ident) => {
        OpcodeDescriptor {
            mnemonic: $mnemonic,
            mode: AddressingMode::$mode,
        }
    };
}

const OPCODE_TABLE: [OpcodeDescriptor; 256] = [
    // 0x00
    op!("brk", Immediate8),
    op!("ora", DirectPageIndexedIndirectX),
    op!("cop", Immediate8),
    op!("ora", StackRelative),
    op!("tsb", DirectPage),
    op!("ora", DirectPage),
    op!("asl", DirectPage),
    op!("ora", DirectPageIndirectLong),
    op!("php", Implied),
    op!("ora", ImmediateM),
    op!("asl", Accumulator),
    op!("phd", Implied),
    op!("tsb", Absolute),
    op!("ora", Absolute),
    op!("asl", Absolute),
    op!("ora", AbsoluteLong),
    // 0x10
    op!("bpl", Relative8),
    op!("ora", DirectPageIndirectIndexedY),
    op!("ora", DirectPageIndirect),
    op!("ora", StackRelativeIndirectIndexedY),
    op!("trb", DirectPage),
    op!("ora", DirectPageX),
    op!("asl", DirectPageX),
    op!("ora", DirectPageIndirectLongIndexedY),
    op!("clc", Implied),
    op!("ora", AbsoluteY),
    op!("inc", Accumulator),
    op!("tcs", Implied),
    op!("trb", Absolute),
    op!("ora", AbsoluteX),
    op!("asl", AbsoluteX),
    op!("ora", AbsoluteLongX),
    // 0x20
    op!("jsr", Absolute),
    op!("and", DirectPageIndexedIndirectX),
    op!("jsl", AbsoluteLong),
    op!("and", StackRelative),
    op!("bit", DirectPage),
    op!("and", DirectPage),
    op!("rol", DirectPage),
    op!("and", DirectPageIndirectLong),
    op!("plp", Implied),
    op!("and", ImmediateM),
    op!("rol", Accumulator),
    op!("pld", Implied),
    op!("bit", Absolute),
    op!("and", Absolute),
    op!("rol", Absolute),
    op!("and", AbsoluteLong),
    // 0x30
    op!("bmi", Relative8),
    op!("and", DirectPageIndirectIndexedY),
    op!("and", DirectPageIndirect),
    op!("and", StackRelativeIndirectIndexedY),
    op!("bit", DirectPageX),
    op!("and", DirectPageX),
    op!("rol", DirectPageX),
    op!("and", DirectPageIndirectLongIndexedY),
    op!("sec", Implied),
    op!("and", AbsoluteY),
    op!("dec", Accumulator),
    op!("tsc", Implied),
    op!("bit", AbsoluteX),
    op!("and", AbsoluteX),
    op!("rol", AbsoluteX),
    op!("and", AbsoluteLongX),
    // 0x40
    op!("rti", Implied),
    op!("eor", DirectPageIndexedIndirectX),
    op!("wdm", Immediate8),
    op!("eor", StackRelative),
    op!("mvp", BlockMove),
    op!("eor", DirectPage),
    op!("lsr", DirectPage),
    op!("eor", DirectPageIndirectLong),
    op!("pha", Implied),
    op!("eor", ImmediateM),
    op!("lsr", Accumulator),
    op!("phk", Implied),
    op!("jmp", Absolute),
    op!("eor", Absolute),
    op!("lsr", Absolute),
    op!("eor", AbsoluteLong),
    // 0x50
    op!("bvc", Relative8),
    op!("eor", DirectPageIndirectIndexedY),
    op!("eor", DirectPageIndirect),
    op!("eor", StackRelativeIndirectIndexedY),
    op!("mvn", BlockMove),
    op!("eor", DirectPageX),
    op!("lsr", DirectPageX),
    op!("eor", DirectPageIndirectLongIndexedY),
    op!("cli", Implied),
    op!("eor", AbsoluteY),
    op!("phy", Implied),
    op!("tcd", Implied),
    op!("jml", AbsoluteLong),
    op!("eor", AbsoluteX),
    op!("lsr", AbsoluteX),
    op!("eor", AbsoluteLongX),
    // 0x60
    op!("rts", Implied),
    op!("adc", DirectPageIndexedIndirectX),
    op!("per", Relative16),
    op!("adc", StackRelative),
    op!("stz", DirectPage),
    op!("adc", DirectPage),
    op!("ror", DirectPage),
    op!("adc", DirectPageIndirectLong),
    op!("pla", Implied),
    op!("adc", ImmediateM),
    op!("ror", Accumulator),
    op!("rtl", Implied),
    op!("jmp", AbsoluteIndirect),
    op!("adc", Absolute),
    op!("ror", Absolute),
    op!("adc", AbsoluteLong),
    // 0x70
    op!("bvs", Relative8),
    op!("adc", DirectPageIndirectIndexedY),
    op!("adc", DirectPageIndirect),
    op!("adc", StackRelativeIndirectIndexedY),
    op!("stz", DirectPageX),
    op!("adc", DirectPageX),
    op!("ror", DirectPageX),
    op!("adc", DirectPageIndirectLongIndexedY),
    op!("sei", Implied),
    op!("adc", AbsoluteY),
    op!("ply", Implied),
    op!("tdc", Implied),
    op!("jmp", AbsoluteIndexedIndirectX),
    op!("adc", AbsoluteX),
    op!("ror", AbsoluteX),
    op!("adc", AbsoluteLongX),
    // 0x80
    op!("bra", Relative8),
    op!("sta", DirectPageIndexedIndirectX),
    op!("brl", Relative16),
    op!("sta", StackRelative),
    op!("sty", DirectPage),
    op!("sta", DirectPage),
    op!("stx", DirectPage),
    op!("sta", DirectPageIndirectLong),
    op!("dey", Implied),
    op!("bit", ImmediateM),
    op!("txa", Implied),
    op!("phb", Implied),
    op!("sty", Absolute),
    op!("sta", Absolute),
    op!("stx", Absolute),
    op!("sta", AbsoluteLong),
    // 0x90
    op!("bcc", Relative8),
    op!("sta", DirectPageIndirectIndexedY),
    op!("sta", DirectPageIndirect),
    op!("sta", StackRelativeIndirectIndexedY),
    op!("sty", DirectPageX),
    op!("sta", DirectPageX),
    op!("stx", DirectPageY),
    op!("sta", DirectPageIndirectLongIndexedY),
    op!("tya", Implied),
    op!("sta", AbsoluteY),
    op!("txs", Implied),
    op!("txy", Implied),
    op!("stz", Absolute),
    op!("sta", AbsoluteX),
    op!("stz", AbsoluteX),
    op!("sta", AbsoluteLongX),
    // 0xA0
    op!("ldy", ImmediateX),
    op!("lda", DirectPageIndexedIndirectX),
    op!("ldx", ImmediateX),
    op!("lda", StackRelative),
    op!("ldy", DirectPage),
    op!("lda", DirectPage),
    op!("ldx", DirectPage),
    op!("lda", DirectPageIndirectLong),
    op!("tay", Implied),
    op!("lda", ImmediateM),
    op!("tax", Implied),
    op!("plb", Implied),
    op!("ldy", Absolute),
    op!("lda", Absolute),
    op!("ldx", Absolute),
    op!("lda", AbsoluteLong),
    // 0xB0
    op!("bcs", Relative8),
    op!("lda", DirectPageIndirectIndexedY),
    op!("lda", DirectPageIndirect),
    op!("lda", StackRelativeIndirectIndexedY),
    op!("ldy", DirectPageX),
    op!("lda", DirectPageX),
    op!("ldx", DirectPageY),
    op!("lda", DirectPageIndirectLongIndexedY),
    op!("clv", Implied),
    op!("lda", AbsoluteY),
    op!("tsx", Implied),
    op!("tyx", Implied),
    op!("ldy", AbsoluteX),
    op!("lda", AbsoluteX),
    op!("ldx", AbsoluteY),
    op!("lda", AbsoluteLongX),
    // 0xC0
    op!("cpy", ImmediateX),
    op!("cmp", DirectPageIndexedIndirectX),
    op!("rep", Immediate8),
    op!("cmp", StackRelative),
    op!("cpy", DirectPage),
    op!("cmp", DirectPage),
    op!("dec", DirectPage),
    op!("cmp", DirectPageIndirectLong),
    op!("iny", Implied),
    op!("cmp", ImmediateM),
    op!("dex", Implied),
    op!("wai", Implied),
    op!("cpy", Absolute),
    op!("cmp", Absolute),
    op!("dec", Absolute),
    op!("cmp", AbsoluteLong),
    // 0xD0
    op!("bne", Relative8),
    op!("cmp", DirectPageIndirectIndexedY),
    op!("cmp", DirectPageIndirect),
    op!("cmp", StackRelativeIndirectIndexedY),
    op!("pei", DirectPageIndirect),
    op!("cmp", DirectPageX),
    op!("dec", DirectPageX),
    op!("cmp", DirectPageIndirectLongIndexedY),
    op!("cld", Implied),
    op!("cmp", AbsoluteY),
    op!("phx", Implied),
    op!("stp", Implied),
    op!("jmp", AbsoluteIndirectLong),
    op!("cmp", AbsoluteX),
    op!("dec", AbsoluteX),
    op!("cmp", AbsoluteLongX),
    // 0xE0
    op!("cpx", ImmediateX),
    op!("sbc", DirectPageIndexedIndirectX),
    op!("sep", Immediate8),
    op!("sbc", StackRelative),
    op!("cpx", DirectPage),
    op!("sbc", DirectPage),
    op!("inc", DirectPage),
    op!("sbc", DirectPageIndirectLong),
    op!("inx", Implied),
    op!("sbc", ImmediateM),
    op!("nop", Implied),
    op!("xba", Implied),
    op!("cpx", Absolute),
    op!("sbc", Absolute),
    op!("inc", Absolute),
    op!("sbc", AbsoluteLong),
    // 0xF0
    op!("beq", Relative8),
    op!("sbc", DirectPageIndirectIndexedY),
    op!("sbc", DirectPageIndirect),
    op!("sbc", StackRelativeIndirectIndexedY),
    op!("pea", Immediate16),
    op!("sbc", DirectPageX),
    op!("inc", DirectPageX),
    op!("sbc", DirectPageIndirectLongIndexedY),
    op!("sed", Implied),
    op!("sbc", AbsoluteY),
    op!("plx", Implied),
    op!("xce", Implied),
    op!("jsr", AbsoluteIndexedIndirectX),
    op!("sbc", AbsoluteX),
    op!("inc", AbsoluteX),
    op!("sbc", AbsoluteLongX),
];

pub fn opcode_descriptor(opcode: u8) -> OpcodeDescriptor {
    OPCODE_TABLE[usize::from(opcode)]
}

pub fn select_encoding(mnemonic: &str, operand: OperandShape) -> Result<Encoding, EncodeError> {
    let lower = mnemonic.to_ascii_lowercase();
    let modes = matching_modes(&lower);
    if modes.is_empty() {
        return Err(EncodeError::UnknownMnemonic {
            mnemonic: mnemonic.to_string(),
        });
    }

    match operand {
        OperandShape::None => {
            if let Some(opcode) = find_opcode(&lower, AddressingMode::Implied) {
                return Ok(Encoding {
                    opcode,
                    mode: AddressingMode::Implied,
                });
            }
            if let Some(opcode) = find_opcode(&lower, AddressingMode::Accumulator) {
                return Ok(Encoding {
                    opcode,
                    mode: AddressingMode::Accumulator,
                });
            }
            Err(EncodeError::InvalidOperand {
                mnemonic: mnemonic.to_string(),
            })
        }
        OperandShape::Immediate(value) => {
            let mut has_immediate_mode = false;
            for mode in [
                AddressingMode::Immediate8,
                AddressingMode::ImmediateM,
                AddressingMode::ImmediateX,
                AddressingMode::Immediate16,
            ] {
                if !modes.contains(&mode) {
                    continue;
                }
                has_immediate_mode = true;
                if immediate_fits(mode, value) {
                    let opcode =
                        find_opcode(&lower, mode).expect("mode presence should imply opcode");
                    return Ok(Encoding { opcode, mode });
                }
            }

            if has_immediate_mode {
                Err(EncodeError::ImmediateOutOfRange)
            } else {
                Err(EncodeError::InvalidOperand {
                    mnemonic: mnemonic.to_string(),
                })
            }
        }
        OperandShape::Address {
            literal,
            force_far,
            mode,
        } => {
            match mode {
                AddressOperandMode::Direct { index } => {
                    if index.is_none() {
                        if modes.contains(&AddressingMode::Relative8) {
                            if force_far {
                                return Err(EncodeError::InvalidOperand {
                                    mnemonic: mnemonic.to_string(),
                                });
                            }
                            let opcode = find_opcode(&lower, AddressingMode::Relative8)
                                .expect("relative mode should exist");
                            return Ok(Encoding {
                                opcode,
                                mode: AddressingMode::Relative8,
                            });
                        }
                        if modes.contains(&AddressingMode::Relative16) {
                            if force_far {
                                return Err(EncodeError::InvalidOperand {
                                    mnemonic: mnemonic.to_string(),
                                });
                            }
                            let opcode = find_opcode(&lower, AddressingMode::Relative16)
                                .expect("relative mode should exist");
                            return Ok(Encoding {
                                opcode,
                                mode: AddressingMode::Relative16,
                            });
                        }
                    }

                    let wants_long = force_far || literal.is_some_and(|value| value > 0xFFFF);
                    match index {
                        Some(IndexRegister::X) => {
                            if wants_long {
                                if let Some(opcode) =
                                    find_opcode(&lower, AddressingMode::AbsoluteLongX)
                                {
                                    return Ok(Encoding {
                                        opcode,
                                        mode: AddressingMode::AbsoluteLongX,
                                    });
                                }
                                return Err(EncodeError::NoLongForm {
                                    mnemonic: mnemonic.to_string(),
                                });
                            }

                            if let Some(opcode) = find_opcode(&lower, AddressingMode::AbsoluteX) {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::AbsoluteX,
                                });
                            }
                            if let Some(opcode) =
                                find_opcode(&lower, AddressingMode::AbsoluteLongX)
                            {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::AbsoluteLongX,
                                });
                            }
                            if let Some(opcode) =
                                find_opcode(&lower, AddressingMode::DirectPageX)
                            {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::DirectPageX,
                                });
                            }
                        }
                        Some(IndexRegister::Y) => {
                            if wants_long {
                                return Err(EncodeError::NoLongForm {
                                    mnemonic: mnemonic.to_string(),
                                });
                            }

                            if let Some(opcode) = find_opcode(&lower, AddressingMode::AbsoluteY) {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::AbsoluteY,
                                });
                            }
                            if let Some(opcode) =
                                find_opcode(&lower, AddressingMode::DirectPageY)
                            {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::DirectPageY,
                                });
                            }
                        }
                        None => {
                            if wants_long {
                                if let Some(opcode) = find_opcode(&lower, AddressingMode::AbsoluteLong)
                                {
                                    return Ok(Encoding {
                                        opcode,
                                        mode: AddressingMode::AbsoluteLong,
                                    });
                                }
                                return Err(EncodeError::NoLongForm {
                                    mnemonic: mnemonic.to_string(),
                                });
                            }

                            if let Some(opcode) = find_opcode(&lower, AddressingMode::Absolute) {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::Absolute,
                                });
                            }
                            if let Some(opcode) =
                                find_opcode(&lower, AddressingMode::AbsoluteLong)
                            {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::AbsoluteLong,
                                });
                            }
                        }
                    }
                }
                AddressOperandMode::Indirect => {
                    if force_far {
                        return Err(EncodeError::InvalidOperand {
                            mnemonic: mnemonic.to_string(),
                        });
                    }

                    if literal.is_some_and(|value| value <= 0xFF) {
                        if let Some(opcode) =
                            find_opcode(&lower, AddressingMode::DirectPageIndirect)
                        {
                            return Ok(Encoding {
                                opcode,
                                mode: AddressingMode::DirectPageIndirect,
                            });
                        }
                    }

                    if literal.is_some_and(|value| value > 0xFFFF) {
                        return Err(EncodeError::InvalidOperand {
                            mnemonic: mnemonic.to_string(),
                        });
                    }

                    if let Some(opcode) = find_opcode(&lower, AddressingMode::AbsoluteIndirect) {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::AbsoluteIndirect,
                        });
                    }
                    if let Some(opcode) =
                        find_opcode(&lower, AddressingMode::AbsoluteIndirectLong)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::AbsoluteIndirectLong,
                        });
                    }
                }
                AddressOperandMode::IndexedIndirectX => {
                    if force_far || literal.is_none() || literal.is_some_and(|value| value > 0xFF) {
                        return Err(EncodeError::InvalidOperand {
                            mnemonic: mnemonic.to_string(),
                        });
                    }

                    if let Some(opcode) =
                        find_opcode(&lower, AddressingMode::DirectPageIndexedIndirectX)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::DirectPageIndexedIndirectX,
                        });
                    }
                    if let Some(opcode) =
                        find_opcode(&lower, AddressingMode::AbsoluteIndexedIndirectX)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::AbsoluteIndexedIndirectX,
                        });
                    }
                }
                AddressOperandMode::IndirectIndexedY => {
                    if force_far || literal.is_none() || literal.is_some_and(|value| value > 0xFF) {
                        return Err(EncodeError::InvalidOperand {
                            mnemonic: mnemonic.to_string(),
                        });
                    }

                    if let Some(opcode) =
                        find_opcode(&lower, AddressingMode::DirectPageIndirectIndexedY)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::DirectPageIndirectIndexedY,
                        });
                    }
                }
            }

            if force_far {
                Err(EncodeError::NoLongForm {
                    mnemonic: mnemonic.to_string(),
                })
            } else {
                Err(EncodeError::InvalidOperand {
                    mnemonic: mnemonic.to_string(),
                })
            }
        }
    }
}

pub fn operand_width(mode: AddressingMode) -> usize {
    match mode {
        AddressingMode::Implied | AddressingMode::Accumulator => 0,
        AddressingMode::Immediate8
        | AddressingMode::ImmediateM
        | AddressingMode::ImmediateX
        | AddressingMode::DirectPage
        | AddressingMode::DirectPageX
        | AddressingMode::DirectPageY
        | AddressingMode::DirectPageIndirect
        | AddressingMode::DirectPageIndirectLong
        | AddressingMode::DirectPageIndexedIndirectX
        | AddressingMode::DirectPageIndirectIndexedY
        | AddressingMode::DirectPageIndirectLongIndexedY
        | AddressingMode::StackRelative
        | AddressingMode::StackRelativeIndirectIndexedY
        | AddressingMode::Relative8 => 1,
        AddressingMode::Immediate16
        | AddressingMode::Absolute
        | AddressingMode::AbsoluteX
        | AddressingMode::AbsoluteY
        | AddressingMode::AbsoluteIndirect
        | AddressingMode::AbsoluteIndexedIndirectX
        | AddressingMode::AbsoluteIndirectLong
        | AddressingMode::Relative16
        | AddressingMode::BlockMove => 2,
        AddressingMode::AbsoluteLong | AddressingMode::AbsoluteLongX => 3,
    }
}

pub fn decode_instruction(bytes: &[u8]) -> Result<DecodedInstruction<'_>, DecodeError> {
    let (&opcode, rest) = bytes.split_first().ok_or(DecodeError::EmptyInput)?;
    let descriptor = opcode_descriptor(opcode);
    let width = operand_width(descriptor.mode);
    if rest.len() < width {
        return Err(DecodeError::Truncated {
            opcode,
            expected: width,
            actual: rest.len(),
        });
    }

    Ok(DecodedInstruction {
        opcode,
        mnemonic: descriptor.mnemonic,
        mode: descriptor.mode,
        operand: &rest[..width],
    })
}

pub fn format_instruction(decoded: &DecodedInstruction<'_>, address: u32) -> String {
    let operand = format_operand(decoded.mode, decoded.operand, address);
    if operand.is_empty() {
        decoded.mnemonic.to_string()
    } else {
        format!("{} {}", decoded.mnemonic, operand)
    }
}

fn format_operand(mode: AddressingMode, operand: &[u8], address: u32) -> String {
    match mode {
        AddressingMode::Implied => String::new(),
        AddressingMode::Accumulator => "A".to_string(),
        AddressingMode::Immediate8 | AddressingMode::ImmediateM | AddressingMode::ImmediateX => {
            format!("#${:02X}", operand[0])
        }
        AddressingMode::Immediate16 => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("#${value:04X}")
        }
        AddressingMode::DirectPage => format!("${:02X}", operand[0]),
        AddressingMode::DirectPageX => format!("${:02X},X", operand[0]),
        AddressingMode::DirectPageY => format!("${:02X},Y", operand[0]),
        AddressingMode::DirectPageIndirect => format!("(${:02X})", operand[0]),
        AddressingMode::DirectPageIndirectLong => format!("[${:02X}]", operand[0]),
        AddressingMode::DirectPageIndexedIndirectX => format!("(${:02X},X)", operand[0]),
        AddressingMode::DirectPageIndirectIndexedY => format!("(${:02X}),Y", operand[0]),
        AddressingMode::DirectPageIndirectLongIndexedY => format!("[${:02X}],Y", operand[0]),
        AddressingMode::StackRelative => format!("${:02X},S", operand[0]),
        AddressingMode::StackRelativeIndirectIndexedY => format!("(${:02X},S),Y", operand[0]),
        AddressingMode::Absolute => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("${value:04X}")
        }
        AddressingMode::AbsoluteX => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("${value:04X},X")
        }
        AddressingMode::AbsoluteY => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("${value:04X},Y")
        }
        AddressingMode::AbsoluteLong => {
            let value = u32::from(operand[0])
                | (u32::from(operand[1]) << 8)
                | (u32::from(operand[2]) << 16);
            format!("${value:06X}")
        }
        AddressingMode::AbsoluteLongX => {
            let value = u32::from(operand[0])
                | (u32::from(operand[1]) << 8)
                | (u32::from(operand[2]) << 16);
            format!("${value:06X},X")
        }
        AddressingMode::AbsoluteIndirect => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("(${value:04X})")
        }
        AddressingMode::AbsoluteIndexedIndirectX => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("(${value:04X},X)")
        }
        AddressingMode::AbsoluteIndirectLong => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("[${value:04X}]")
        }
        AddressingMode::Relative8 => {
            let delta = i8::from_le_bytes([operand[0]]) as i32;
            let target = (address.wrapping_add(2)).wrapping_add_signed(delta) & 0x00FF_FFFF;
            format!("${target:06X}")
        }
        AddressingMode::Relative16 => {
            let delta = i16::from_le_bytes([operand[0], operand[1]]) as i32;
            let target = (address.wrapping_add(3)).wrapping_add_signed(delta) & 0x00FF_FFFF;
            format!("${target:06X}")
        }
        AddressingMode::BlockMove => format!("${:02X},${:02X}", operand[0], operand[1]),
    }
}

fn matching_modes(mnemonic: &str) -> Vec<AddressingMode> {
    OPCODE_TABLE
        .iter()
        .filter(|entry| entry.mnemonic == mnemonic)
        .map(|entry| entry.mode)
        .collect()
}

fn find_opcode(mnemonic: &str, mode: AddressingMode) -> Option<u8> {
    OPCODE_TABLE
        .iter()
        .position(|entry| entry.mnemonic == mnemonic && entry.mode == mode)
        .map(|index| index as u8)
}

fn immediate_fits(mode: AddressingMode, value: i64) -> bool {
    match mode {
        AddressingMode::Immediate8 | AddressingMode::ImmediateM | AddressingMode::ImmediateX => {
            (0..=0xFF).contains(&value)
        }
        AddressingMode::Immediate16 => (0..=0xFFFF).contains(&value),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decodes_common_opcode() {
        let decoded = decode_instruction(&[0xA9, 0x42]).expect("decode");
        assert_eq!(decoded.mnemonic, "lda");
        assert_eq!(decoded.mode, AddressingMode::ImmediateM);
        assert_eq!(format_instruction(&decoded, 0x1000), "lda #$42");
    }

    #[test]
    fn disassembles_relative_targets() {
        let decoded = decode_instruction(&[0xD0, 0xFC]).expect("decode");
        assert_eq!(format_instruction(&decoded, 0x0010), "bne $00000E");
    }

    #[test]
    fn far_is_rejected_for_nop() {
        let err = select_encoding(
            "nop",
            OperandShape::Address {
                literal: Some(1),
                force_far: true,
                mode: AddressOperandMode::Direct { index: None },
            },
        )
        .expect_err("must fail");
        assert!(matches!(err, EncodeError::NoLongForm { .. }));
    }

    #[test]
    fn selects_jsl_long_opcode() {
        let encoding = select_encoding(
            "jsl",
            OperandShape::Address {
                literal: Some(0x1234),
                force_far: false,
                mode: AddressOperandMode::Direct { index: None },
            },
        )
        .expect("encoding");
        assert_eq!(encoding.opcode, 0x22);
        assert_eq!(encoding.mode, AddressingMode::AbsoluteLong);
    }

    #[test]
    fn selects_lda_abs_x_vs_long_x() {
        let abs = select_encoding(
            "lda",
            OperandShape::Address {
                literal: Some(0x1234),
                force_far: false,
                mode: AddressOperandMode::Direct {
                    index: Some(IndexRegister::X),
                },
            },
        )
        .expect("encoding");
        assert_eq!(abs.opcode, 0xBD);

        let long = select_encoding(
            "lda",
            OperandShape::Address {
                literal: Some(0x12_3456),
                force_far: false,
                mode: AddressOperandMode::Direct {
                    index: Some(IndexRegister::X),
                },
            },
        )
        .expect("encoding");
        assert_eq!(long.opcode, 0xBF);
    }

    #[test]
    fn selects_lda_abs_y() {
        let abs_y = select_encoding(
            "lda",
            OperandShape::Address {
                literal: Some(0x1234),
                force_far: false,
                mode: AddressOperandMode::Direct {
                    index: Some(IndexRegister::Y),
                },
            },
        )
        .expect("encoding");
        assert_eq!(abs_y.opcode, 0xB9);
        assert_eq!(abs_y.mode, AddressingMode::AbsoluteY);
    }

    #[test]
    fn selects_lda_dp_indirect_indexed_modes() {
        let ind_x = select_encoding(
            "lda",
            OperandShape::Address {
                literal: Some(0x20),
                force_far: false,
                mode: AddressOperandMode::IndexedIndirectX,
            },
        )
        .expect("encoding");
        assert_eq!(ind_x.opcode, 0xA1);
        assert_eq!(ind_x.mode, AddressingMode::DirectPageIndexedIndirectX);

        let ind_y = select_encoding(
            "lda",
            OperandShape::Address {
                literal: Some(0x20),
                force_far: false,
                mode: AddressOperandMode::IndirectIndexedY,
            },
        )
        .expect("encoding");
        assert_eq!(ind_y.opcode, 0xB1);
        assert_eq!(ind_y.mode, AddressingMode::DirectPageIndirectIndexedY);
    }

    #[test]
    fn all_opcode_slots_are_populated() {
        assert_eq!(OPCODE_TABLE.len(), 256);
        assert!(OPCODE_TABLE.iter().all(|entry| !entry.mnemonic.is_empty()));
    }
}
