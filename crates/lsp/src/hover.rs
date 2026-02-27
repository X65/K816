use std::collections::BTreeSet;

use super::{INSTRUCTION_DESCRIPTIONS, INSTRUCTION_METADATA, ServerState, SymbolLocation};

pub(super) fn hover_contents_for_symbol(
    canonical: &str,
    symbol: &SymbolLocation,
    state: &ServerState,
) -> String {
    if let Some(doc) = state.documents.get(&symbol.uri) {
        if let Some(meta) = doc.analysis.semantic.functions.get(canonical) {
            let mut lines = vec![
                format!("**function** `{}`", symbol.name),
                format!("- far: {}", yes_no(meta.is_far)),
                format!("- naked: {}", yes_no(meta.is_naked)),
                format!("- inline: {}", yes_no(meta.is_inline)),
            ];
            if let Some(width) = meta.mode_contract.a_width {
                lines.push(format!("- A width: {}", reg_width_name(width)));
            }
            if let Some(width) = meta.mode_contract.i_width {
                lines.push(format!("- I width: {}", reg_width_name(width)));
            }
            if let Some((addr, size)) = doc.address_at_offset(symbol.selection.start) {
                lines.push(format!("- address: `{}`", format_address_range(addr, size)));
            }
            return lines.join("\n");
        }
        if let Some(meta) = doc.analysis.semantic.consts.get(canonical) {
            return hover_contents_for_constant(symbol.name.as_str(), *meta);
        }
        if let Some(meta) = doc.analysis.semantic.vars.get(canonical) {
            return format!(
                "**variable** `{}`\n- address: `{}`\n- size: `{}`",
                symbol.name,
                format_address(meta.address),
                meta.size
            );
        }
    }

    let mut text = format!("**{}** `{}`", symbol.category.detail(), symbol.name);
    if let Some(doc) = state.documents.get(&symbol.uri)
        && let Some((addr, size)) = doc.address_at_offset(symbol.selection.start)
    {
        text.push_str(&format!(
            "\n- address: `{}`",
            format_address_range(addr, size)
        ));
    }
    text
}

fn hover_contents_for_constant(name: &str, value: k816_core::sema::ConstMeta) -> String {
    if let Some(value) = value.value.to_i64_exact() {
        return format!(
            "**constant** `{name}`\n{}",
            format_numeric_value_lines(value, false).join("\n")
        );
    }
    format!("**constant** `{name}`\n- value: `{}`", value.value)
}

pub(super) fn hover_contents_for_numeric_literal(value: i64) -> String {
    format_numeric_value_lines(value, true).join("\n")
}

fn format_numeric_value_lines(value: i64, include_binary_for_u8: bool) -> Vec<String> {
    let mut lines = vec![
        format!("- decimal: `{value}`"),
        format!("- hex: `{}`", format_signed_hex(value)),
    ];
    if include_binary_for_u8 && (0..=255).contains(&value) {
        lines.push(format!("- binary: `%{:08b}`", value as u8));
    }
    lines
}

fn format_signed_hex(value: i64) -> String {
    if value < 0 {
        let magnitude = (-i128::from(value)) as u128;
        return format!("-${magnitude:X}");
    }
    format!("${:X}", value as u64)
}

pub(super) fn format_address(addr: u32) -> String {
    if addr > 0xFFFF {
        format!("${:06X}", addr)
    } else {
        format!("${:04X}", addr)
    }
}

pub(super) fn format_address_range(addr: u32, size: u32) -> String {
    if size <= 1 {
        format_address(addr)
    } else {
        let end = addr + size - 1;
        format!("{}..{}", format_address(addr), format_address(end))
    }
}

fn yes_no(value: bool) -> &'static str {
    if value { "yes" } else { "no" }
}

fn reg_width_name(width: k816_core::ast::RegWidth) -> &'static str {
    match width {
        k816_core::ast::RegWidth::W8 => "8-bit",
        k816_core::ast::RegWidth::W16 => "16-bit",
    }
}

pub(super) fn builtin_hover_text(token: &str) -> Option<String> {
    let token = token.to_ascii_lowercase();
    if opcode_keywords().iter().any(|opcode| opcode == &token) {
        let description = INSTRUCTION_DESCRIPTIONS
            .get(&token.to_ascii_uppercase())
            .map(|d| d.as_str())
            .unwrap_or("WDC 65816 instruction mnemonic.");
        let metadata = INSTRUCTION_METADATA.get(&token.to_ascii_uppercase());
        let cycles = metadata
            .map(|m| m.cycles.trim())
            .filter(|value| !value.is_empty())
            .unwrap_or("not documented");
        let flags = metadata
            .map(|m| {
                if m.flags.is_empty() {
                    "none".to_string()
                } else {
                    m.flags.join(", ")
                }
            })
            .unwrap_or_else(|| "not documented".to_string());
        let addressing_modes = mnemonic_addressing_modes(&token)
            .into_iter()
            .map(format_addressing_mode_name)
            .collect::<Vec<_>>();
        let addressing_summary = if addressing_modes.is_empty() {
            "not documented".to_string()
        } else {
            addressing_modes.join(", ")
        };
        return Some(format!(
            "**opcode** `{token}`\n\n{description}\n\n- flags: `{flags}`\n- cycles: `{cycles}`\n- addressing: {addressing_summary}"
        ));
    }

    let text = match token.as_str() {
        "segment" => "Select an output segment for following code/data.",
        "const" => "Declare a compile-time numeric constant.",
        "var" => "Declare a variable symbol (optionally typed and/or initialized).",
        "func" => "Declare a function block.",
        "far" => "Marks a function for far call/return semantics.",
        "naked" => "Disables automatic function epilogue emission.",
        "inline" => "Marks function as inline-capable in HLA lowering.",
        "data" => "Open a data emission block.",
        "align" => "Align output location to a power-of-two boundary.",
        "address" => "Set absolute output address for following bytes.",
        "nocross" => "Prevent emitted bytes from crossing boundary size.",
        "call" => "Call a known function symbol.",
        "goto" => "Jump to an absolute address or indirect through a vector.",
        "return" => "Return from subroutine (RTS).",
        "return_i" => "Return from interrupt (RTI).",
        "else" => "Alternate branch for conditional blocks.",
        "break" => "Exit the current loop block.",
        "repeat" => "Restart the current loop block or repeat data in a data block.",
        "always" => "Unconditional loop (JMP back to start).",
        "never" => "One-shot block with no branch back.",
        "charset" => "Define character mapping for subsequent string data.",
        "image" => "Load pixel data from bitmap images.",
        "binary" => "Embed raw binary file contents.",
        "code" => "Embed executable code within a data block.",
        "@a8" => "Set accumulator width contract to 8-bit.",
        "@a16" => "Set accumulator width contract to 16-bit.",
        "@i8" => "Set index width contract to 8-bit.",
        "@i16" => "Set index width contract to 16-bit.",
        _ => return None,
    };

    Some(format!("**directive** `{token}`\n{text}"))
}

pub(super) fn opcode_keywords() -> Vec<String> {
    let mut mnemonics = BTreeSet::new();
    for opcode in 0_u16..=255 {
        let descriptor = k816_isa65816::opcode_descriptor(opcode as u8);
        mnemonics.insert(descriptor.mnemonic.to_string());
    }
    mnemonics.into_iter().collect()
}

pub(super) fn register_keywords() -> &'static [(&'static str, &'static str)] {
    &[
        ("a", "Accumulator register A."),
        ("b", "Accumulator high-byte register B (native mode)."),
        ("c", "Combined 16-bit accumulator view C (A:B)."),
        ("x", "Index register X."),
        ("y", "Index register Y."),
        ("d", "Direct page register D."),
        ("s", "Stack pointer register S."),
    ]
}

pub(super) fn is_register_name(name: &str) -> bool {
    register_keywords().iter().any(|(entry, _)| *entry == name)
}

pub(super) fn directive_keywords() -> &'static [&'static str] {
    &[
        "segment", "const", "var", "func", "far", "naked", "inline", "data", "align", "address",
        "nocross", "call", "goto", "return", "return_i", "else", "break", "repeat", "always",
        "never", "charset", "image", "binary", "code", "@a8", "@a16", "@i8", "@i16",
    ]
}

fn mnemonic_addressing_modes(mnemonic: &str) -> Vec<k816_isa65816::AddressingMode> {
    let mut out = Vec::new();
    for opcode in 0_u16..=255 {
        let descriptor = k816_isa65816::opcode_descriptor(opcode as u8);
        if descriptor.mnemonic.eq_ignore_ascii_case(mnemonic) && !out.contains(&descriptor.mode) {
            out.push(descriptor.mode);
        }
    }
    out
}

fn format_addressing_mode_name(mode: k816_isa65816::AddressingMode) -> &'static str {
    match mode {
        k816_isa65816::AddressingMode::Implied => "implied",
        k816_isa65816::AddressingMode::Accumulator => "accumulator",
        k816_isa65816::AddressingMode::Immediate8 => "imm8",
        k816_isa65816::AddressingMode::Immediate16 => "imm16",
        k816_isa65816::AddressingMode::ImmediateM => "immM",
        k816_isa65816::AddressingMode::ImmediateX => "immX",
        k816_isa65816::AddressingMode::DirectPage => "dp",
        k816_isa65816::AddressingMode::DirectPageX => "dp,x",
        k816_isa65816::AddressingMode::DirectPageY => "dp,y",
        k816_isa65816::AddressingMode::DirectPageIndirect => "(dp)",
        k816_isa65816::AddressingMode::DirectPageIndirectLong => "[dp]",
        k816_isa65816::AddressingMode::DirectPageIndexedIndirectX => "(dp,x)",
        k816_isa65816::AddressingMode::DirectPageIndirectIndexedY => "(dp),y",
        k816_isa65816::AddressingMode::DirectPageIndirectLongIndexedY => "[dp],y",
        k816_isa65816::AddressingMode::StackRelative => "sr,s",
        k816_isa65816::AddressingMode::StackRelativeIndirectIndexedY => "(sr,s),y",
        k816_isa65816::AddressingMode::Absolute => "abs",
        k816_isa65816::AddressingMode::AbsoluteX => "abs,x",
        k816_isa65816::AddressingMode::AbsoluteY => "abs,y",
        k816_isa65816::AddressingMode::AbsoluteLong => "long",
        k816_isa65816::AddressingMode::AbsoluteLongX => "long,x",
        k816_isa65816::AddressingMode::AbsoluteIndirect => "(abs)",
        k816_isa65816::AddressingMode::AbsoluteIndexedIndirectX => "(abs,x)",
        k816_isa65816::AddressingMode::AbsoluteIndirectLong => "[abs]",
        k816_isa65816::AddressingMode::Relative8 => "rel8",
        k816_isa65816::AddressingMode::Relative16 => "rel16",
        k816_isa65816::AddressingMode::BlockMove => "block-move",
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct EvaluatorSignature {
    pub(super) label: &'static str,
    pub(super) parameters: &'static [&'static str],
    pub(super) documentation: &'static str,
}

pub(super) fn evaluator_signature(name: &str) -> Option<EvaluatorSignature> {
    match name.to_ascii_lowercase().as_str() {
        "min" => Some(EvaluatorSignature {
            label: "min(lhs, rhs)",
            parameters: &["lhs", "rhs"],
            documentation: "Returns the smaller value.",
        }),
        "max" => Some(EvaluatorSignature {
            label: "max(lhs, rhs)",
            parameters: &["lhs", "rhs"],
            documentation: "Returns the larger value.",
        }),
        "abs" => Some(EvaluatorSignature {
            label: "abs(x)",
            parameters: &["x"],
            documentation: "Absolute value.",
        }),
        "sin" => Some(EvaluatorSignature {
            label: "sin(x)",
            parameters: &["x"],
            documentation: "Sine (radians).",
        }),
        "cos" => Some(EvaluatorSignature {
            label: "cos(x)",
            parameters: &["x"],
            documentation: "Cosine (radians).",
        }),
        "asin" => Some(EvaluatorSignature {
            label: "asin(x)",
            parameters: &["x"],
            documentation: "Arc-sine.",
        }),
        "acos" => Some(EvaluatorSignature {
            label: "acos(x)",
            parameters: &["x"],
            documentation: "Arc-cosine.",
        }),
        "sqrt" => Some(EvaluatorSignature {
            label: "sqrt(x)",
            parameters: &["x"],
            documentation: "Square root.",
        }),
        "pow" => Some(EvaluatorSignature {
            label: "pow(base, exp)",
            parameters: &["base", "exp"],
            documentation: "Power function.",
        }),
        "floor" => Some(EvaluatorSignature {
            label: "floor(x)",
            parameters: &["x"],
            documentation: "Round down.",
        }),
        "ceil" => Some(EvaluatorSignature {
            label: "ceil(x)",
            parameters: &["x"],
            documentation: "Round up.",
        }),
        "round" => Some(EvaluatorSignature {
            label: "round(x)",
            parameters: &["x"],
            documentation: "Round to nearest integer.",
        }),
        "frac" => Some(EvaluatorSignature {
            label: "frac(x)",
            parameters: &["x"],
            documentation: "Fractional component.",
        }),
        "clamp" => Some(EvaluatorSignature {
            label: "clamp(x, min, max)",
            parameters: &["x", "min", "max"],
            documentation: "Clamps x into [min, max].",
        }),
        _ => None,
    }
}
