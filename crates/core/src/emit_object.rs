use indexmap::IndexMap;
use rustc_hash::FxHashMap;

use k816_isa65816::{
    AddressOperandMode as IsaAddressOperandMode, AddressSizeHint as IsaAddressSizeHint,
    AddressingMode, EncodeError, IndexRegister as IsaIndexRegister, MemoryEffect, OperandShape,
    opcode_descriptor, operand_width_for_mode, select_encoding, supported_modes,
};
use k816_o65::{
    CallMetadata, DataStringFragment, FunctionDisassembly, FunctionMetadata, O65Object, Relocation,
    RelocationKind, Section, SectionChunk, SourceLocation, Symbol, SymbolDefinition,
};

use crate::diag::Diagnostic;
use crate::fold_mode::instruction_mode_needs;
use crate::hir::{
    AddressOperandMode, AddressSizeHint, AddressValue, ByteRelocationKind, IndexRegister, Op,
    OperandOp, Program,
};
use crate::sema::{FunctionMeta, SemanticModel};
use crate::span::{SourceMap, Span};

#[derive(Debug, Clone)]
pub struct AddressableSite {
    pub segment: String,
    pub offset: u32,
    pub size: u32,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EmitObjectOutput {
    pub object: O65Object,
    pub addressable_sites: Vec<AddressableSite>,
}

#[derive(Debug)]
struct SegmentState {
    chunks: Vec<SectionChunk>,
    section_offset: u32,
    fixed_cursor: Option<u32>,
    nocross_boundary: Option<u16>,
}

#[derive(Debug)]
struct Fixup {
    segment: String,
    offset: u32,
    width: u8,
    kind: RelocationKind,
    label: String,
    addend: i32,
    span: Span,
    call_metadata: Option<CallMetadata>,
}

#[derive(Debug)]
struct FunctionInstructionSite {
    segment: String,
    function: String,
    offset: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnknownWidthCause {
    Plp,
    Rti,
}

/// English verb describing how an instruction touches its memory operand,
/// for the width-dependence error message
/// (`` `{mnemonic}` {verb} a width-dependent number of bytes... ``).
fn memory_access_verb(effect: MemoryEffect) -> Option<&'static str> {
    match effect {
        MemoryEffect::Load => Some("reads"),
        MemoryEffect::Store => Some("writes"),
        MemoryEffect::Modify => Some("modifies"),
        // Stack ops, block move, and non-memory ops aren't flagged by the
        // width-unknown check — paired-by-convention or fixed-width.
        MemoryEffect::None
        | MemoryEffect::StackPush
        | MemoryEffect::StackPull
        | MemoryEffect::BlockMove => None,
    }
}

fn to_isa_index(index: IndexRegister) -> IsaIndexRegister {
    match index {
        IndexRegister::X => IsaIndexRegister::X,
        IndexRegister::Y => IsaIndexRegister::Y,
        IndexRegister::S => IsaIndexRegister::S,
    }
}

fn to_isa_address_mode(mode: AddressOperandMode) -> IsaAddressOperandMode {
    match mode {
        AddressOperandMode::Direct { index } => IsaAddressOperandMode::Direct {
            index: index.map(to_isa_index),
        },
        AddressOperandMode::Indirect => IsaAddressOperandMode::Indirect,
        AddressOperandMode::IndirectLong => IsaAddressOperandMode::IndirectLong,
        AddressOperandMode::IndexedIndirectX => IsaAddressOperandMode::IndexedIndirectX,
        AddressOperandMode::IndirectIndexedY => IsaAddressOperandMode::IndirectIndexedY,
        AddressOperandMode::IndirectLongIndexedY => IsaAddressOperandMode::IndirectLongIndexedY,
        AddressOperandMode::StackRelativeIndirectIndexedY => {
            IsaAddressOperandMode::StackRelativeIndirectIndexedY
        }
    }
}

fn to_isa_address_size_hint(size_hint: AddressSizeHint) -> IsaAddressSizeHint {
    match size_hint {
        AddressSizeHint::Auto => IsaAddressSizeHint::Auto,
        AddressSizeHint::ForceDirectPage => IsaAddressSizeHint::ForceDirectPage,
        AddressSizeHint::ForceAbsolute16 => IsaAddressSizeHint::ForceAbsolute16,
        AddressSizeHint::ForceAbsoluteLong => IsaAddressSizeHint::ForceAbsoluteLong,
    }
}

fn lookup_function<'a>(
    sema: &'a SemanticModel,
    external: Option<&'a IndexMap<String, FunctionMeta>>,
    mnemonic: &str,
) -> Option<(&'a str, &'a FunctionMeta)> {
    find_in_map(&sema.functions, mnemonic)
        .or_else(|| external.and_then(|m| find_in_map(m, mnemonic)))
}

fn find_in_map<'a>(
    map: &'a IndexMap<String, FunctionMeta>,
    mnemonic: &str,
) -> Option<(&'a str, &'a FunctionMeta)> {
    if let Some((name, meta)) = map.get_key_value(mnemonic) {
        return Some((name.as_str(), meta));
    }
    map.iter()
        .find(|(name, _)| name.eq_ignore_ascii_case(mnemonic))
        .map(|(name, meta)| (name.as_str(), meta))
}

fn function_call_help(name: &str, meta: &FunctionMeta) -> String {
    format!(
        "`{name}` is {kind}function; call it as `{sig}`",
        kind = meta.kind_prefix_with_article(),
        sig = meta.signature_call_form(name),
    )
}

/// If the user gave a bare label as the operand to a strictly-immediate mnemonic
/// (e.g. `pea label`), explain that an immediate is required and — for the
/// 16-bit `Immediate16` case — suggest the address-of form `pea #&&label`.
///
/// Rule: triggers only when every accepted addressing mode for the mnemonic is
/// some flavor of immediate. Mnemonics that also accept Direct addressing
/// (`lda`, `ldx`, …) take the bare label through the address path and never
/// reach `InvalidOperand`, so they are filtered out automatically.
fn invalid_operand_help(mnemonic: &str, operand: &Option<OperandOp>) -> Option<String> {
    let label = match operand {
        Some(OperandOp::Address {
            value: AddressValue::Label(name),
            mode: AddressOperandMode::Direct { index: None },
            ..
        }) => name.as_str(),
        Some(OperandOp::Address {
            value: AddressValue::LabelOffset { label, .. },
            mode: AddressOperandMode::Direct { index: None },
            ..
        }) => label.as_str(),
        _ => return None,
    };

    let modes = supported_modes(mnemonic);
    let all_immediate = !modes.is_empty()
        && modes.iter().all(|m| {
            matches!(
                m,
                AddressingMode::Immediate8
                    | AddressingMode::Immediate16
                    | AddressingMode::ImmediateM
                    | AddressingMode::ImmediateX
            )
        });
    if !all_immediate {
        return None;
    }

    if modes.contains(&AddressingMode::Immediate16) {
        Some(format!(
            "`{mnemonic}` requires an immediate operand; use `{mnemonic} #&&{label}` to pass the 16-bit address of `{label}`"
        ))
    } else {
        Some(format!(
            "`{mnemonic}` requires an immediate operand; pass a `#`-prefixed constant value (e.g. `{mnemonic} #$00`)"
        ))
    }
}

/// Short user-facing label for the operand the user actually wrote, used as
/// the primary span label when an `InvalidOperand` falls through to the
/// generic enrichment path.
fn operand_kind_label(operand: &Option<OperandOp>) -> &'static str {
    match operand {
        None => "missing operand",
        Some(OperandOp::Immediate(_))
        | Some(OperandOp::ImmediateByteRelocation { .. })
        | Some(OperandOp::ImmediateWordRelocation { .. })
        | Some(OperandOp::ImmediateFarRelocation { .. }) => "immediate operand",
        Some(OperandOp::Address {
            mode: AddressOperandMode::Direct { index: None },
            ..
        }) => "address operand",
        Some(OperandOp::Address {
            mode: AddressOperandMode::Direct { index: Some(_) },
            ..
        }) => "indexed address operand",
        Some(OperandOp::Address {
            mode: AddressOperandMode::Indirect,
            ..
        }) => "indirect operand",
        Some(OperandOp::Address {
            mode: AddressOperandMode::IndexedIndirectX,
            ..
        }) => "indexed-indirect operand",
        Some(OperandOp::Address {
            mode: AddressOperandMode::IndirectIndexedY,
            ..
        }) => "indirect-indexed operand",
        Some(OperandOp::Address {
            mode: AddressOperandMode::IndirectLong,
            ..
        }) => "long-indirect operand",
        Some(OperandOp::Address {
            mode: AddressOperandMode::IndirectLongIndexedY,
            ..
        }) => "long-indirect-indexed operand",
        Some(OperandOp::Address {
            mode: AddressOperandMode::StackRelativeIndirectIndexedY,
            ..
        }) => "stack-relative-indirect operand",
        Some(OperandOp::BlockMove { .. }) => "block-move operand",
    }
}

/// Render the addressing-mode set returned by `supported_modes(mnemonic)` as a
/// short, deduplicated, user-readable list with concrete syntax examples in
/// terms of the *actual* mnemonic. Returns `None` when the mode set is empty
/// (which would be an internal bug, since `EncodeError::UnknownMnemonic` is
/// reported separately).
fn describe_supported_modes(mnemonic: &str, modes: &[AddressingMode]) -> Option<String> {
    if modes.is_empty() {
        return None;
    }
    let mut families: Vec<String> = Vec::new();
    for m in modes {
        let family = mode_family_description(mnemonic, *m);
        if !families.contains(&family) {
            families.push(family);
        }
    }
    Some(match families.as_slice() {
        [] => return None,
        [single] => single.clone(),
        [a, b] => format!("{a} or {b}"),
        rest => {
            let head = rest[..rest.len() - 1].join(", ");
            let tail = &rest[rest.len() - 1];
            format!("{head}, or {tail}")
        }
    })
}

fn mode_family_description(m: &str, mode: AddressingMode) -> String {
    use AddressingMode::*;
    match mode {
        Implied => format!("no operand (implied form, just `{m}`)"),
        Accumulator => format!("the accumulator (`{m} A`)"),
        Immediate8 => format!("an 8-bit immediate (`{m} #$NN`)"),
        Immediate16 => format!("a 16-bit immediate (`{m} #$NNNN`)"),
        ImmediateM => format!(
            "an M-width immediate (`{m} #$NN` under `@a8`, `{m} #$NNNN` under `@a16`)"
        ),
        ImmediateX => format!(
            "an X-width immediate (`{m} #$NN` under `@i8`, `{m} #$NNNN` under `@i16`)"
        ),
        DirectPage | DirectPageX | DirectPageY => {
            format!("a direct-page address (`{m} addr`, `{m} addr,X`, or `{m} addr,Y`)")
        }
        DirectPageIndirect | DirectPageIndirectLong => {
            format!("an indirect address (`{m} (addr)` or `{m} [addr]`)")
        }
        DirectPageIndexedIndirectX => format!("an indexed-indirect address (`{m} (addr,X)`)"),
        DirectPageIndirectIndexedY | DirectPageIndirectLongIndexedY => {
            format!("an indirect-indexed address (`{m} (addr),Y` or `{m} [addr],Y`)")
        }
        Absolute | AbsoluteX | AbsoluteY => {
            format!("an absolute address (`{m} addr`, `{m} addr,X`, or `{m} addr,Y`)")
        }
        AbsoluteLong | AbsoluteLongX => {
            format!("an absolute-long address (`{m} addr` or `{m} addr,X`)")
        }
        AbsoluteIndirect | AbsoluteIndirectLong | AbsoluteIndexedIndirectX => {
            format!(
                "an absolute-indirect address (`{m} (addr)`, `{m} [addr]`, or `{m} (addr,X)`)"
            )
        }
        StackRelative | StackRelativeIndirectIndexedY => {
            format!("a stack-relative offset (`{m} offset,S` or `{m} (offset,S),Y`)")
        }
        Relative8 | Relative16 => {
            format!("a PC-relative branch target (`{m} label`)")
        }
        BlockMove => format!("a block-move pair (`{m} #srcbank, #dstbank`)"),
    }
}

/// Architectural note for the W65C816 software-interrupt mnemonics. `brk`,
/// `cop`, and `wdm` each consume the byte after the opcode as a "signature"
/// the trap handler can read, so the encoder always requires an `Immediate8`.
fn software_interrupt_note(mnemonic: &str) -> Option<&'static str> {
    match mnemonic.to_ascii_lowercase().as_str() {
        "brk" => Some(
            "On the W65C816, `brk` is a software interrupt: the byte immediately after the opcode is a signature byte that the BRK vector handler reads via the pushed PC to dispatch traps. The K816 encoder always requires it (write `brk #$00` if you have nothing meaningful to put there).",
        ),
        "cop" => Some(
            "On the W65C816, `cop` is the coprocessor / OS-trap software interrupt: the byte immediately after the opcode is a signature byte that the COP vector handler reads to dispatch the trap. The K816 encoder always requires it (write `cop #$00` if you have nothing meaningful to put there).",
        ),
        "wdm" => Some(
            "On the W65C816, `wdm` is reserved for future expansion but is encoded today as a 2-byte sequence: opcode + a signature byte. The K816 encoder requires the signature byte even though current silicon ignores it.",
        ),
        _ => None,
    }
}

/// True when `name` resolves to a function, var, const, symbolic-subscript
/// field, or cross-unit external var classification visible to this
/// translation unit. Mirrors the lookup at `lower.rs:1849`. Used to detect
/// unresolved labels at encode-error time so help text can distinguish
/// "symbol is genuinely unknown" from "symbol is declared somewhere in the
/// link group but the encoding it asks for can't be satisfied".
fn is_locally_resolvable(sema: &SemanticModel, name: &str) -> bool {
    if sema.functions.contains_key(name)
        || sema.vars.contains_key(name)
        || sema.consts.contains_key(name)
        || sema.external_var_classes.contains_key(name)
    {
        return true;
    }
    if let Some((base, _field)) = name.split_once('.')
        && (sema.vars.contains_key(base) || sema.external_var_classes.contains_key(base))
    {
        return true;
    }
    false
}

/// Build a `Diagnostic` for an `EncodeError`. Centralizes the per-variant
/// enrichment that decorates the bare error message with help/labels/notes
/// drawn from `instruction.operand`, `sema`, and `supported_modes`.
fn build_encode_diagnostic(
    span: Span,
    instruction_mnemonic: &str,
    operand: &Option<OperandOp>,
    err: &EncodeError,
    sema: &SemanticModel,
    external_functions: Option<&IndexMap<String, FunctionMeta>>,
) -> Diagnostic {
    let mut diag = Diagnostic::error(span, err.to_string());
    match err {
        EncodeError::UnknownMnemonic { mnemonic } => {
            if let Some((name, meta)) = lookup_function(sema, external_functions, mnemonic) {
                diag = diag.with_help(function_call_help(name, meta));
            }
        }
        EncodeError::InvalidOperand { mnemonic } => {
            if let Some(help) = invalid_operand_help(mnemonic, operand) {
                diag = diag
                    .with_primary_label(operand_kind_label(operand))
                    .with_help(help);
                if let Some(note) = software_interrupt_note(mnemonic) {
                    diag = diag.with_note(note);
                }
            } else if let Some(enriched) = enrich_invalid_indirect(span, mnemonic, operand, sema) {
                diag = enriched;
            } else {
                let kind = operand_kind_label(operand);
                let modes = supported_modes(mnemonic);
                diag = diag.with_primary_label(kind);
                if let Some(accepted) = describe_supported_modes(mnemonic, &modes) {
                    let lead = if operand.is_none() {
                        format!("`{mnemonic}` requires an operand: it accepts {accepted}")
                    } else {
                        format!(
                            "`{mnemonic}` accepts {accepted}; the operand you wrote does not match any of those forms"
                        )
                    };
                    diag = diag.with_help(lead);
                }
                if let Some(note) = software_interrupt_note(mnemonic) {
                    diag = diag.with_note(note);
                }
            }
        }
        EncodeError::NoDirectPageForm => {
            diag = Diagnostic::error(
                span,
                format!(
                    "`{instruction_mnemonic}` has no direct-page form for this operand",
                ),
            )
            .with_primary_label("forced direct-page operand".to_string())
            .with_help(format!(
                "drop the `dp` prefix; `{instruction_mnemonic}` only accepts absolute or long addressing here"
            ));
        }
        EncodeError::InvalidImmediateOperand { mnemonic } => {
            let mnemonic_lower = mnemonic.to_ascii_lowercase();
            let stores = matches!(mnemonic_lower.as_str(), "sta" | "stx" | "sty" | "stz");
            let help = if stores {
                format!(
                    "`{mnemonic}` writes a register to memory; drop the `#` and pass an address (`{mnemonic} addr`), or load the value into a register first (`a = #$xx`) and then store it"
                )
            } else {
                format!(
                    "`{mnemonic}` operates on memory or an address — remove the `#` and pass an address operand instead, or pick a mnemonic that does take an immediate"
                )
            };
            let note = if stores {
                "Store mnemonics on the W65C816 (`sta`/`stx`/`sty`/`stz`) only address memory; immediates have no destination, so the assembler refuses the `#` form."
            } else {
                "Only a subset of W65C816 mnemonics accept the `#imm` form (notably `lda`/`ldx`/`ldy`/`cmp`/`cpx`/`cpy`/`adc`/`sbc`/`and`/`ora`/`eor`/`bit`); everything else expects an address operand."
            };
            diag = Diagnostic::error(span, err.to_string())
                .with_primary_label("immediate operand")
                .with_help(help)
                .with_note(note);
        }
        EncodeError::NoAbsolute16Form => {
            // For indirect-family operands, this fires when an `abs`-classified
            // var (typically declared in a sibling file as `var abs X:word`)
            // is used in a mode (`(X)`, `(X,X)`, `[X]`, …) that only has a
            // direct-page encoding on this mnemonic. Surface the DP
            // requirement and point at the symbol — the generic
            // "no absolute form" message hides the actual cause.
            if let Some(enriched) =
                enrich_invalid_indirect(span, instruction_mnemonic, operand, sema)
            {
                diag = enriched;
            } else {
                diag = Diagnostic::error(
                    span,
                    format!(
                        "`{instruction_mnemonic}` has no absolute form for this operand",
                    ),
                )
                .with_primary_label("forced absolute operand".to_string())
                .with_help(format!(
                    "drop the `abs` prefix; `{instruction_mnemonic}` only accepts direct-page or long addressing here"
                ));
            }
        }
        EncodeError::DirectPageOutOfRange => {
            let value_phrase = match operand {
                Some(OperandOp::Address {
                    value: AddressValue::Literal(v),
                    ..
                }) => Some(format!("value ${v:X}")),
                Some(OperandOp::Address {
                    value: AddressValue::LabelOffset { label, addend },
                    ..
                }) => Some(format!("`{label}+{addend}`")),
                Some(OperandOp::Address {
                    value: AddressValue::Label(name),
                    ..
                }) => Some(format!("`{name}`")),
                _ => None,
            };
            let primary = match &value_phrase {
                Some(p) => format!(
                    "forced direct-page {p} is out of range for `{instruction_mnemonic}` (must fit in 0x00..=0xFF)",
                ),
                None => format!(
                    "forced direct-page operand for `{instruction_mnemonic}` is out of range (must fit in 0x00..=0xFF)"
                ),
            };
            diag = Diagnostic::error(span, primary)
                .with_primary_label("forced direct-page operand".to_string())
                .with_help(format!(
                    "drop the `dp` prefix to let `{instruction_mnemonic}` use absolute addressing, or place the symbol at a zero-page address"
                ));
        }
        _ => {}
    }
    diag
}

/// For an `InvalidOperand` error on an indirect-family operand (`(addr)`,
/// `(addr,X)`, `(addr),Y`, `[addr]`, `[addr],Y`), build a diagnostic that
/// names the symbol, explains the direct-page constraint, and points at the
/// fix. Returns `None` for non-indirect operands so callers can fall through
/// to the bare-error path.
fn enrich_invalid_indirect(
    span: Span,
    mnemonic: &str,
    operand: &Option<OperandOp>,
    sema: &SemanticModel,
) -> Option<Diagnostic> {
    let Some(OperandOp::Address { value, mode, .. }) = operand else {
        return None;
    };
    let (mode_syntax, label_phrase) = match mode {
        AddressOperandMode::Indirect => ("(addr)", "indirect operand"),
        AddressOperandMode::IndexedIndirectX => ("(addr,X)", "indexed-indirect operand"),
        AddressOperandMode::IndirectIndexedY => ("(addr),Y", "indirect-indexed operand"),
        AddressOperandMode::IndirectLong => ("[addr]", "long-indirect operand"),
        AddressOperandMode::IndirectLongIndexedY => {
            ("[addr],Y", "long-indirect-indexed operand")
        }
        _ => return None,
    };

    let primary_msg = format!(
        "`{mnemonic} {mode_syntax}` requires the address to lie within the direct page",
    );
    let inner_syntax = match mode {
        AddressOperandMode::Indirect => "(dp X)",
        AddressOperandMode::IndexedIndirectX => "(dp X,X)",
        AddressOperandMode::IndirectIndexedY => "(dp X),Y",
        AddressOperandMode::IndirectLong => "[dp X]",
        AddressOperandMode::IndirectLongIndexedY => "[dp X],Y",
        _ => unreachable!(),
    };
    let help = match value {
        AddressValue::Label(name) | AddressValue::LabelOffset { label: name, .. } => {
            if is_locally_resolvable(sema, name) {
                format!(
                    "use the `dp` prefix to force direct-page indirect (write `{}`), or declare `{name}` at a direct-page address",
                    inner_syntax.replace('X', name),
                )
            } else {
                format!(
                    "symbol `{name}` is not declared in this file; declare it as a zero-page `var` (e.g. `var {name} = $80`), or use the `dp` prefix to force direct-page indirect (`{}`)",
                    inner_syntax.replace('X', name),
                )
            }
        }
        AddressValue::Literal(value) => format!(
            "value ${value:X} exceeds the direct-page range (0x00..=0xFF); only direct-page indirect forms are encodable for `{mnemonic}`"
        ),
    };
    let mut diag = Diagnostic::error(span, primary_msg)
        .with_primary_label(label_phrase.to_string())
        .with_help(help);
    if matches!(mode, AddressOperandMode::Indirect) && mnemonic.eq_ignore_ascii_case("lda") {
        diag = diag.with_note(
            "`lda` has no `(abs)` form on the W65C816 — only `jmp`, `jsl`, and `jsr` accept absolute-indirect addressing".to_string(),
        );
    }
    Some(diag)
}

/// If `origin` resolves to a known source, append an `InlineOrigin` supplement
/// to `diag` so renderers can show "(Inlined from …)" pointing to the original
/// (foreign-source) location of an inlined op. Idempotent: skips if `diag`
/// already carries an `InlineOrigin` supplement.
fn attach_inline_origin(diag: &mut Diagnostic, origin: Span, source_map: &SourceMap) {
    use crate::diag::Supplemental;
    if diag
        .supplements
        .iter()
        .any(|s| matches!(s, Supplemental::InlineOrigin { .. }))
    {
        return;
    }
    let Some(file) = source_map.get(origin.source_id) else {
        return;
    };
    let (line, _col) = file.line_col(origin.start);
    let label = format!("Inlined from {}:{}", file.name, line);
    diag.supplements.push(Supplemental::InlineOrigin {
        span: origin,
        label,
    });
}

pub fn emit_object(
    program: &Program,
    source_map: &SourceMap,
    sema: &SemanticModel,
    external_functions: Option<&IndexMap<String, FunctionMeta>>,
) -> Result<EmitObjectOutput, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut segments: IndexMap<String, SegmentState> = IndexMap::new();
    let mut labels: FxHashMap<String, (String, u32, Span)> = FxHashMap::default();
    let mut absolute_symbols: FxHashMap<String, (u32, Span)> = FxHashMap::default();
    let mut fixups = Vec::new();
    let mut current_segment = crate::DEFAULT_SEGMENT.to_string();
    let mut current_function: Option<String> = None;
    let mut function_instruction_sites = Vec::new();
    let mut function_initial_modes: FxHashMap<(String, String), (bool, bool)> =
        FxHashMap::default();
    let mut data_string_fragments = Vec::new();
    let mut addressable_sites: Vec<AddressableSite> = Vec::new();
    let mut function_label_metadata: FxHashMap<String, FunctionMetadata> = FxHashMap::default();
    let mut m_wide: Option<bool> = None; // accumulator width: None=unknown, Some(true)=16-bit, Some(false)=8-bit
    let mut x_wide: Option<bool> = None; // index width: None=unknown, Some(true)=16-bit, Some(false)=8-bit
    let mut m_unknown_cause: Option<UnknownWidthCause> = None;
    let mut x_unknown_cause: Option<UnknownWidthCause> = None;
    // Function-level commitment to a width (explicit @a/@i contract OR entry-point default).
    // Used by the memory-access width-dependence check to trust the programmer's intent
    // even after a PLP/RTI scrambles runtime tracking.
    let mut m_wide_contract: Option<bool> = None;
    let mut x_wide_contract: Option<bool> = None;

    segments
        .entry(current_segment.clone())
        .or_insert(SegmentState {
            chunks: Vec::new(),
            section_offset: 0,
            fixed_cursor: None,
            nocross_boundary: None,
        });

    for op in &program.ops {
        match &op.node {
            Op::SelectSegment(name) => {
                current_segment = name.clone();
                segments
                    .entry(current_segment.clone())
                    .or_insert(SegmentState {
                        chunks: Vec::new(),
                        section_offset: 0,
                        fixed_cursor: None,
                        nocross_boundary: None,
                    });
            }
            Op::FunctionStart {
                name,
                mode_contract,
                is_entry,
                is_far,
            } => {
                current_function = Some(name.clone());
                // Entry point (main) defaults to 8-bit (emulation mode).
                // Functions require explicit mode contract for width-dependent ops.
                let default = if *is_entry { Some(false) } else { None };
                m_wide = mode_contract
                    .a_width
                    .map(|w| w == crate::ast::RegWidth::W16)
                    .or(default);
                x_wide = mode_contract
                    .i_width
                    .map(|w| w == crate::ast::RegWidth::W16)
                    .or(default);
                m_wide_contract = m_wide;
                x_wide_contract = x_wide;
                m_unknown_cause = None;
                x_unknown_cause = None;
                function_initial_modes.insert(
                    (current_segment.clone(), name.clone()),
                    (m_wide.unwrap_or(false), x_wide.unwrap_or(false)),
                );
                function_label_metadata.insert(
                    name.clone(),
                    FunctionMetadata {
                        is_far: *is_far,
                        a_width: mode_contract
                            .a_width
                            .map(|w| w == crate::ast::RegWidth::W16),
                        i_width: mode_contract
                            .i_width
                            .map(|w| w == crate::ast::RegWidth::W16),
                    },
                );
            }
            Op::FunctionEnd => {
                current_function = None;
                m_wide_contract = None;
                x_wide_contract = None;
                m_unknown_cause = None;
                x_unknown_cause = None;
            }
            Op::Label(name) => {
                let segment = segments
                    .get(&current_segment)
                    .expect("current segment must exist during emit");
                addressable_sites.push(AddressableSite {
                    segment: current_segment.clone(),
                    offset: segment.section_offset,
                    size: 0,
                    span: op.span,
                });
                if labels
                    .insert(
                        name.clone(),
                        (current_segment.clone(), segment.section_offset, op.span),
                    )
                    .is_some()
                    || absolute_symbols.contains_key(name)
                {
                    diagnostics.push(
                        Diagnostic::error(op.span, format!("duplicate label '{name}'"))
                            .with_help("rename one of the labels"),
                    );
                }
            }
            Op::DefineAbsoluteSymbol { name, address } => {
                if labels.contains_key(name)
                    || absolute_symbols
                        .insert(name.clone(), (*address, op.span))
                        .is_some()
                {
                    diagnostics.push(
                        Diagnostic::error(op.span, format!("duplicate symbol '{name}'"))
                            .with_help("rename one of the symbols"),
                    );
                }
            }
            Op::SetMode(mode_contract) => {
                m_wide = mode_contract
                    .a_width
                    .map(|w| w == crate::ast::RegWidth::W16);
                x_wide = mode_contract
                    .i_width
                    .map(|w| w == crate::ast::RegWidth::W16);
                m_unknown_cause = None;
                x_unknown_cause = None;
            }
            Op::Align { boundary, offset } => {
                if *boundary == 0 {
                    diagnostics.push(Diagnostic::error(op.span, "align value must be non-zero"));
                    continue;
                }

                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");

                let cursor = active_cursor(segment);
                let align = u32::from(*boundary);
                let offset = u32::from(*offset);
                let rem = (cursor.wrapping_sub(offset)) % align;
                let pad = if rem == 0 { 0 } else { align - rem };

                if pad > 0 {
                    emit_zeroes(segment, pad as usize, op.span, &mut diagnostics);
                }
            }
            Op::Address(address) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                segment.fixed_cursor = Some(*address);
            }
            Op::Nocross(boundary) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                segment.nocross_boundary = Some(*boundary);
            }
            Op::Rep { mask, .. } => {
                if mask & 0x20 != 0 {
                    m_wide = Some(true);
                    m_unknown_cause = None;
                }
                if mask & 0x10 != 0 {
                    x_wide = Some(true);
                    x_unknown_cause = None;
                }
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                let opcode_offset = segment.section_offset;
                addressable_sites.push(AddressableSite {
                    segment: current_segment.clone(),
                    offset: opcode_offset,
                    size: 2,
                    span: op.span,
                });
                if let Some(function) = &current_function {
                    function_instruction_sites.push(FunctionInstructionSite {
                        segment: current_segment.clone(),
                        function: function.clone(),
                        offset: opcode_offset,
                    });
                }
                let bytes = [0xC2, *mask];
                append_bytes(segment, &bytes, op.span, &mut diagnostics);
            }
            Op::Sep { mask, .. } => {
                if mask & 0x20 != 0 {
                    m_wide = Some(false);
                    m_unknown_cause = None;
                }
                if mask & 0x10 != 0 {
                    x_wide = Some(false);
                    x_unknown_cause = None;
                }
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                let opcode_offset = segment.section_offset;
                addressable_sites.push(AddressableSite {
                    segment: current_segment.clone(),
                    offset: opcode_offset,
                    size: 2,
                    span: op.span,
                });
                if let Some(function) = &current_function {
                    function_instruction_sites.push(FunctionInstructionSite {
                        segment: current_segment.clone(),
                        function: function.clone(),
                        offset: opcode_offset,
                    });
                }
                let bytes = [0xE2, *mask];
                append_bytes(segment, &bytes, op.span, &mut diagnostics);
            }
            Op::EmitBytes(bytes) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");

                apply_nocross_if_needed(segment, bytes.len(), op.span, &mut diagnostics);
                let emit_offset = segment.section_offset;
                addressable_sites.push(AddressableSite {
                    segment: current_segment.clone(),
                    offset: emit_offset,
                    size: bytes.len() as u32,
                    span: op.span,
                });
                if let Some(text) = string_literal_text_for_emit(bytes, source_map, op.span) {
                    data_string_fragments.push(DataStringFragment {
                        section: current_segment.clone(),
                        offset: emit_offset,
                        text,
                    });
                }
                append_bytes(segment, bytes, op.span, &mut diagnostics);
            }
            Op::EmitRelocBytes { bytes, relocations } => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");

                apply_nocross_if_needed(segment, bytes.len(), op.span, &mut diagnostics);
                let emit_offset = segment.section_offset;
                addressable_sites.push(AddressableSite {
                    segment: current_segment.clone(),
                    offset: emit_offset,
                    size: bytes.len() as u32,
                    span: op.span,
                });
                append_bytes(segment, bytes, op.span, &mut diagnostics);
                for relocation in relocations {
                    if relocation.offset >= bytes.len() as u32 {
                        diagnostics.push(Diagnostic::error(
                            op.span,
                            format!(
                                "byte relocation offset {} is outside emitted byte range",
                                relocation.offset
                            ),
                        ));
                        continue;
                    }
                    let relocation_span =
                        relocation_span_for_label(source_map, op.span, &relocation.label);
                    let (width, kind) = match relocation.kind {
                        ByteRelocationKind::LowByte => (1, RelocationKind::LowByte),
                        ByteRelocationKind::HighByte => (1, RelocationKind::HighByte),
                        ByteRelocationKind::FullWord => (2, RelocationKind::Absolute),
                        ByteRelocationKind::FullLong => (3, RelocationKind::Absolute),
                    };
                    fixups.push(Fixup {
                        segment: current_segment.clone(),
                        offset: emit_offset + relocation.offset,
                        width,
                        kind,
                        label: relocation.label.clone(),
                        addend: 0,
                        span: relocation_span,
                        call_metadata: None,
                    });
                }
            }
            Op::Instruction(instruction) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                let mnemonic = instruction.mnemonic.to_ascii_lowercase();

                let operand_shape = match &instruction.operand {
                    None => OperandShape::None,
                    // Width-dependent immediates are sized after mode tracking, so
                    // encoding selection should not depend on the literal magnitude here.
                    Some(OperandOp::Immediate(_)) => OperandShape::Immediate(0),
                    Some(OperandOp::ImmediateByteRelocation { .. }) => OperandShape::Immediate(0),
                    Some(OperandOp::ImmediateWordRelocation { .. }) => OperandShape::Immediate(0),
                    Some(OperandOp::ImmediateFarRelocation { .. }) => OperandShape::Immediate(0),
                    Some(OperandOp::Address {
                        value,
                        size_hint,
                        mode,
                    }) => match value {
                        AddressValue::Literal(literal) => OperandShape::Address {
                            literal: Some(*literal),
                            size_hint: to_isa_address_size_hint(*size_hint),
                            mode: to_isa_address_mode(*mode),
                        },
                        AddressValue::Label(_) | AddressValue::LabelOffset { .. } => {
                            OperandShape::Address {
                                literal: None,
                                size_hint: to_isa_address_size_hint(*size_hint),
                                mode: to_isa_address_mode(*mode),
                            }
                        }
                    },
                    Some(OperandOp::BlockMove { src, dst }) => OperandShape::BlockMove(*src, *dst),
                };

                let encoding = match select_encoding(&instruction.mnemonic, operand_shape) {
                    Ok(encoding) => encoding,
                    Err(err) => {
                        diagnostics.push(build_encode_diagnostic(
                            op.span,
                            &instruction.mnemonic,
                            &instruction.operand,
                            &err,
                            sema,
                            external_functions,
                        ));
                        continue;
                    }
                };

                if encoding.mode == AddressingMode::ImmediateM && m_wide.is_none() {
                    let help = match m_unknown_cause {
                        Some(UnknownWidthCause::Plp) => {
                            "PLP restores processor flags from runtime stack; add @a8 or @a16 to re-establish accumulator width"
                        }
                        Some(UnknownWidthCause::Rti) => {
                            "RTI restores processor flags from runtime state; add @a8 or @a16 to re-establish accumulator width"
                        }
                        None => "add @a8 or @a16 to the enclosing function",
                    };
                    diagnostics.push(
                        Diagnostic::error(
                            op.span,
                            format!(
                                "`{}` uses a width-dependent immediate but accumulator width is unknown",
                                instruction.mnemonic,
                            ),
                        )
                        .with_help(help),
                    );
                    continue;
                }
                if encoding.mode == AddressingMode::ImmediateX && x_wide.is_none() {
                    let help = match x_unknown_cause {
                        Some(UnknownWidthCause::Plp) => {
                            "PLP restores processor flags from runtime stack; add @i8 or @i16 to re-establish index width"
                        }
                        Some(UnknownWidthCause::Rti) => {
                            "RTI restores processor flags from runtime state; add @i8 or @i16 to re-establish index width"
                        }
                        None => "add @i8 or @i16 to the enclosing function",
                    };
                    diagnostics.push(
                        Diagnostic::error(
                            op.span,
                            format!(
                                "`{}` uses a width-dependent immediate but index width is unknown",
                                instruction.mnemonic,
                            ),
                        )
                        .with_help(help),
                    );
                    continue;
                }

                let descriptor = opcode_descriptor(encoding.opcode);
                let (needs_m, needs_x) = instruction_mode_needs(instruction);

                if let Some(verb) = memory_access_verb(descriptor.memory_effect) {
                    if needs_m && m_wide.is_none() && m_wide_contract.is_none() {
                        let help = match m_unknown_cause {
                            Some(UnknownWidthCause::Plp) => {
                                "PLP restores processor flags from runtime stack; add @a8 or @a16 to re-establish accumulator width"
                            }
                            Some(UnknownWidthCause::Rti) => {
                                "RTI restores processor flags from runtime state; add @a8 or @a16 to re-establish accumulator width"
                            }
                            None => "add @a8 or @a16 to the enclosing function",
                        };
                        diagnostics.push(
                            Diagnostic::error(
                                op.span,
                                format!(
                                    "`{}` {} a width-dependent number of bytes but accumulator width is unknown",
                                    instruction.mnemonic, verb,
                                ),
                            )
                            .with_help(help),
                        );
                        continue;
                    }
                    if needs_x && x_wide.is_none() && x_wide_contract.is_none() {
                        let help = match x_unknown_cause {
                            Some(UnknownWidthCause::Plp) => {
                                "PLP restores processor flags from runtime stack; add @i8 or @i16 to re-establish index width"
                            }
                            Some(UnknownWidthCause::Rti) => {
                                "RTI restores processor flags from runtime state; add @i8 or @i16 to re-establish index width"
                            }
                            None => "add @i8 or @i16 to the enclosing function",
                        };
                        diagnostics.push(
                            Diagnostic::error(
                                op.span,
                                format!(
                                    "`{}` {} a width-dependent number of bytes but index width is unknown",
                                    instruction.mnemonic, verb,
                                ),
                            )
                            .with_help(help),
                        );
                        continue;
                    }
                } else if needs_m && needs_x {
                    // Cross-bank transfer (tax/tay/txa/tya): error only when
                    // both widths are *known* and *different*. If either is
                    // unknown the programmer is presumed to know what they're
                    // doing.
                    if let (Some(m), Some(x)) = (m_wide, x_wide)
                        && m != x
                    {
                        let (a, i) = (if m { 16 } else { 8 }, if x { 16 } else { 8 });
                        diagnostics.push(
                            Diagnostic::error(
                                op.span,
                                format!(
                                    "`{}` requires accumulator and index widths to match (a={a}, i={i})",
                                    instruction.mnemonic,
                                ),
                            )
                            .with_help(
                                "use @a8 @i8 or @a16 @i16 around this transfer, or omit one width to defer the choice",
                            ),
                        );
                        continue;
                    }
                }

                let width = operand_width_for_mode(
                    encoding.mode,
                    m_wide.unwrap_or(false),
                    x_wide.unwrap_or(false),
                );
                apply_nocross_if_needed(segment, 1 + width, op.span, &mut diagnostics);

                let opcode_offset = segment.section_offset;
                addressable_sites.push(AddressableSite {
                    segment: current_segment.clone(),
                    offset: opcode_offset,
                    size: 1 + width as u32,
                    span: op.span,
                });
                append_bytes(segment, &[encoding.opcode], op.span, &mut diagnostics);
                let operand_offset = segment.section_offset;

                if let Some(function) = &current_function {
                    function_instruction_sites.push(FunctionInstructionSite {
                        segment: current_segment.clone(),
                        function: function.clone(),
                        offset: opcode_offset,
                    });
                }

                match &instruction.operand {
                    None => {}
                    Some(OperandOp::Immediate(value)) => {
                        // Positive values flow through emit_literal's existing
                        // unsigned range check. Negative values that fit the
                        // operand's signed range wrap as two's complement to
                        // the operand width (per docs/syntax-reference.md).
                        let v = *value;
                        if v >= 0 {
                            match u32::try_from(v) {
                                Ok(raw) => {
                                    emit_literal(segment, raw, width, op.span, &mut diagnostics)
                                }
                                Err(_) => diagnostics.push(Diagnostic::error(
                                    op.span,
                                    "immediate value out of range",
                                )),
                            }
                        } else {
                            let (min, mask): (i64, u32) = match width {
                                1 => (-(1i64 << 7), 0xFF),
                                2 => (-(1i64 << 15), 0xFFFF),
                                3 => (-(1i64 << 23), 0xFF_FFFF),
                                _ => (0, 0),
                            };
                            if v >= min {
                                let raw = (v as u32) & mask;
                                emit_literal(segment, raw, width, op.span, &mut diagnostics);
                            } else {
                                diagnostics.push(Diagnostic::error(
                                    op.span,
                                    "immediate value cannot be negative",
                                ));
                            }
                        }
                    }
                    Some(OperandOp::ImmediateByteRelocation { kind, label }) => {
                        if width == 0 {
                            diagnostics.push(Diagnostic::error(
                                op.span,
                                "immediate byte relocation requires a non-zero operand width",
                            ));
                            continue;
                        }
                        emit_zeroes(segment, width, op.span, &mut diagnostics);
                        let relocation_span = relocation_span_for_label(source_map, op.span, label);
                        let (reloc_width, reloc_kind) = match kind {
                            ByteRelocationKind::LowByte => (1, RelocationKind::LowByte),
                            ByteRelocationKind::HighByte => (1, RelocationKind::HighByte),
                            ByteRelocationKind::FullWord => (2, RelocationKind::Absolute),
                            ByteRelocationKind::FullLong => (3, RelocationKind::Absolute),
                        };
                        fixups.push(Fixup {
                            segment: current_segment.clone(),
                            offset: operand_offset,
                            width: reloc_width,
                            kind: reloc_kind,
                            label: label.clone(),
                            addend: 0,
                            span: relocation_span,
                            call_metadata: None,
                        });
                    }
                    Some(OperandOp::ImmediateWordRelocation { label, addend, .. }) => {
                        if width != 2 {
                            diagnostics.push(
                                Diagnostic::error(
                                    op.span,
                                    "`&&` immediate address-of requires 16-bit accumulator/index width",
                                )
                                .with_help(
                                    "switch to `@a16`/`@i16`, or use `&<symbol`/`&>symbol` for byte halves in 8-bit mode",
                                ),
                            );
                            emit_zeroes(segment, width, op.span, &mut diagnostics);
                            continue;
                        }
                        emit_zeroes(segment, width, op.span, &mut diagnostics);
                        let relocation_span = relocation_span_for_label(source_map, op.span, label);
                        fixups.push(Fixup {
                            segment: current_segment.clone(),
                            offset: operand_offset,
                            width: 2,
                            kind: RelocationKind::Absolute,
                            label: label.clone(),
                            addend: *addend,
                            span: relocation_span,
                            call_metadata: None,
                        });
                    }
                    Some(OperandOp::ImmediateFarRelocation { label, addend, .. }) => {
                        if width != 3 {
                            diagnostics.push(
                                Diagnostic::error(
                                    op.span,
                                    "`&&&` immediate far address-of requires 24-bit operand width",
                                )
                                .with_help(
                                    "this operator is only valid for far-immediate instructions in `@a16`/`@al` mode",
                                ),
                            );
                            emit_zeroes(segment, width, op.span, &mut diagnostics);
                            continue;
                        }
                        emit_zeroes(segment, width, op.span, &mut diagnostics);
                        let relocation_span = relocation_span_for_label(source_map, op.span, label);
                        fixups.push(Fixup {
                            segment: current_segment.clone(),
                            offset: operand_offset,
                            width: 3,
                            kind: RelocationKind::Absolute,
                            label: label.clone(),
                            addend: *addend,
                            span: relocation_span,
                            call_metadata: None,
                        });
                    }
                    Some(OperandOp::Address { value, .. }) => match value {
                        AddressValue::Literal(literal) => {
                            if matches!(
                                encoding.mode,
                                AddressingMode::Relative8 | AddressingMode::Relative16
                            ) {
                                diagnostics.push(Diagnostic::error(
                                    op.span,
                                    "relative branch operands must be labels, not numeric literals",
                                ));
                            } else {
                                emit_literal(segment, *literal, width, op.span, &mut diagnostics)
                            }
                        }
                        AddressValue::Label(label) | AddressValue::LabelOffset { label, .. } => {
                            let addend = match value {
                                AddressValue::LabelOffset { addend, .. } => *addend,
                                _ => 0,
                            };
                            emit_zeroes(segment, width, op.span, &mut diagnostics);
                            let relocation_span =
                                relocation_span_for_label(source_map, op.span, label);
                            let is_call = matches!(mnemonic.as_str(), "jsr" | "jsl");
                            fixups.push(Fixup {
                                segment: current_segment.clone(),
                                offset: operand_offset,
                                width: width as u8,
                                kind: if matches!(
                                    encoding.mode,
                                    AddressingMode::Relative8 | AddressingMode::Relative16
                                ) {
                                    RelocationKind::Relative
                                } else {
                                    RelocationKind::Absolute
                                },
                                label: label.clone(),
                                addend,
                                span: relocation_span,
                                call_metadata: if is_call {
                                    Some(CallMetadata {
                                        caller_a_width: m_wide,
                                        caller_i_width: x_wide,
                                    })
                                } else {
                                    None
                                },
                            });
                        }
                    },
                    Some(OperandOp::BlockMove { src, dst }) => {
                        append_bytes(segment, &[*dst, *src], op.span, &mut diagnostics);
                    }
                }

                if mnemonic == "plp" || mnemonic == "rti" {
                    m_wide = None;
                    x_wide = None;
                    let cause = if mnemonic == "plp" {
                        UnknownWidthCause::Plp
                    } else {
                        UnknownWidthCause::Rti
                    };
                    m_unknown_cause = Some(cause);
                    x_unknown_cause = Some(cause);
                }
            }
        }
    }

    // Post-loop pass: any diagnostic whose primary span matches an op that was
    // inlined from a foreign source gets an `InlineOrigin` supplement pointing
    // back to the original location. We walk ops in reverse so that, when
    // multiple inlined ops collapse to the same call-site span (every inlined
    // op shares the call site after `retarget_spans`), the *last* statement's
    // origin wins — most diagnostics are produced for the trailing instruction
    // of an inline body (e.g. the `adc` of `clc; adc #b`), which is the line
    // the user wants to see referenced.
    let mut origin_for_span: rustc_hash::FxHashMap<(crate::span::SourceId, usize, usize), Span> =
        rustc_hash::FxHashMap::default();
    for op in program.ops.iter().rev() {
        if let Some(origin) = op.origin {
            origin_for_span
                .entry((op.span.source_id, op.span.start, op.span.end))
                .or_insert(origin);
        }
    }
    if !origin_for_span.is_empty() {
        for diag in diagnostics.iter_mut() {
            let key = (diag.primary.source_id, diag.primary.start, diag.primary.end);
            if let Some(&origin) = origin_for_span.get(&key) {
                attach_inline_origin(diag, origin, source_map);
            }
        }
    }

    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    let skip_default_empty = segments.len() > 1
        && segments
            .get(crate::DEFAULT_SEGMENT)
            .is_some_and(|state| state.chunks.is_empty());

    let mut listing_blocks = Vec::new();
    let mut sections = IndexMap::new();
    for (segment_name, state) in &segments {
        if skip_default_empty && segment_name == crate::DEFAULT_SEGMENT {
            continue;
        }

        listing_blocks.push(format_listing_block(segment_name, &state.chunks));
        sections.insert(
            segment_name.clone(),
            Section {
                chunks: state.chunks.clone(),
            },
        );
    }

    let mut symbols = Vec::new();
    for (name, (segment, offset, span)) in &labels {
        if skip_default_empty && segment == crate::DEFAULT_SEGMENT {
            continue;
        }
        symbols.push(Symbol {
            name: name.clone(),
            global: true,
            definition: Some(SymbolDefinition::Section {
                section: segment.clone(),
                offset: *offset,
                source: source_location_for_span(source_map, *span),
            }),
            function_metadata: function_label_metadata.get(name.as_str()).copied(),
        });
    }
    for (name, (address, span)) in &absolute_symbols {
        symbols.push(Symbol {
            name: name.clone(),
            global: true,
            definition: Some(SymbolDefinition::Absolute {
                address: *address,
                source: source_location_for_span(source_map, *span),
            }),
            function_metadata: None,
        });
    }
    symbols.sort_by(|a, b| a.name.cmp(&b.name));

    let mut relocations = Vec::new();
    for fixup in &fixups {
        if skip_default_empty && fixup.segment == crate::DEFAULT_SEGMENT {
            continue;
        }
        relocations.push(Relocation {
            section: fixup.segment.clone(),
            offset: fixup.offset,
            width: fixup.width,
            kind: fixup.kind,
            symbol: fixup.label.clone(),
            addend: fixup.addend,
            source: source_location_for_span(source_map, fixup.span),
            call_metadata: fixup.call_metadata,
        });
    }

    let mut grouped_disassembly: IndexMap<(String, String), Vec<u32>> = IndexMap::new();
    for site in function_instruction_sites {
        if skip_default_empty && site.segment == crate::DEFAULT_SEGMENT {
            continue;
        }
        grouped_disassembly
            .entry((site.segment, site.function))
            .or_default()
            .push(site.offset);
    }

    let function_disassembly = grouped_disassembly
        .into_iter()
        .map(|((section, function), instruction_offsets)| {
            let (m_wide, x_wide) = function_initial_modes
                .get(&(section.clone(), function.clone()))
                .copied()
                .unwrap_or((false, false));
            FunctionDisassembly {
                section,
                function,
                instruction_offsets,
                m_wide,
                x_wide,
            }
        })
        .collect();
    data_string_fragments.sort_by(|lhs, rhs| {
        lhs.section
            .cmp(&rhs.section)
            .then_with(|| lhs.offset.cmp(&rhs.offset))
    });

    Ok(EmitObjectOutput {
        object: O65Object {
            sections,
            symbols,
            relocations,
            function_disassembly,
            data_string_fragments,
            listing: listing_blocks.join("\n\n"),
        },
        addressable_sites,
    })
}

fn active_cursor(segment: &SegmentState) -> u32 {
    segment.fixed_cursor.unwrap_or(segment.section_offset)
}

fn emit_zeroes(
    segment: &mut SegmentState,
    count: usize,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if count == 0 {
        return;
    }
    let bytes = vec![0_u8; count];
    append_bytes(segment, &bytes, span, diagnostics);
}

fn append_bytes(
    segment: &mut SegmentState,
    bytes: &[u8],
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if bytes.is_empty() {
        return;
    }

    let len = match u32::try_from(bytes.len()) {
        Ok(len) => len,
        Err(_) => {
            diagnostics.push(Diagnostic::error(
                span,
                "emitted bytes exceed 32-bit section offset range",
            ));
            return;
        }
    };

    let section_start = segment.section_offset;
    let fixed_start = segment.fixed_cursor;

    let can_extend_last = segment
        .chunks
        .last()
        .is_some_and(|last| can_extend_chunk(last, section_start, fixed_start));

    if can_extend_last {
        if let Some(last) = segment.chunks.last_mut() {
            last.bytes.extend_from_slice(bytes);
        }
    } else {
        segment.chunks.push(SectionChunk {
            offset: section_start,
            address: fixed_start,
            bytes: bytes.to_vec(),
        });
    }

    segment.section_offset = match segment.section_offset.checked_add(len) {
        Some(next) => next,
        None => {
            diagnostics.push(Diagnostic::error(
                span,
                "section offset overflow while appending bytes",
            ));
            return;
        }
    };

    if let Some(fixed_start) = segment.fixed_cursor {
        segment.fixed_cursor = match fixed_start.checked_add(len) {
            Some(next) => Some(next),
            None => {
                diagnostics.push(Diagnostic::error(
                    span,
                    "absolute address overflow while appending bytes",
                ));
                None
            }
        };
    }
}

fn can_extend_chunk(last: &SectionChunk, section_start: u32, fixed_start: Option<u32>) -> bool {
    let last_len = match u32::try_from(last.bytes.len()) {
        Ok(len) => len,
        Err(_) => return false,
    };

    let Some(last_section_end) = last.offset.checked_add(last_len) else {
        return false;
    };
    if last_section_end != section_start {
        return false;
    }

    match (last.address, fixed_start) {
        (None, None) => true,
        (Some(last_addr), Some(fixed_addr)) => {
            let Some(last_addr_end) = last_addr.checked_add(last_len) else {
                return false;
            };
            last_addr_end == fixed_addr
        }
        _ => false,
    }
}

fn apply_nocross_if_needed(
    segment: &mut SegmentState,
    next_size: usize,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(boundary) = segment.nocross_boundary.take() else {
        return;
    };

    let boundary = usize::from(boundary);
    if boundary == 0 {
        diagnostics.push(Diagnostic::error(span, "nocross value must be non-zero"));
        return;
    }

    if next_size > boundary {
        diagnostics.push(Diagnostic::error(
            span,
            format!("emit chunk size {next_size} exceeds nocross boundary {boundary}"),
        ));
        return;
    }

    let cursor = active_cursor(segment) as usize;
    let offset_in_window = cursor % boundary;
    if offset_in_window + next_size > boundary {
        let pad = boundary - offset_in_window;
        emit_zeroes(segment, pad, span, diagnostics);
    }
}

fn emit_literal(
    segment: &mut SegmentState,
    value: u32,
    width: usize,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match width {
        0 => {}
        1 => match u8::try_from(value) {
            Ok(value) => append_bytes(segment, &[value], span, diagnostics),
            Err(_) => diagnostics.push(Diagnostic::error(
                span,
                "value does not fit in 8-bit operand",
            )),
        },
        2 => match u16::try_from(value) {
            Ok(value) => append_bytes(segment, &value.to_le_bytes(), span, diagnostics),
            Err(_) => diagnostics.push(Diagnostic::error(
                span,
                "value does not fit in 16-bit operand",
            )),
        },
        3 => {
            if value > 0x00FF_FFFF {
                diagnostics.push(Diagnostic::error(
                    span,
                    "value does not fit in 24-bit operand",
                ));
            } else {
                let bytes24 = value.to_le_bytes();
                append_bytes(segment, &bytes24[..3], span, diagnostics);
            }
        }
        _ => diagnostics.push(Diagnostic::error(span, "unsupported operand width")),
    }
}

fn format_listing_block(segment_name: &str, chunks: &[SectionChunk]) -> String {
    let mut out = String::new();
    out.push_str(&format!("[{segment_name}]\n"));

    if chunks.is_empty() {
        out.push_str("(empty)\n");
        return out;
    }

    for chunk in chunks {
        for (index, row) in chunk.bytes.chunks(16).enumerate() {
            let mut hex = String::new();
            for (i, byte) in row.iter().enumerate() {
                if i > 0 {
                    hex.push(' ');
                }
                hex.push_str(&format!("{byte:02X}"));
            }

            let address = chunk.offset + (index as u32 * 16);
            out.push_str(&format!("{address:06X}: {hex}\n"));
        }
    }

    out
}

fn source_location_for_span(source_map: &SourceMap, span: Span) -> Option<SourceLocation> {
    let file = source_map.get(span.source_id)?;
    let (line, column) = file.line_col(span.start);
    let end_offset = span.end.saturating_sub(1).max(span.start);
    let (end_line, end_column_inclusive) = file.line_col(end_offset);
    let column_end = if end_line == line {
        end_column_inclusive.saturating_add(1)
    } else {
        column.saturating_add(1)
    };
    let line_index = line.saturating_sub(1);
    let line_text = file
        .text
        .lines()
        .nth(line_index)
        .unwrap_or_default()
        .to_string();

    Some(SourceLocation {
        file: file.name.clone(),
        line: line as u32,
        column: column as u32,
        column_end: column_end as u32,
        line_text,
    })
}

fn relocation_span_for_label(source_map: &SourceMap, fallback: Span, label: &str) -> Span {
    let Some(file) = source_map.get(fallback.source_id) else {
        return fallback;
    };

    let text_len = file.text.len();
    let start = fallback.start.min(text_len);
    let end = fallback.end.min(text_len);
    if start >= end {
        return fallback;
    }

    let haystack = &file.text[start..end];
    for candidate in label_span_candidates(label) {
        if let Some(idx) = haystack.find(&candidate) {
            return Span::new(
                fallback.source_id,
                start.saturating_add(idx),
                start.saturating_add(idx).saturating_add(candidate.len()),
            );
        }
    }

    fallback
}

fn label_span_candidates(label: &str) -> Vec<String> {
    let mut candidates = vec![label.to_string()];
    if let Some(local_name) = label.split("::.").nth(1) {
        candidates.push(format!(".{local_name}"));
    }
    candidates
}

fn string_literal_text_for_emit(
    bytes: &[u8],
    source_map: &SourceMap,
    span: Span,
) -> Option<String> {
    let source = source_location_for_span(source_map, span)?;
    if !source.line_text.trim_start().starts_with('"') {
        return None;
    }

    std::str::from_utf8(bytes).ok().map(|text| text.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ModeContract;
    use crate::hir::{
        AddressOperandMode, AddressValue, ByteRelocationKind, InstructionOp, Op, OperandOp, Program,
    };
    use crate::span::{SourceId, SourceMap, Span, Spanned};

    fn op(node: Op) -> Spanned<Op> {
        Spanned::new(node, Span::new(SourceId(0), 0, 0))
    }

    #[test]
    fn address_creates_sparse_chunk_without_zero_padding() {
        let mut source_map = SourceMap::default();
        source_map.add_source("test.k65", "func a {}\n");
        let program = Program {
            ops: vec![
                op(Op::Label("func_a".to_string())),
                op(Op::EmitBytes(vec![0xEA, 0x60])),
                op(Op::Address(0x4000)),
                op(Op::Label("func_b".to_string())),
                op(Op::EmitBytes(vec![0xEA, 0x60])),
            ],
        };

        let emitted = emit_object(&program, &source_map, &SemanticModel::default(), None)
            .expect("emit object");
        let section = emitted
            .object
            .sections
            .get(crate::DEFAULT_SEGMENT)
            .expect("default section");

        assert_eq!(section.chunks.len(), 2);
        assert_eq!(section.chunks[0].offset, 0);
        assert_eq!(section.chunks[0].address, None);
        assert_eq!(section.chunks[0].bytes, vec![0xEA, 0x60]);

        assert_eq!(section.chunks[1].offset, 2);
        assert_eq!(section.chunks[1].address, Some(0x4000));
        assert_eq!(section.chunks[1].bytes, vec![0xEA, 0x60]);
    }

    #[test]
    fn labels_after_address_keep_compact_section_offsets() {
        let mut source_map = SourceMap::default();
        source_map.add_source("test.k65", "func a {}\n");
        let program = Program {
            ops: vec![
                op(Op::Label("func_a".to_string())),
                op(Op::EmitBytes(vec![0xEA, 0x60])),
                op(Op::Address(0x4000)),
                op(Op::Label("func_b".to_string())),
                op(Op::EmitBytes(vec![0xEA, 0x60])),
            ],
        };

        let emitted = emit_object(&program, &source_map, &SemanticModel::default(), None)
            .expect("emit object");
        let mut symbols = emitted.object.symbols;
        symbols.sort_by(|a, b| a.name.cmp(&b.name));

        assert_eq!(symbols.len(), 2);
        assert_eq!(symbols[0].name, "func_a");
        assert!(matches!(
            symbols[0].definition.as_ref(),
            Some(SymbolDefinition::Section { offset: 0, .. })
        ));
        assert_eq!(symbols[1].name, "func_b");
        assert!(matches!(
            symbols[1].definition.as_ref(),
            Some(SymbolDefinition::Section { offset: 2, .. })
        ));
    }

    #[test]
    fn keeps_unresolved_labels_for_linker() {
        let mut source_map = SourceMap::default();
        source_map.add_source("test.k65", "func a {}\n");
        let program = Program {
            ops: vec![
                op(Op::FunctionStart {
                    name: "main".to_string(),
                    mode_contract: ModeContract::default(),
                    is_entry: true,
                    is_far: false,
                }),
                op(Op::Instruction(InstructionOp {
                    mnemonic: "lda".to_string(),
                    operand: Some(OperandOp::Address {
                        value: AddressValue::Label("missing".to_string()),
                        size_hint: AddressSizeHint::Auto,
                        mode: AddressOperandMode::Direct { index: None },
                    }),
                })),
                op(Op::FunctionEnd),
            ],
        };

        let emitted = emit_object(&program, &source_map, &SemanticModel::default(), None)
            .expect("unresolved labels deferred to linker");
        assert_eq!(emitted.object.relocations.len(), 1);
        assert_eq!(emitted.object.relocations[0].symbol, "missing");
    }

    #[test]
    fn keeps_immediate_byte_relocations_for_linker() {
        let mut source_map = SourceMap::default();
        source_map.add_source("test.k65", "func main { a=&<missing }\n");
        let program = Program {
            ops: vec![
                op(Op::FunctionStart {
                    name: "main".to_string(),
                    mode_contract: ModeContract::default(),
                    is_entry: true,
                    is_far: false,
                }),
                op(Op::Instruction(InstructionOp {
                    mnemonic: "lda".to_string(),
                    operand: Some(OperandOp::ImmediateByteRelocation {
                        kind: ByteRelocationKind::LowByte,
                        label: "missing".to_string(),
                    }),
                })),
                op(Op::FunctionEnd),
            ],
        };

        let emitted = emit_object(&program, &source_map, &SemanticModel::default(), None)
            .expect("unresolved labels deferred to linker");
        assert_eq!(emitted.object.symbols.len(), 0);
        assert_eq!(emitted.object.relocations.len(), 1);
        assert_eq!(emitted.object.relocations[0].symbol, "missing");
        assert_eq!(emitted.object.relocations[0].kind, RelocationKind::LowByte);
        assert_eq!(emitted.object.relocations[0].width, 1);
    }

    #[test]
    fn emits_absolute_symbol_definitions() {
        let mut source_map = SourceMap::default();
        source_map.add_source("test.k65", "var foo = 0x1234\n");
        let program = Program {
            ops: vec![op(Op::DefineAbsoluteSymbol {
                name: "foo".to_string(),
                address: 0x1234,
            })],
        };

        let emitted = emit_object(&program, &source_map, &SemanticModel::default(), None)
            .expect("emit object");
        assert_eq!(emitted.object.symbols.len(), 1);
        assert!(matches!(
            emitted.object.symbols[0].definition.as_ref(),
            Some(SymbolDefinition::Absolute {
                address: 0x1234,
                ..
            })
        ));
    }

    #[test]
    fn immediate_overflow_8bit_operand() {
        let mut source_map = SourceMap::default();
        source_map.add_source("test.k65", "func main { @a8 cmp #$100 }\n");
        // Sep(0x20) sets accumulator to 8-bit (m_wide = false).
        // cmp #256 uses ImmediateM → width=1 → 256 overflows u8.
        let program = Program {
            ops: vec![
                op(Op::Sep {
                    mask: 0x20,
                    fixed: false,
                }),
                op(Op::Instruction(InstructionOp {
                    mnemonic: "cmp".to_string(),
                    operand: Some(OperandOp::Immediate(0x100)),
                })),
            ],
        };

        let err = emit_object(&program, &source_map, &SemanticModel::default(), None)
            .expect_err("should fail on 8-bit overflow");
        let msg = err
            .iter()
            .map(|d| d.message.as_str())
            .collect::<Vec<_>>()
            .join("; ");
        assert!(
            msg.contains("value does not fit in 8-bit operand"),
            "unexpected error: {msg}"
        );
    }

    #[test]
    fn immediate_overflow_16bit_operand() {
        let mut source_map = SourceMap::default();
        source_map.add_source("test.k65", "func main { @a16 cmp #$10000 }\n");
        // Rep(0x20) sets accumulator to 16-bit (m_wide = true).
        // cmp #0x10000 uses ImmediateM → width=2 → 0x10000 overflows u16.
        let program = Program {
            ops: vec![
                op(Op::Rep {
                    mask: 0x20,
                    fixed: false,
                }),
                op(Op::Instruction(InstructionOp {
                    mnemonic: "cmp".to_string(),
                    operand: Some(OperandOp::Immediate(0x10000)),
                })),
            ],
        };

        let err = emit_object(&program, &source_map, &SemanticModel::default(), None)
            .expect_err("should fail on 16-bit overflow");
        let msg = err
            .iter()
            .map(|d| d.message.as_str())
            .collect::<Vec<_>>()
            .join("; ");
        assert!(
            msg.contains("value does not fit in 16-bit operand"),
            "unexpected error: {msg}"
        );
    }

    #[test]
    fn is_locally_resolvable_checks_all_namespaces() {
        let mut sema = SemanticModel::default();
        sema.consts.insert(
            "ZP_TEMP".into(),
            crate::sema::ConstMeta {
                value: k816_eval::Number::int(0),
            },
        );
        assert!(is_locally_resolvable(&sema, "ZP_TEMP"));
        assert!(!is_locally_resolvable(&sema, "missing"));
    }
}
