/*****************************************************************************/
/*                                                                           */
/*                                   op.rs                                   */
/*                                                                           */
/*        The common lowered operation model shared by native and HLA        */
/*                                                                           */
/*                                                                           */
/* Copyright (c) 2026 Tomasz Sterna (smokku)                                 */
/*                                                                           */
/* Permission to use, copy, modify, and/or distribute this software for any  */
/* purpose with or without fee is hereby granted.                            */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  */
/* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   */
/* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            */
/*                                                                           */
/*****************************************************************************/

//! `KOp`: the common, assembly-shaped operation both native and HLA syntax
//! lower into.
//!
//! After lowering, the optimizer and emitter must not care which syntax
//! produced an operation — only its [`Origin`] records that, for source
//! annotations. Only the slice variants are populated; the enum shapes are
//! the frozen contract and grow in sub-plans C/D/E.

use crate::effects::{Effects, instruction_effects};
use crate::isa::{AddressingMode, Mnemonic};
use crate::source::Span;
use crate::symbol::SymbolId;

/// Where a lowered operation came from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Origin {
    /// Written by the user as a native instruction.
    User,
    /// Expanded from an HLA statement.
    HlaExpansion,
    /// Synthesized by the compiler (entry-mode setup, implicit `rts`).
    CompilerGenerated,
}

/// One lowered operation with its source position and origin.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KOp {
    pub kind: KOpKind,
    pub span: Span,
    pub origin: Origin,
}

/// The operation itself. Slice subset: instructions and labels; segments,
/// data, alignment, imports/exports, and mode assumptions arrive with C/D.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KOpKind {
    Instruction(Instruction),
    Label(SymbolId),
}

/// A strongly typed instruction: validated mnemonic plus typed operand.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Instruction {
    pub mnemonic: Mnemonic,
    pub operand: Option<Operand>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operand {
    /// `#value` — the value is already a resolved constant in the slice.
    Immediate(u32),
    /// An absolute memory reference through a declared symbol.
    Address(SymbolId),
}

impl Instruction {
    /// The addressing mode implied by the operand shape.
    pub fn mode(&self) -> AddressingMode {
        match self.operand {
            None => AddressingMode::Implied,
            Some(Operand::Immediate(_)) => AddressingMode::Immediate,
            Some(Operand::Address(_)) => AddressingMode::Absolute,
        }
    }

    /// The conservative CPU effects of this instruction, derived from the
    /// ISA table (never stored on the op itself).
    pub fn effects(&self) -> Effects {
        let immediate = match self.operand {
            Some(Operand::Immediate(value)) => Some(value),
            _ => None,
        };
        instruction_effects(self.mnemonic, self.mode(), immediate)
    }
}

/// Width of one CPU register pair per the 65816 M/X flags.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegisterWidth {
    W8,
    W16,
}

/// A function's entry-mode contract: the widths its body assumes on entry.
/// `None` on an axis means the contract leaves that width unspecified.
///
/// This is the minimal slice of the width model (sub-plan A contract 6);
/// scoped blocks, exit inference, and call-site bridging land in sub-plan D.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct WidthContract {
    pub a: Option<RegisterWidth>,
    pub i: Option<RegisterWidth>,
}

/// One lowered function: its symbol, entry contract, and operation stream.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KFunction {
    pub symbol: SymbolId,
    pub contract: WidthContract,
    pub ops: Vec<KOp>,
}

/// A complete lowered compilation unit.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct KProgram {
    pub functions: Vec<KFunction>,
}
