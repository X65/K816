/*****************************************************************************/
/*                                                                           */
/*                                 symbol.rs                                 */
/*                                                                           */
/*                     Symbol table and symbol resolution                    */
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

//! The symbol table: interned declarations, name lookup, and scoping.
//!
//! Two scopes exist. Unit-level names — variables, constants, functions,
//! global labels, and data-block labels — are visible across the whole
//! compilation unit and, once linkage is derived, across the workspace.
//! Function-local `.name` labels are visible only inside the function that
//! declares them, so two functions may reuse a name without colliding
//! (`doc/syntax-reference.md`, "Labels" / "Local").
//!
//! Every symbol that has an address also carries an [`AddressSize`]. It is
//! the axis operand classification uses to choose an encoding, and the axis
//! `.import`/`.export` qualifiers come from — ca65 validates addressing
//! modes against an imported symbol's address size, so getting it wrong
//! shows up as an assembly error rather than a wrong byte.

use std::collections::HashMap;

use crate::source::Span;

/// An interned symbol identifier.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolId(u32);

impl SymbolId {
    pub fn index(self) -> usize {
        self.0 as usize
    }

    /// The identifier at `index`. Only a [`SymbolTable`] hands out real
    /// identifiers; this exists so tests can name one without building a
    /// table around it.
    pub fn from_index(index: usize) -> Self {
        Self(u32::try_from(index).expect("symbol index fits u32"))
    }
}

/// How wide a plain reference to a symbol encodes its address.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AddressSize {
    /// One operand byte, `dp` at the declaration and `z:` in ca65.
    DirectPage,
    /// Two operand bytes — the default.
    Absolute,
    /// Three operand bytes, `far` at the declaration and `f:` in ca65.
    Long,
}

impl AddressSize {
    /// The size a compile-time-known address takes when the declaration
    /// carries no prefix: the narrowest encoding that holds the value.
    pub fn of_value(address: u32) -> Self {
        if address <= 0xFF {
            Self::DirectPage
        } else if address <= 0xFFFF {
            Self::Absolute
        } else {
            Self::Long
        }
    }
}

/// Where a label is visible.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LabelScope {
    /// `name:` — visible across the compilation unit.
    Global,
    /// `.name:` — visible only inside the declaring function.
    Local,
}

/// What a symbol denotes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymbolKind {
    /// `const NAME = expr` — a compile-time value with no storage and no
    /// address. Using one in address position is an error: a `const` names
    /// a value, not a memory location.
    Const {
        value: u32,
    },
    /// `var NAME [= addr]`. `address` is `None` for a linker-allocated
    /// variable, whose address only ld65 knows.
    Var {
        address: Option<u32>,
        size: AddressSize,
    },
    /// A code label, `name:` or `.name:`.
    Label {
        scope: LabelScope,
        size: AddressSize,
    },
    /// A label inside a data block, including a block's own name.
    DataLabel {
        size: AddressSize,
    },
    Function {
        size: AddressSize,
    },
}

impl SymbolKind {
    /// The address size of plain references to this symbol, or `None` when
    /// the symbol has no address at all.
    pub fn address_size(self) -> Option<AddressSize> {
        match self {
            Self::Const { .. } => None,
            Self::Var { size, .. }
            | Self::Label { size, .. }
            | Self::DataLabel { size }
            | Self::Function { size } => Some(size),
        }
    }

    /// The address this symbol is pinned at, when K816 knows it now.
    ///
    /// Only a fixed-address variable qualifies. Labels, data labels, and
    /// functions are placed by ld65, so K816 must never assume a value for
    /// them — that is what keeps auto-shrink from picking a direct-page
    /// encoding for an address it only *thinks* is in page zero.
    pub fn known_address(self) -> Option<u32> {
        match self {
            Self::Var { address, .. } => address,
            Self::Const { .. }
            | Self::Label { .. }
            | Self::DataLabel { .. }
            | Self::Function { .. } => None,
        }
    }

    /// True when the symbol names a memory location an operand may address.
    pub fn is_addressable(self) -> bool {
        match self {
            Self::Var { .. } | Self::Label { .. } | Self::DataLabel { .. } => true,
            Self::Const { .. } | Self::Function { .. } => false,
        }
    }

    /// A short noun for diagnostics, e.g. "a `const`".
    pub fn describe(self) -> &'static str {
        match self {
            Self::Const { .. } => "a `const`",
            Self::Var { .. } => "a variable",
            Self::Label {
                scope: LabelScope::Global,
                ..
            } => "a label",
            Self::Label {
                scope: LabelScope::Local,
                ..
            } => "a local label",
            Self::DataLabel { .. } => "a data label",
            Self::Function { .. } => "a function",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbol {
    /// The name exactly as written, so diagnostics quote the source. A local
    /// label keeps its leading `.`.
    pub name: String,
    pub kind: SymbolKind,
    /// The function this symbol is scoped to, for local labels only.
    pub scope: Option<SymbolId>,
    /// Span of the declaring name, for "declared here" secondary labels.
    /// For an imported symbol this points into the unit that defines it.
    pub declaration: Span,
    /// True when another unit in the workspace defines this name. K816 emits
    /// an `.import` for it and ld65 resolves it; the kind and address size
    /// are the defining unit's, because ca65 validates addressing modes
    /// against an imported symbol's address size.
    pub imported: bool,
}

impl Symbol {
    /// True when this symbol is visible to the whole unit — and therefore a
    /// candidate for `.export`.
    pub fn is_unit_level(&self) -> bool {
        self.scope.is_none()
    }
}

/// All symbols of one compilation unit.
#[derive(Debug, Default)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
    unit: HashMap<String, SymbolId>,
    /// Function-scoped names, keyed by the owning function and the name.
    local: HashMap<(SymbolId, String), SymbolId>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Declares a unit-level `name`. On a duplicate, returns the existing
    /// symbol as `Err` so the caller can attach a "previously declared here"
    /// secondary label.
    pub fn declare(
        &mut self,
        name: &str,
        kind: SymbolKind,
        declaration: Span,
    ) -> Result<SymbolId, SymbolId> {
        if let Some(&existing) = self.unit.get(name) {
            return Err(existing);
        }
        let id = self.intern(name, kind, None, declaration);
        self.unit.insert(name.to_owned(), id);
        Ok(id)
    }

    /// Declares a name another unit defines, if this unit has not declared
    /// one of its own. Returns the identifier either way.
    pub fn declare_imported(
        &mut self,
        name: &str,
        kind: SymbolKind,
        declaration: Span,
    ) -> SymbolId {
        if let Some(&existing) = self.unit.get(name) {
            return existing;
        }
        let id = self.intern(name, kind, None, declaration);
        self.symbols[id.index()].imported = true;
        self.unit.insert(name.to_owned(), id);
        id
    }

    /// Declares `name` inside `owner`'s scope. Two functions may declare the
    /// same local label; a repeat within one function is the duplicate.
    pub fn declare_local(
        &mut self,
        owner: SymbolId,
        name: &str,
        kind: SymbolKind,
        declaration: Span,
    ) -> Result<SymbolId, SymbolId> {
        let key = (owner, name.to_owned());
        if let Some(&existing) = self.local.get(&key) {
            return Err(existing);
        }
        let id = self.intern(name, kind, Some(owner), declaration);
        self.local.insert(key, id);
        Ok(id)
    }

    fn intern(
        &mut self,
        name: &str,
        kind: SymbolKind,
        scope: Option<SymbolId>,
        declaration: Span,
    ) -> SymbolId {
        let id = SymbolId(u32::try_from(self.symbols.len()).expect("too many symbols"));
        self.symbols.push(Symbol {
            name: name.to_owned(),
            kind,
            scope,
            declaration,
            imported: false,
        });
        id
    }

    pub fn lookup(&self, name: &str) -> Option<SymbolId> {
        self.unit.get(name).copied()
    }

    /// Looks `name` up in `owner`'s scope only. Local labels never fall back
    /// to unit scope: `.loop` and `loop` are different names.
    pub fn lookup_local(&self, owner: SymbolId, name: &str) -> Option<SymbolId> {
        self.local.get(&(owner, name.to_owned())).copied()
    }

    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.index()]
    }

    pub fn name(&self, id: SymbolId) -> &str {
        &self.symbol(id).name
    }

    pub fn kind(&self, id: SymbolId) -> SymbolKind {
        self.symbol(id).kind
    }

    /// Replaces a symbol's kind. Declaration runs before values are known
    /// for the symbols whose value depends on other declarations.
    pub fn set_kind(&mut self, id: SymbolId, kind: SymbolKind) {
        self.symbols[id.index()].kind = kind;
    }

    pub fn iter(&self) -> impl Iterator<Item = (SymbolId, &Symbol)> {
        self.symbols
            .iter()
            .enumerate()
            .map(|(index, symbol)| (SymbolId(index as u32), symbol))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{SourceMap, Span};

    fn fixed(address: u32) -> SymbolKind {
        SymbolKind::Var {
            address: Some(address),
            size: AddressSize::of_value(address),
        }
    }

    #[test]
    fn declare_lookup_and_duplicate_detection() {
        let mut map = SourceMap::new();
        let file = map.add_file("t.k816", "var SCREEN = $0400\nvar SCREEN = $0401\n");
        let mut table = SymbolTable::new();

        let first = table
            .declare("SCREEN", fixed(0x0400), Span::new(file, 4, 10))
            .expect("first declaration succeeds");
        assert_eq!(table.lookup("SCREEN"), Some(first));
        assert_eq!(table.name(first), "SCREEN");
        assert_eq!(table.symbol(first).kind, fixed(0x0400));

        let duplicate = table.declare("SCREEN", fixed(0x0401), Span::new(file, 23, 29));
        assert_eq!(duplicate, Err(first));
        assert_eq!(table.lookup("MISSING"), None);
    }

    #[test]
    fn two_functions_may_reuse_one_local_label_name() {
        let mut map = SourceMap::new();
        let file = map.add_file("t.k816", "func one { .loop: }\nfunc two { .loop: }\n");
        let span = Span::new(file, 0, 1);
        let mut table = SymbolTable::new();

        let label = SymbolKind::Label {
            scope: LabelScope::Local,
            size: AddressSize::Absolute,
        };
        let one = table
            .declare(
                "one",
                SymbolKind::Function {
                    size: AddressSize::Absolute,
                },
                span,
            )
            .unwrap();
        let two = table
            .declare(
                "two",
                SymbolKind::Function {
                    size: AddressSize::Absolute,
                },
                span,
            )
            .unwrap();

        let first = table.declare_local(one, ".loop", label, span).unwrap();
        let second = table.declare_local(two, ".loop", label, span).unwrap();
        assert_ne!(first, second, "the two labels are distinct symbols");
        assert_eq!(table.lookup_local(one, ".loop"), Some(first));
        assert_eq!(table.lookup_local(two, ".loop"), Some(second));

        // A repeat inside one function is still a duplicate…
        assert_eq!(
            table.declare_local(one, ".loop", label, span),
            Err(first),
            "a local label may not be declared twice in one function"
        );
        // …and a local label never leaks into unit scope.
        assert_eq!(table.lookup(".loop"), None);
    }

    #[test]
    fn address_size_follows_the_value_when_no_prefix_says_otherwise() {
        assert_eq!(AddressSize::of_value(0x00), AddressSize::DirectPage);
        assert_eq!(AddressSize::of_value(0xFF), AddressSize::DirectPage);
        assert_eq!(AddressSize::of_value(0x0100), AddressSize::Absolute);
        assert_eq!(AddressSize::of_value(0xFFFF), AddressSize::Absolute);
        assert_eq!(AddressSize::of_value(0x01_0000), AddressSize::Long);
    }

    #[test]
    fn only_a_pinned_variable_has_an_address_k816_knows() {
        assert_eq!(fixed(0x0400).known_address(), Some(0x0400));
        assert_eq!(
            SymbolKind::Var {
                address: None,
                size: AddressSize::Absolute
            }
            .known_address(),
            None,
            "a linker-allocated variable is ld65's to place"
        );
        assert_eq!(
            SymbolKind::Label {
                scope: LabelScope::Global,
                size: AddressSize::Absolute
            }
            .known_address(),
            None,
            "a label is ld65's to place"
        );
    }
}
