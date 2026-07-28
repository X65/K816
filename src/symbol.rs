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

//! The symbol table: interned declarations and name lookup.
//!
//! Slice subset: fixed-address variables (`var NAME = $addr`) and functions.
//! Allocated variables, imports/exports, and scoped locals arrive with
//! sub-plans C/E.

use std::collections::HashMap;

use crate::source::Span;

/// An interned symbol identifier.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

/// What a symbol denotes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymbolKind {
    /// `var NAME = $addr` — a variable pinned at a fixed address.
    FixedAddressVar {
        address: u32,
    },
    /// `const NAME = expr` — a compile-time value with no storage and no
    /// address. Using one in address position is an error: a `const` names
    /// a value, not a memory location.
    Const {
        value: u32,
    },
    Function,
}

impl SymbolKind {
    /// A short noun for diagnostics, e.g. "a `const`".
    pub fn describe(self) -> &'static str {
        match self {
            Self::FixedAddressVar { .. } => "a variable",
            Self::Const { .. } => "a `const`",
            Self::Function => "a function",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    /// Span of the declaring name, for "declared here" secondary labels.
    pub declaration: Span,
}

/// All symbols of one compilation unit.
#[derive(Debug, Default)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
    by_name: HashMap<String, SymbolId>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Declares `name`. On a duplicate, returns the existing symbol as `Err`
    /// so the caller can attach a "previously declared here" diagnostic.
    pub fn declare(
        &mut self,
        name: &str,
        kind: SymbolKind,
        declaration: Span,
    ) -> Result<SymbolId, SymbolId> {
        if let Some(&existing) = self.by_name.get(name) {
            return Err(existing);
        }
        let id = SymbolId(u32::try_from(self.symbols.len()).expect("too many symbols"));
        self.symbols.push(Symbol {
            name: name.to_owned(),
            kind,
            declaration,
        });
        self.by_name.insert(name.to_owned(), id);
        Ok(id)
    }

    pub fn lookup(&self, name: &str) -> Option<SymbolId> {
        self.by_name.get(name).copied()
    }

    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.index()]
    }

    pub fn name(&self, id: SymbolId) -> &str {
        &self.symbol(id).name
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

    #[test]
    fn declare_lookup_and_duplicate_detection() {
        let mut map = SourceMap::new();
        let file = map.add_file("t.k816", "var SCREEN = $0400\nvar SCREEN = $0401\n");
        let mut table = SymbolTable::new();

        let first = table
            .declare(
                "SCREEN",
                SymbolKind::FixedAddressVar { address: 0x0400 },
                Span::new(file, 4, 10),
            )
            .expect("first declaration succeeds");
        assert_eq!(table.lookup("SCREEN"), Some(first));
        assert_eq!(table.name(first), "SCREEN");
        assert_eq!(
            table.symbol(first).kind,
            SymbolKind::FixedAddressVar { address: 0x0400 }
        );

        let duplicate = table.declare(
            "SCREEN",
            SymbolKind::FixedAddressVar { address: 0x0401 },
            Span::new(file, 23, 29),
        );
        assert_eq!(duplicate, Err(first));
        assert_eq!(table.lookup("MISSING"), None);
    }
}
