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
//!
//! A variable may also carry a [`Layout`]: the packed field list of a
//! symbolic subscript declaration, with every field's offset already
//! computed. Layouts live in their own arena and are named by [`LayoutId`],
//! which is what lets a field nest another layout inside itself.

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

/// How wide one *element* of a variable's data is — the `:byte` / `:word` /
/// `:far` axis of a declaration.
///
/// Deliberately not [`AddressSize`]. The two are independent axes and
/// `doc/syntax-reference.md` ("Two orthogonal axes") is explicit about it:
/// the storage class says where a variable lives and how a plain *reference*
/// to it encodes, the type width says how wide one *datum* is. `var dp p:far`
/// is a 24-bit value living in the direct page, so collapsing the two into
/// one enum would make that declaration unrepresentable.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DataWidth {
    Byte,
    Word,
    Far,
}

impl DataWidth {
    /// The bytes one element occupies. Layouts are packed, so this is also
    /// the distance to the next field (`doc/symbolic-subscripts.md`,
    /// "Semantics").
    pub fn bytes(self) -> u32 {
        match self {
            Self::Byte => 1,
            Self::Word => 2,
            Self::Far => 3,
        }
    }
}

/// Identifies an interned [`Layout`].
///
/// Layouts are interned rather than stored inline in a [`SymbolKind`] for two
/// reasons: a nested field names its sub-layout by identifier, which is how
/// nesting stays a flat structure instead of a recursive one; and it keeps
/// `SymbolKind` [`Copy`], which the borrow patterns around the symbol table
/// depend on.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LayoutId(u32);

impl LayoutId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// The layouts one compilation unit declares.
///
/// A separate arena from the symbols, because a layout is not a symbol: a
/// nested field list has one without ever having a name.
#[derive(Debug, Default)]
pub struct Layouts {
    layouts: Vec<Layout>,
}

impl Layouts {
    /// Interns `layout`. Nested layouts are added before the layout that
    /// names them, so a [`FieldShape::Nested`] identifier always resolves.
    pub fn add(&mut self, layout: Layout) -> LayoutId {
        let id = LayoutId(u32::try_from(self.layouts.len()).expect("too many layouts"));
        self.layouts.push(layout);
        id
    }

    pub fn get(&self, id: LayoutId) -> &Layout {
        &self.layouts[id.index()]
    }
}

/// A packed field layout: what `var foo[ .lo :byte .hi :byte ]` declares.
///
/// Offsets are computed once, by the declaration pass, and stored — an access
/// reads them rather than re-deriving them, so the answer `foo.hi` gives in a
/// diagnostic and the answer it lowers to cannot drift apart. Layouts are
/// packed and sequential, with no padding.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Layout {
    pub fields: Vec<Field>,
    /// The bytes one element of this layout occupies, which is what
    /// `foo:sizeof` answers — *not* the total allocation of a `* count`
    /// declaration (`doc/symbolic-subscripts.md`, "Semantics").
    pub size: u32,
}

impl Layout {
    /// The field named `name`, which is written without its leading `.`.
    pub fn field(&self, name: &str) -> Option<&Field> {
        self.fields.iter().find(|field| field.name == name)
    }
}

/// One entry of a symbolic subscript field list.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Field {
    /// The name *without* the leading `.`, because that is the identity the
    /// two access spellings share: `foo.lo` and `foo[.lo]` name one field.
    pub name: String,
    /// Bytes from the base of the enclosing layout.
    pub offset: u32,
    pub shape: FieldShape,
    /// `[count]` — how many elements the field repeats, `1` when the
    /// declaration omits it. It is the extent a subscript is checked against.
    pub count: u32,
    /// Span of the declaring `.name`, for "declared here" secondary labels.
    pub declaration: Span,
}

impl Field {
    /// The bytes this field occupies, counting its repeats.
    pub fn size(&self, layouts: &Layouts) -> u32 {
        self.element_size(layouts) * self.count
    }

    /// The bytes *one* element occupies, which is the stride a subscript
    /// index multiplies.
    pub fn element_size(&self, layouts: &Layouts) -> u32 {
        match self.shape {
            FieldShape::Scalar(width) => width.bytes(),
            FieldShape::Nested(layout) => layouts.get(layout).size,
        }
    }
}

/// What one field holds: a datum of some width, or a nested layout.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FieldShape {
    /// `.name :word` — a scalar, or `:byte` when the declaration named no
    /// width and the symbol declared no default.
    Scalar(DataWidth),
    /// `.name[ … ]` — a nested field list, with its own offsets from the
    /// field's own base.
    Nested(LayoutId),
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
    Const { value: u32 },
    /// `var NAME [= addr]`. `address` is `None` for a linker-allocated
    /// variable, whose address only ld65 knows.
    Var {
        address: Option<u32>,
        size: AddressSize,
        /// The field layout `var NAME[ .field :word … ]` declares, or `None`
        /// for a variable that has no fields.
        layout: Option<LayoutId>,
    },
    /// `abstract var NAME[ … ]` — a field layout and no storage at all.
    ///
    /// It reserves nothing, emits nothing, and has no address, so it answers
    /// `:sizeof` and `:offsetof` and nothing else; using one where storage is
    /// required is [`crate::diagnostic::DiagnosticCode::ABSTRACT_HAS_NO_STORAGE`].
    /// That is why it is a kind of its own rather than a `Var` with no
    /// address: a linker-allocated `Var` also has no address *yet*, and the
    /// two must not be confused.
    AbstractVar { layout: LayoutId },
    /// A code label, `name:` or `.name:`.
    Label {
        scope: LabelScope,
        size: AddressSize,
    },
    /// A label inside a data block, including a block's own name.
    DataLabel { size: AddressSize },
    /// An `inline` parameter alias, scoped to the section that declares it.
    ///
    /// It stands for whatever the use site substitutes, which is why it has a
    /// kind of its own rather than being a `Const` with a placeholder value:
    /// nothing may fold it, and every stage that meets one has to carry it
    /// through to expansion intact.
    ///
    /// `immediate` is `#name` — the alias stands for a *value*. A bare
    /// identifier alias stands for a *place*, and takes
    /// [`AddressSize::Absolute`], because the address it will name is not
    /// known where the body is classified and an over-wide encoding costs a
    /// byte where an under-wide one is wrong.
    Alias { immediate: bool },
    /// An `inline` section: a body expanded at each use site.
    ///
    /// Not a [`SymbolKind::Function`] with a flag, because the two answer
    /// every structural question differently. An `inline` has no address, so
    /// it is not exported, not callable by `jsr`, not a jump target, and not
    /// a value any expression may fold. What it has is a body, and the only
    /// thing a use site may do with it is expand that body.
    Inline,
    /// A code section, `func` or `far func`.
    ///
    /// `size` carries the calling convention: a `far func` is
    /// [`AddressSize::Long`] and every other function is
    /// [`AddressSize::Absolute`]. The two are the same fact — a section
    /// reached by `jsl` and left by `rtl` is one whose address is 24 bits —
    /// so there is one field rather than a size and a redundant `is_far`
    /// that could disagree with it. Ask [`SymbolKind::is_far_function`]
    /// rather than matching the size, so the intent survives a reader.
    Function { size: AddressSize },
}

impl SymbolKind {
    /// The kind a section declaration produces, near or far.
    pub fn function(is_far: bool) -> Self {
        Self::Function {
            size: if is_far {
                AddressSize::Long
            } else {
                AddressSize::Absolute
            },
        }
    }

    /// True for a `far func`: called with `JSL`, returning with `RTL`.
    pub fn is_far_function(self) -> bool {
        matches!(
            self,
            Self::Function {
                size: AddressSize::Long
            }
        )
    }

    /// The address size of plain references to this symbol, or `None` when
    /// the symbol has no address at all.
    pub fn address_size(self) -> Option<AddressSize> {
        match self {
            Self::Const { .. } | Self::AbstractVar { .. } | Self::Inline => None,
            Self::Alias { immediate } => (!immediate).then_some(AddressSize::Absolute),
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
            | Self::AbstractVar { .. }
            | Self::Alias { .. }
            | Self::Inline
            | Self::Label { .. }
            | Self::DataLabel { .. }
            | Self::Function { .. } => None,
        }
    }

    /// True when the symbol names a memory location an operand may address.
    pub fn is_addressable(self) -> bool {
        match self {
            Self::Var { .. } | Self::Label { .. } | Self::DataLabel { .. } => true,
            // A place alias is exactly a stand-in for an address; a `#name`
            // one is a stand-in for a value and may not be stored to.
            Self::Alias { immediate } => !immediate,
            Self::Const { .. }
            | Self::AbstractVar { .. }
            | Self::Inline
            | Self::Function { .. } => false,
        }
    }

    /// A short noun for diagnostics, e.g. "a `const`".
    pub fn describe(self) -> &'static str {
        match self {
            Self::Const { .. } => "a `const`",
            Self::Var { .. } => "a variable",
            Self::AbstractVar { .. } => "an `abstract var`",
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
            Self::Inline => "an `inline`",
            Self::Alias { .. } => "an `inline` alias",
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
    layouts: Layouts,
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

    /// Interns a field layout, for a symbol that declares one.
    pub fn add_layout(&mut self, layout: Layout) -> LayoutId {
        self.layouts.add(layout)
    }

    pub fn layout(&self, id: LayoutId) -> &Layout {
        self.layouts.get(id)
    }

    /// The layout arena, for the field arithmetic that spans several fields
    /// at once — a nested field's size is its sub-layout's.
    pub fn layouts(&self) -> &Layouts {
        &self.layouts
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
            layout: None,
        }
    }

    /// `.lo :byte .hi :byte`, with the offsets a declaration pass computes.
    fn pair(span: Span) -> Layout {
        let field = |name: &str, offset: u32| Field {
            name: name.to_owned(),
            offset,
            shape: FieldShape::Scalar(DataWidth::Byte),
            count: 1,
            declaration: span,
        };
        Layout {
            fields: vec![field("lo", 0), field("hi", 1)],
            size: 2,
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
                size: AddressSize::Absolute,
                layout: None
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

    #[test]
    fn a_layout_answers_field_offsets_and_its_own_size() {
        let mut map = SourceMap::new();
        let file = map.add_file("t.k816", "var vec[ .lo :byte .hi :byte ] = $10\n");
        let span = Span::new(file, 9, 12);
        let mut table = SymbolTable::new();

        let layout = table.add_layout(pair(span));
        let vec = table
            .declare(
                "vec",
                SymbolKind::Var {
                    address: Some(0x10),
                    size: AddressSize::DirectPage,
                    layout: Some(layout),
                },
                span,
            )
            .unwrap();

        let SymbolKind::Var {
            layout: Some(declared),
            ..
        } = table.kind(vec)
        else {
            panic!("the declaration carries its layout");
        };
        let declared = table.layout(declared);
        assert_eq!(declared.size, 2, "`vec:sizeof` is one element's bytes");
        assert_eq!(declared.field("lo").map(|field| field.offset), Some(0));
        assert_eq!(declared.field("hi").map(|field| field.offset), Some(1));
        assert_eq!(
            declared.field(".hi").map(|field| field.offset),
            None,
            "the declaring dot is not part of the name; `vec.hi` and `vec[.hi]` \
             both look the field up as `hi`"
        );
    }

    #[test]
    fn a_nested_field_is_sized_by_the_layout_it_names() {
        let mut map = SourceMap::new();
        let file = map.add_file("t.k816", "var t[ .p[ .lo :byte .hi :byte ] ] = $20\n");
        let span = Span::new(file, 7, 9);
        let mut table = SymbolTable::new();

        let inner = table.add_layout(pair(span));
        let outer = Layout {
            fields: vec![Field {
                name: "p".to_owned(),
                offset: 0,
                shape: FieldShape::Nested(inner),
                count: 4,
                declaration: span,
            }],
            size: 8,
        };
        let field = &outer.fields[0];

        assert_eq!(
            field.element_size(table.layouts()),
            2,
            "one element is the nested layout's size, which is the subscript stride"
        );
        assert_eq!(field.size(table.layouts()), 8, "four of them, packed");
    }

    #[test]
    fn an_abstract_var_has_a_layout_and_nothing_else() {
        let mut map = SourceMap::new();
        let file = map.add_file("t.k816", "abstract var handler[ .lo :byte .hi :byte ]\n");
        let span = Span::new(file, 13, 20);
        let mut table = SymbolTable::new();

        let layout = table.add_layout(pair(span));
        let kind = SymbolKind::AbstractVar { layout };

        assert_eq!(table.layout(layout).size, 2, "it still answers `:sizeof`");
        assert_eq!(kind.address_size(), None, "no reference to it encodes");
        assert_eq!(kind.known_address(), None);
        assert!(
            !kind.is_addressable(),
            "an `abstract var` reserves nothing, so nothing addresses it"
        );
        assert!(
            !crate::linkage::always_exported(kind),
            "there is no place to export"
        );
        assert_eq!(kind.describe(), "an `abstract var`");
    }
}
