/*****************************************************************************/
/*                                                                           */
/*                                 layout.rs                                 */
/*                                                                           */
/*            How much a declaration reserves, and where it goes             */
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

//! Variable allocation: how many bytes a declaration reserves, how far apart
//! its elements are, and how much of the direct page a unit has asked for.
//!
//! K816 does not choose addresses — ld65 does, and
//! `plans/feature-manifest.md` §3 says so. What is decided here is *size*,
//! which is a property of the declaration and of nothing else, and which two
//! passes need: `semantic.rs` to check an access against the extent it
//! declares, and `lower.rs` to emit the reservation.
//!
//! The direct page is the one place a size becomes a *limit*. It is 256
//! bytes and no linker configuration makes it bigger, so a unit asking for
//! more is wrong at the declaration rather than at the link — that is
//! sub-plan E's decision 13, and [`DirectPage`] is the accounting it needs.

use crate::ast::TypeWidthKind;
use crate::source::Span;
use crate::symbol::{DataWidth, Field, FieldShape, Layout, LayoutId, Layouts};

/// The bytes the direct page has, which is what makes its pool a pool.
pub const DIRECT_PAGE_BYTES: u32 = 256;

/// What one `var` declaration reserves.
///
/// `element` is the stride between repeat elements and the answer
/// `NAME:sizeof` gives; `total` is what the reservation is for. They differ
/// by the *count* and by nothing else, because
/// `&&NAME[i] == &&NAME + i * NAME:sizeof` has to stay true whatever the
/// count is (`doc/syntax-reference.md`, "Repeat element accessors").
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Allocation {
    /// The bytes one element occupies — the datum, times the raw span.
    ///
    /// The span is part of the element and the count is not, which is the
    /// document's own division: `* count` "repeats the computed element
    /// layout", and `[N]` is part of that layout.
    pub element: u32,
    /// The bytes the whole declaration occupies: elements times the count.
    pub total: u32,
}

/// The declaration axis a `:byte` / `:word` / `:far` suffix sets.
///
/// The same three spellings appear at a use site as a typed view, and mean
/// the same three widths there — which is why the mapping is here rather
/// than in whichever pass happened to need it first.
pub fn data_width(kind: TypeWidthKind) -> DataWidth {
    match kind {
        TypeWidthKind::Byte => DataWidth::Byte,
        TypeWidthKind::Word => DataWidth::Word,
        TypeWidthKind::Far => DataWidth::Far,
    }
}

/// The bytes one *datum* of a declaration occupies, before either multiplier.
///
/// A field list answers it, because a layout knows its own packed size.
/// Otherwise the typed width does, and an untyped declaration is one byte —
/// `var arr[10]` reserves ten bytes, which is the document's own wording
/// ("N elements of the declared width, or N bytes when no width is
/// written"). That default belongs *here* and not on the symbol: a variable
/// that declared no width has no width to validate an access against, and
/// defaulting it there would invent a claim the author never made.
pub fn datum_size(width: Option<DataWidth>, layout: Option<LayoutId>, layouts: &Layouts) -> u32 {
    match layout {
        Some(id) => layouts.get(id).size,
        None => width.map_or(1, DataWidth::bytes),
    }
}

/// What `var NAME[span] * count` reserves, given the size of one datum.
///
/// Both multipliers are plain repetition and a declaration carrying both
/// reserves the product, but they sit on opposite sides of the *element*
/// boundary. `[N]` is inside it: `var pairs:word[2] * 3` has three elements
/// of two words each, so its stride is four bytes and not two. `* N` is
/// outside it, and is also the only one of the two that makes `NAME[i]`
/// element access rather than byte arithmetic.
pub fn allocation(datum: u32, span: Option<u32>, count: Option<u32>) -> Allocation {
    let element = datum.saturating_mul(span.unwrap_or(1));
    Allocation {
        element,
        total: element.saturating_mul(count.unwrap_or(1)),
    }
}

/// The width one field of a symbolic subscript list holds.
///
/// Three answers in order of nearness: what the field itself wrote, then the
/// default the enclosing declaration set (`var baz:byte[ .a .b ]`), and
/// `:byte` when neither said anything. The middle one is what makes a
/// declaration-level suffix mean *default field width* on a declaration that
/// has fields, rather than the datum width it means on one that has none.
pub fn field_width(declared: Option<TypeWidthKind>, default: Option<DataWidth>) -> DataWidth {
    declared
        .map(data_width)
        .or(default)
        .unwrap_or(DataWidth::Byte)
}

/// A field list being packed, one field at a time.
///
/// Layouts are packed and sequential with no padding, so a field's offset is
/// the sum of the sizes before it and the layout's own size is the sum of
/// them all (`doc/symbolic-subscripts.md`, "Semantics"). Offsets are computed
/// here, once, and stored on the [`Field`]: an access reads them rather than
/// re-deriving them, so what a diagnostic says `.hi` is at and what the
/// operand adds cannot drift apart.
///
/// It accumulates rather than being one `pack(fields)` call because a nested
/// list has to be interned before the field naming it can be sized, and the
/// recursion that does that also owns the complaints — a duplicate name, a
/// count that does not fold — which are the declaring pass's and not this
/// module's.
#[derive(Debug, Default)]
pub struct Packing {
    fields: Vec<Field>,
    size: u32,
}

impl Packing {
    /// The field already packed under `name`, which is what makes a repeated
    /// `.name` a diagnostic instead of a second entry nothing can reach.
    pub fn declared(&self, name: &str) -> Option<&Field> {
        self.fields.iter().find(|field| field.name == name)
    }

    /// Appends `name` at the current offset and advances past it.
    ///
    /// `count` is the field's `[N]`, which is *not* the raw span a top-level
    /// declaration's `[N]` is: `doc/symbolic-subscripts.md` gives an indexed
    /// field element the address `field_base + index * element_width`, so a
    /// field's bracket makes elements and is checked, where a variable's
    /// makes a span and is not.
    pub fn push(
        &mut self,
        name: &str,
        shape: FieldShape,
        count: u32,
        declaration: Span,
        layouts: &Layouts,
    ) {
        let field = Field {
            name: name.to_owned(),
            offset: self.size,
            shape,
            count,
            declaration,
        };
        self.size = self.size.saturating_add(field.size(layouts));
        self.fields.push(field);
    }

    /// The packed layout, ready to intern.
    pub fn finish(self) -> Layout {
        Layout {
            fields: self.fields,
            size: self.size,
        }
    }
}

/// One unit's claim on the 256-byte direct page.
///
/// Two kinds of claim go in, and they are counted differently because they
/// behave differently. An *allocated* `var dp` asks for a slot and gets
/// whichever one is free, so every request adds. A *pinned* one names its
/// offset, and the document allows two pinned variables to name the same one
/// — "useful for typed views over the same DP slot" — so pinned claims are a
/// union of byte ranges rather than a sum. What the allocator has left is
/// the page minus both.
#[derive(Debug, Default)]
pub struct DirectPage {
    /// The bytes allocated slots have asked for, summed.
    requested: u32,
    /// The byte offsets pinned slots cover, as a set, so an overlap is
    /// counted once.
    pinned: std::collections::BTreeSet<u32>,
}

impl DirectPage {
    /// Records an allocated slot of `bytes` and answers the pool's total
    /// claim afterwards.
    pub fn request(&mut self, bytes: u32) -> u32 {
        self.requested = self.requested.saturating_add(bytes);
        self.claimed()
    }

    /// Records a slot pinned at `address` covering `bytes`, and answers the
    /// pool's total claim afterwards.
    ///
    /// A pinned slot that runs off the end of the page is left to the caller:
    /// the claim it reports is the part inside the page, and the part outside
    /// is a different complaint about the same declaration.
    pub fn pin(&mut self, address: u32, bytes: u32) -> u32 {
        for offset in address..address.saturating_add(bytes) {
            if offset < DIRECT_PAGE_BYTES {
                self.pinned.insert(offset);
            }
        }
        self.claimed()
    }

    /// The bytes of the page this unit has claimed, allocated and pinned.
    pub fn claimed(&self) -> u32 {
        self.requested
            .saturating_add(u32::try_from(self.pinned.len()).unwrap_or(u32::MAX))
    }
}

/// True when a pinned direct-page slot does not fit inside the page.
///
/// The address itself must be a direct-page one, and so must its last byte:
/// `var dp p:far = $FE` is three bytes starting two from the end, and the
/// third is not in the page at all.
pub fn pinned_slot_overflows(address: u32, bytes: u32) -> bool {
    address.saturating_add(bytes.max(1)) > DIRECT_PAGE_BYTES
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_untyped_declaration_reserves_bytes_and_a_typed_one_reserves_elements() {
        let layouts = Layouts::default();
        assert_eq!(datum_size(None, None, &layouts), 1);
        assert_eq!(datum_size(Some(DataWidth::Word), None, &layouts), 2);
        assert_eq!(datum_size(Some(DataWidth::Far), None, &layouts), 3);

        // `var arr[10]` — ten bytes. `var arr:word[10]` — ten words.
        assert_eq!(allocation(1, Some(10), None).total, 10);
        assert_eq!(allocation(2, Some(10), None).total, 20);
        // `var simple:byte * 32` — thirty-two one-byte elements.
        assert_eq!(allocation(1, None, Some(32)).total, 32);
        assert_eq!(allocation(1, None, Some(32)).element, 1);
        // `var pairs:word[2] * 3` — three elements, and the span is inside
        // the element, so the stride is four and the total is twelve.
        assert_eq!(allocation(2, Some(2), Some(3)).element, 4);
        assert_eq!(allocation(2, Some(2), Some(3)).total, 12);
    }

    #[test]
    fn allocated_slots_add_and_pinned_ones_only_cover() {
        let mut page = DirectPage::default();
        assert_eq!(page.request(4), 4);
        assert_eq!(page.request(2), 6);
        // Two pinned views over the same slot claim it once, which is what
        // makes the documented overlap free rather than double-counted.
        assert_eq!(page.pin(0x80, 2), 8);
        assert_eq!(page.pin(0x80, 2), 8);
        // A third that only partly overlaps adds the part that is new.
        assert_eq!(page.pin(0x81, 2), 9);
    }

    #[test]
    fn a_field_takes_the_nearest_width_that_was_written() {
        // The field's own, then the declaration's default, then `:byte`.
        assert_eq!(
            field_width(Some(TypeWidthKind::Word), Some(DataWidth::Far)),
            DataWidth::Word
        );
        assert_eq!(field_width(None, Some(DataWidth::Far)), DataWidth::Far);
        assert_eq!(field_width(None, None), DataWidth::Byte);
    }

    #[test]
    fn packing_is_sequential_and_a_count_multiplies_the_field() {
        let layouts = Layouts::default();
        let mut map = crate::source::SourceMap::new();
        let file = map.add_file("t.k816", "var TASKS[ .sp :word .state .str[5] ]\n");
        let span = Span::new(file, 0, 0);
        let mut packing = Packing::default();
        packing.push("sp", FieldShape::Scalar(DataWidth::Word), 1, span, &layouts);
        packing.push(
            "state",
            FieldShape::Scalar(DataWidth::Byte),
            1,
            span,
            &layouts,
        );
        packing.push(
            "str",
            FieldShape::Scalar(DataWidth::Byte),
            5,
            span,
            &layouts,
        );
        assert_eq!(packing.declared("state").expect("packed").offset, 2);
        assert!(packing.declared("nope").is_none());

        let layout = packing.finish();
        assert_eq!(layout.field("str").expect("packed").offset, 3);
        assert_eq!(layout.size, 8, "2 + 1 + 5, packed and in order");
    }

    #[test]
    fn a_pinned_slot_has_to_fit_the_page_it_names() {
        assert!(!pinned_slot_overflows(0xFF, 1));
        assert!(pinned_slot_overflows(0xFF, 2));
        assert!(pinned_slot_overflows(0x100, 1));
        // A zero-byte slot still occupies the offset it names.
        assert!(pinned_slot_overflows(0x100, 0));
    }
}
