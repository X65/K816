/*****************************************************************************/
/*                                                                           */
/*                                actions.rs                                 */
/*                                                                           */
/*        Every structured fix, offered generically and filtered by          */
/*                               nothing                                     */
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

//! Code actions: every [`Fix`] a diagnostic carries, converted generically.
//!
//! No fix is special-cased and none is filtered. That is decision 5, and it
//! is the reason this module is short: `Fix` is already a titled set of
//! `TextEdit`s with an applicability, so the conversion is mechanical and the
//! *content* of every action is the compiler's.
//!
//! What H adds is not a fix but a property, and it lives in
//! `tests/diagnostics.rs` where it belongs: applying a `MachineApplicable`
//! fix and re-analyzing removes the diagnostic it was attached to. Nothing
//! tested that before, so a fix could have been offered for years while
//! producing source the compiler refused differently.

use crate::diagnostic::{Applicability, Diagnostic, Fix};
use crate::source::Span;

/// One offered action: a fix, and the diagnostic it belongs to.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Action<'a> {
    pub fix: &'a Fix,
    pub diagnostic: &'a Diagnostic,
    /// True for a `MachineApplicable` fix, which an editor may apply without
    /// asking.
    pub preferred: bool,
}

/// Every fix attached to a diagnostic whose primary span meets `range`.
///
/// "Meets" rather than "is inside": an editor sends the selection, which is
/// usually a caret, and a diagnostic underlining a whole statement should
/// still offer its fix when the caret is anywhere in it.
pub fn actions<'a>(diagnostics: &'a [Diagnostic], range: Span) -> Vec<Action<'a>> {
    diagnostics
        .iter()
        .filter(|diagnostic| overlaps(diagnostic.primary.span, range))
        .flat_map(|diagnostic| {
            diagnostic.fixes.iter().map(move |fix| Action {
                fix,
                diagnostic,
                preferred: fix.applicability == Applicability::MachineApplicable,
            })
        })
        .collect()
}

/// Whether two spans of the same file share any position.
///
/// A zero-width range — a caret — meets a span it sits inside or at either
/// end of, so a fix is offered with the cursor immediately after the
/// construct it is about.
fn overlaps(span: Span, range: Span) -> bool {
    span.file == range.file && span.start <= range.end && range.start <= span.end
}
