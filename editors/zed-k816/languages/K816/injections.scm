; injections.scm — deliberately almost empty, and this comment is why.
;
; An injection tells an editor that a region of one language is written in
; another.  K816 embeds no other language: a `data` block holds bytes, a
; string holds bytes, `binary "file"` names a path the assembler resolves, and
; the compile-time evaluator's `[ … ]` brackets are K816's own grammar rather
; than a guest one — `eval_bracket` and everything under it is parsed by this
; same parser, so there is nothing to hand over.
;
; The file exists rather than being absent because an editor that looks for it
; and does not find one cannot tell "no injections" from "not written yet",
; and because `tests/queries.rs` compiles all five.
;
; The one real entry is for the *other* direction: a Markdown or host-language
; fence tagged `k65` or `k816` is K816, which is what `tree-sitter.json`'s
; `injection-regex` already declares for editors that read it from there.

; A block comment whose content is a documentation comment stays plain text;
; nothing in K816 injects a language into a comment.
