; indents.scm — where a line's indentation comes from.
;
; One rule, and it is the formatter's: a line is indented once per
; brace-delimited construct it is inside (`plans/07-lsp-tooling.md`
; decision 4).  `tests/queries.rs` gates the two against each other over the
; corpus — the depth `src/parser/format.rs` assigns each line has to equal the
; count this query yields for it — so the file an editor reads and the
; formatter `k816 fmt` runs cannot drift apart.
;
; Indentation is four spaces.  The corpus has two conventions — the goldens
; and `examples/fullstack` use four, the K65 fixtures and the atari-xl
; examples use two, which is the original compiler's style — and four is what
; the formatter writes.

; Every construct whose body is braced indents its contents.
(block) @indent
(data_body) @indent
(code_subblock) @indent
(field_list) @indent
(eval_bracket) @indent
(eval_statement_block) @indent
(nocross_data_group) @indent

; The closing brace goes back out to the enclosing level.
"}" @outdent
"]" @outdent
")" @outdent
