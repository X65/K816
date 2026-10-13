; indents.scm — where a line's indentation comes from, in Zed's dialect.
;
; Derived from `tree-sitter-k65/queries/indents.scm`, which is written for
; readers that close an indent with a separate `"}" @outdent` pattern.  Zed
; closes a range with the closer captured `@end` inside the same pattern —
; without it the range runs to the end of the node and the line holding the
; `}` stays indented.  Two constructs do not own their closer: `code { }` and
; `nocross { }` wrap a body node that does, so the `@end` is the body's.
;
; `tests/zed.rs` holds this file to the grammar's twice: the constructs named
; here are `format::INDENTING`, and over the corpus the `@indent` captures
; cover exactly the spans the grammar's query covers.

(block "}" @end) @indent
(data_body "}" @end) @indent
(code_subblock body: (block "}" @end)) @indent
(field_list "]" @end) @indent
(eval_bracket "]" @end) @indent
(eval_statement_block "}" @end) @indent
(nocross_data_group body: (data_body "}" @end)) @indent
