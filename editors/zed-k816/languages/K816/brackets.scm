; brackets.scm — the pairs Zed matches and colours.
;
; Zed-only: the grammar's own queries have no bracket file, because no other
; reader asks for one.  `tests/zed.rs` compiles it against the grammar, so a
; renamed token fails there rather than in the editor.

("(" @open ")" @close)
("[" @open "]" @close)
("{" @open "}" @close)
