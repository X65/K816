; outline.scm — what Zed's outline panel and symbol picker list.
;
; Derived from `tree-sitter-k65/queries/tags.scm`: the same unit-level
; definitions, and only those.  `tests/zed.rs` holds the two together over
; the corpus — the names here are tags.scm's names and the items its
; definitions — so a declaration form added to the language and to tags.scm
; and not here fails.
;
; `@context` is the keyword that says what a name is, where the item has
; one.  A `var` or `const` entry has none of its own: the keyword belongs to
; the declaration, which may list several entries.  Neither does an `extern`,
; whose keyword belongs to the wrapper around the declaration.

(func_declaration
  far: (far_marker)? @context
  kind: _ @context
  name: (identifier) @name) @item

; `main { … }`: the entry point the original K65 wrote with no keyword.
(func_declaration
  far: (far_marker)? @context
  !kind
  name: (identifier) @name) @item

(evalfunc_declaration "evalfunc" @context name: (identifier) @name) @item

(var_entry name: (identifier) @name) @item

(const_entry name: (identifier) @name) @item

(binary_declaration "binary" @context name: (identifier) @name) @item

(data_block "data" @context name: (identifier) @name) @item

(data_label name: (identifier) @name) @item

(extern_func far: (far_marker)? @context "func" @context name: (identifier) @name) @item

(extern_var "var" @context name: (identifier) @name) @item

(extern_const "const" @context name: (identifier) @name) @item

; A unit-level label: `name:` without a leading dot.
(label name: (identifier) @name) @item
