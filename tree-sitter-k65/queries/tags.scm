; tags.scm — the definitions a code index should carry.
;
; Exactly the unit-level declarations the symbol table has, and nothing else:
; `tests/queries.rs` compares this query's captures with
; `SemanticModel.symbols`' unit-level entries over the corpus, so a
; declaration form added to the language and not added here is a failure.
;
; A local label is deliberately absent.  It is scoped to one section, so it is
; not something another file can navigate to, and a tag index that carried
; every `.spin:` in the tree would be a list of the same six names.

(func_declaration
  name: (identifier) @name) @definition.function

(evalfunc_declaration
  name: (identifier) @name) @definition.function

(var_entry
  name: (identifier) @name) @definition.variable

(const_entry
  name: (identifier) @name) @definition.constant

(binary_declaration
  name: (identifier) @name) @definition.variable

(data_block
  name: (identifier) @name) @definition.variable

(data_label
  name: (identifier) @name) @definition.variable

; A unit-level label: `name:` without a leading dot.
(label
  name: (identifier) @name) @definition.variable

; The call sites a code index links definitions to.
(call_statement
  target: (identifier) @name) @reference.call

(bare_call
  target: (identifier) @name) @reference.call
