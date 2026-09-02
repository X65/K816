; locals.scm — the scopes and the bindings K65/K816 has.
;
; Two scopes and no more: the compilation unit, and a code section.  A local
; label (`.name:`) and an `inline` alias live in a section's scope; every
; other declaration is unit-level.  That is the same rule `symbol.rs` states
; with `Symbol.scope`, and it is why an editor that reads this file resolves a
; `.spin` to the `.spin:` of the same function and not the one next door.
;
; A scope is `@local.scope`, a binding `@local.definition`, and a use
; `@local.reference` — the standard Tree-sitter names, so a plugin written for
; another language works here.

; ---------------------------------------------------------------------------
; Scopes
; ---------------------------------------------------------------------------

(source_file) @local.scope
(func_declaration) @local.scope
(evalfunc_declaration) @local.scope

; ---------------------------------------------------------------------------
; Definitions
; ---------------------------------------------------------------------------

(var_entry name: (identifier) @local.definition.var)
(const_entry name: (identifier) @local.definition.constant)
(func_declaration name: (identifier) @local.definition.function)
(evalfunc_declaration name: (identifier) @local.definition.function)
(binary_declaration name: (identifier) @local.definition.var)
(data_block name: (identifier) @local.definition.var)
(data_label name: (identifier) @local.definition.var)
(extern_func name: (identifier) @local.definition.function)
(extern_var name: (identifier) @local.definition.var)
(extern_const name: (identifier) @local.definition.constant)
(field_declaration name: (local_identifier) @local.definition.field)
(label name: (identifier) @local.definition)
(label name: (local_identifier) @local.definition)

; An `inline`'s aliases: bound at each use site, and named in the contract.
(contract_parameter (identifier) @local.definition.parameter)
(evalfunc_declaration parameter: (identifier) @local.definition.parameter)

; `for x = A..B eval [ … ]` binds its loop variable for the body.
(for_eval variable: (identifier) @local.definition.parameter)

; ---------------------------------------------------------------------------
; References
; ---------------------------------------------------------------------------

(identifier) @local.reference
(local_identifier) @local.reference
