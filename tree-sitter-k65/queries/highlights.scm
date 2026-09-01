; highlights.scm — the K65/K816 highlighting, in the format editors read
; directly and the one the language server derives its semantic tokens from.
;
; There are two formats and no third (`plans/07-lsp-tooling.md` decision 6):
; editors that consume Tree-sitter queries — Helix, Zed, Neovim, Emacs —
; highlight from this file, and editors that do not are served
; `textDocument/semanticTokens`, which the server answers by running *this*
; query and mapping each capture onto its legend.  No TextMate grammar is
; written or shipped.
;
; The capture names are the standard Tree-sitter ones, so a theme written for
; any other language applies here.  They are also a closed set: every name
; below is in the server's token legend, and `tests/queries.rs` fails if one
; is not.
;
; Later patterns win in Tree-sitter's highlighter, so the specific ones come
; after the general ones.

; ---------------------------------------------------------------------------
; Comments, literals, and punctuation
; ---------------------------------------------------------------------------

[(line_comment) (block_comment)] @comment

(string) @string
(char_literal) @string
[(number) (float)] @number

[
  "(" ")" "[" "]" "{" "}" "," ";" ":" "->" ".."
] @punctuation

; ---------------------------------------------------------------------------
; Operators
; ---------------------------------------------------------------------------

[
  "=" "+" "-" "*" "/" "|" "^" "~" "?" "!" "%" "<" ">" "<<" ">>" "<<<" ">>>"
  "==" "!=" "<=" ">=" "++" "--" "&?" "&=" "+=" "-=" "*=" "/=" "|=" "^="
  "%=" "<<=" ">>=" "!!" "??" "?<" "?>" "&&" "||"
] @operator

; The markers that say what an operand *is* rather than what it computes, and
; which are the same tokens the evaluator uses for its logical operators.
; Scoped to the node that makes them markers, and written after the general
; operator list so the more specific pattern wins.
(immediate "#" @punctuation.special)
(address_of_expression operator: _ @punctuation.special)
(address_and_expression operator: _ @punctuation.special)

; ---------------------------------------------------------------------------
; Keywords
; ---------------------------------------------------------------------------

[
  "var" "const" "segment" "abstract" "binary" "evalfunc" "data" "eval"
  "evaluator" "charset" "align" "address" "nocross" "code" "image" "bank"
] @keyword

["func" "naked" "inline" "far"] @keyword.function

["return" "return_i" "goto" "call" "repeat"] @keyword.return

; `break` is a statement node rather than a token of its own.
(break_statement) @keyword.return

["else" "always" "never" "if" "while" "for"] @keyword.control

; A width annotation is a type: it says how wide the register is, which is
; the only type the language has.
(width_annotation) @type
(type_suffix) @type
["byte" "word" "sizeof" "offsetof"] @type

; ---------------------------------------------------------------------------
; The machine
; ---------------------------------------------------------------------------

(mnemonic) @function.builtin
(register) @variable.builtin
(index_register) @variable.builtin
(flag_name) @variable.builtin
(storage_class) @keyword
(address_encoding) @keyword

; ---------------------------------------------------------------------------
; Names
; ---------------------------------------------------------------------------

; The general case: an identifier is a variable until something more specific
; below says otherwise, and the server refines what is left through the
; symbol table.
(identifier) @variable
(local_identifier) @label
(field_name) @property

; Declarations.
(var_entry name: (identifier) @variable)
(const_entry name: (identifier) @constant)
(field_declaration name: (local_identifier) @property)
(func_declaration name: (identifier) @function)
(evalfunc_declaration name: (identifier) @function)
(binary_declaration name: (identifier) @variable)
(data_block name: (identifier) @label)
(data_label name: (identifier) @label)
(segment_declaration name: (identifier) @namespace)
(label name: (identifier) @label)
(label name: (local_identifier) @label)

; Uses that the grammar itself can classify.
(call_statement target: (identifier) @function)
(bare_call target: (identifier) @function)
(eval_call name: (identifier) @function)
(eval_method_call method: (field_name) @function)
(eval_member field: (field_name) @function)

; ---------------------------------------------------------------------------
; The preprocessor and the compile-time evaluator
; ---------------------------------------------------------------------------

(preprocessor_directive) @keyword.directive
(directive_name) @keyword.directive
(eval_pi) @constant.builtin
(reserved_function) @function.builtin
