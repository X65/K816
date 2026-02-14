# Syntax Fixture Matrix

These `syntax-*` fixtures cover K65 HLA surface syntax as documented in `vendor/src/compiler_src.cpp` and the K65 documentation.

Key focus areas:

- one-letter/symbolic operator forms (`a+...`, `mem=a`, `c+?{...}`, `*`)
- data/evaluator/control-flow shorthand patterns from examples
- const declarations and compile-time symbol resolution

Primary operator-coverage fixtures:

- `syntax-one-letter-operators`
- `syntax-wait-loop-bit`
- `syntax-wait-loop-lda`
- `syntax-postfix-le-efficiency-warning`
- `syntax-postfix-gt-efficiency-warning`
- `syntax-data-address-byte-operators`
- `syntax-data-address-byte-main-symbol`
- `syntax-doc-instructions-indexed-y`
- `syntax-c-addrmodes-indirect-xy`
- `syntax-const-declarations`

Fixtures are input-heavy by design. Some are intentionally ahead of implementation.
