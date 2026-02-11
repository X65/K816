# Syntax Fixture Matrix

These `syntax-*` fixtures cover K65 HLA surface syntax as documented in:

- `docs/legacy/syntax.md`
- `docs/legacy/instructions.md`
- `docs/legacy/examples.md`
- `vendor/doc/docs/instructions.md`
- `vendor/src/compiler_src.cpp` (`ADDRMODES`: `AM_VAR_Y`, `AM_IND`, `AM_IND_X`, `AM_IND_Y`)

Key focus areas:

- one-letter/symbolic operator forms (`a+...`, `mem=a`, `c+?{...}`, `*`)
- deprecated feature naming (`bank` in old docs)
- data/evaluator/control-flow shorthand patterns from examples

Primary operator-coverage fixture:

- `syntax-one-letter-operators-legacy`
- `syntax-wait-loop-bit`
- `syntax-wait-loop-lda`
- `syntax-postfix-le-efficiency-warning`
- `syntax-postfix-gt-efficiency-warning`
- `syntax-data-address-byte-operators`
- `syntax-data-address-byte-main-symbol`
- `syntax-doc-instructions-indexed-y`
- `syntax-c-addrmodes-indirect-xy`

Fixtures are input-heavy by design. Some are intentionally ahead of implementation.

Compatibility note for this codebase:

- `segment` is the preferred keyword in k816.
- `syntax-bank-deprecated` keeps explicit coverage of `bank` compatibility, deprecation warnings, and bank-switching usage.
