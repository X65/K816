# Syntax Fixture Matrix

These `syntax-*` fixtures prioritize legacy K65 surface syntax as documented in:

- `docs/legacy/syntax.md`
- `docs/legacy/instructions.md`
- `docs/legacy/examples.md`

Key focus areas:

- one-letter/symbolic operator forms (`a+...`, `mem=a`, `c+?{...}`, `*`)
- legacy feature naming (`bank` in legacy docs)
- data/evaluator/control-flow shorthand patterns from examples

Primary operator-coverage fixture:

- `syntax-one-letter-operators-legacy`
- `syntax-wait-loop-bit`
- `syntax-wait-loop-lda`

Fixtures are input-heavy by design. Some are intentionally ahead of implementation.

Compatibility note for this codebase:

- `segment` is the preferred keyword in k816.
- `syntax-bank-deprecated` keeps explicit coverage of legacy `bank` compatibility, deprecation warnings, and bank-switching usage.
