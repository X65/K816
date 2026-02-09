# HLA AST/IR Refactor Execution Plan

## Goal
Remove text-level HLA desugaring and replace it with:
1. Surface AST that preserves native + HLA syntax.
2. AST normalization pass for semantic equivalence (including condition shorthand normalization like `>=` vs `>= 0`).
3. IR-level expansion of HLA constructs into native instruction sequences.
4. Existing ISA/object/link emission flow unchanged in architecture.

## Execution Steps

### 1. Remove pre-parse text desugaring from pipeline
- Remove `desugar_hla_syntax(...)` from compile driver flow.
- Keep source map bound to original input text only.

### 2. Introduce first-class HLA AST nodes
- Extend `crates/core/src/ast/mod.rs` with:
  - `Item::NamedDataBlock(...)`.
  - `Stmt::Hla(...)` container and HLA statement variants.
  - HLA condition/test model with optional RHS.
  - Named-data entries supporting strings, bytes, directives, and converter commands.

### 3. Upgrade lexer for HLA grammar
- Add punctuation/operators required for HLA forms (`?`, `&`, `++`, relational operators, etc.).
- Preserve existing eval-fragment tokenization and literal behavior.

### 4. Parse HLA directly into AST
- Add parser rules for:
  - `x = #expr`.
  - `x++`.
  - `dest = a = rhs`.
  - `{ a&?SYMBOL } n-?` wait-loop statement.
  - `{` and `} <cond>` do/while statements.
  - `data <name> { ... }` named data blocks with mixed entry types.
- Keep existing native syntax parser behavior.

### 5. Add AST normalization pass
- New pass module to normalize HLA tests:
  - Fill omitted RHS with zero for relational conditions.
  - Canonical representation for equivalent condition forms.
- Wire pass into compile pipeline after eval expansion, before sema/lower.

### 6. Lower HLA AST to existing HIR ops
- Expand HLA statements in lowering:
  - `x = #expr` -> `ldx #expr`.
  - `x++` -> `inx`.
  - `dest = a = rhs` -> `lda rhs` + `sta dest`.
  - wait-loop -> local label + `lda sym` + branch.
  - do/while -> loop labels + cmp/branch sequence.
- Lower named data blocks to labels + byte/directive ops.

### 7. Extend ISA map for branch coverage used by normalized conditions
- Add required branch mnemonics used by lowered HLA conditions (`beq`, `bcc`, `bcs`, `bmi` as needed).

### 8. Update AST consumers
- Update formatter (`crates/fmt/src/print_ast.rs`) for new AST variants.
- Update eval expansion and sema traversal to include new nodes.

### 9. Delete legacy text desugar module
- Remove `crates/core/src/hla_syntax.rs` and module export/use sites.
- Replace legacy desugar tests with parser/normalizer/lower tests.

### 10. Validate
- Run targeted unit tests for parser/lower/eval/format modules.
- Run golden and CLI tests.
- Fix breakages until all tests pass.

## Acceptance Criteria
- No text rewrite pass before parser.
- HLA syntax is represented explicitly in AST.
- Condition shorthand equivalence is normalized in AST pass.
- HLA expansion happens in lowering to IR ops.
- Existing compile/link flows produce valid outputs.
