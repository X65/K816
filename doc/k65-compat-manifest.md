# The K65 compatibility manifest

**What this is.** One row per rule of the original K65 compiler's grammar,
plus one row per K816 construct that is not in that grammar. Every row carries
a status, the fixture that proves it, and — where the status is not
`compatible` — the reason.

`doc/k65-compat.md` asks for "its own manifest"; this is it, and it is sub-plan
G's primary artifact. It lives in `doc/` rather than beside the sub-plan
because the suite reads it — `plans/` is not tracked, and a test that depends
on an untracked file fails on a clean checkout — and because what it classifies
is the language, which is `doc/`'s subject. `tests/compat.rs` reads this file: every row's fixture has to
exist, every fixture's row has to be here, and the line ranges have to tile the
grammar with nothing dropped and nothing counted twice.

**The inventory is the grammar, not the documentation.** The rows below walk
`src/compiler.inc` of the original K65 checkout — 515 lines, the whole
language — because the original's documentation is partial (`doc/k65/syntax.md`
never mentions `evalfunc`, `bank` gets one line, the illegal-opcode forms
appear nowhere) and its grammar is complete by construction. A row per rule is
checkable; a row per documented feature is not. The `Lines` column is that
file's line numbers, so the walk can be re-checked against it.

**The ruling every row is decided by** (`plans/06-compatibility.md` decision
0): K816 is backwards compatible with K65 wherever compatibility is cheap for
K816; where it is not, K816 recognises the K65 syntax as a named node and
errors out with a specific diagnostic — never a generic parse error, and never
a K65 spelling given a K816 meaning except the one recorded exception
(decision 11). "Cheap" is an implementation judgment made per row and written
into the `Reason` column: a grammar alias, a lowering the existing machinery
already has, or an interpreter feature the existing interpreter already
supports is cheap; a decoder, a second linker, section mutation, or a semantics
the 65816 cannot realize is not.

## The statuses

| Status | Meaning |
| --- | --- |
| `compatible` | Implemented with the original's semantics; the oracle and K816 agree at the bytes, or at the instruction sequence where layout differs. |
| `intentionally-changed` | K816 does something else on purpose, the difference is documented, and `--strict-k65` reports it. |
| `legacy-defective` | The original (or the legacy K816) did something its own documentation contradicts; K816 does what the documentation says, and the row says what the old behaviour was. |
| `backend-different` | Same behaviour, different bytes or layout, because ca65/ld65 own encoding and placement. Compared at the instruction sequence. |
| `K816-extension` | Not in the original grammar. `--strict-k65` rejects it. |
| `open` | No decision yet. The task named in the reason settles it. **G10's done criterion is that no row reads `open`.** |

`reserved` is deliberately absent: a row that is still reserved is a row with
no decision, and `open` says the same thing while naming who closes it.

## Status of the walk

Written in G0 on 2026-08-21. Statuses are filled where a decision of
`plans/06-compatibility.md` settles them and `open` where a later task does.
Where the `Reason` column reads that something was "measured", the evidence is
an oracle golden under `tests/compat/k65/`, produced by the original K65
compiler at the revision `tests/compat/oracle.lock` pins.
Fixtures under `tests/compat/` are named without that prefix; a fixture under
`tests/golden/` is named with its `golden/` one. A `Fixture` of `—` means the
fixture is the task named in the `Reason` column's to write, and `tests/compat.rs`
only checks the fixtures that are named. G10's done criterion closes both
columns: no row reads `open`, and no fixture reads `—`.

## The original grammar, rule by rule

| Row | Lines | Construct | Status | Fixture | Reason |
| --- | --- | --- | --- | --- | --- |
| `syntax-toplist` | 2, 4-5 | `Syntax -> TopList`, `TopList -> TopElem \| TopList TopElem` | compatible | k65/eval-sees-consts | A source is a sequence of top-level elements. K816's `source_file` is the same rule. |
| `topelem-empty` | 29 | `TopElem -> ;` | compatible | — | A stray semicolon at top level is nothing, and K816 already accepts one. The fixture is G2's. |
| `topelem-preprocessor` | 7-10 | `TopElem -> PPIf TopList TopListPPElse PPEndif` | open | legacy-k816/syntax-preprocessor-top-level | Structural regions at top level. G5 implements them and sets the status; see `ppif-directives`. |
| `topelem-pperror` | 12, 20-21 | `TopElem -> PPError`, `#error String`, `#warn String` | open | legacy-k816/syntax-preprocessor-top-level | `PREPROCESSOR_ERROR` and `PREPROCESSOR_WARN` carry the message. G5. |
| `ppif-directives` | 15-18 | `#if`, `#else`, `#elif`, `#endif` | open | legacy-k816/syntax-preprocessor-sequence-and-data | The original disables the *semantic action* while parsing on (`compiler.inc:509`), so a dead region still has to parse. K816 parses everything anyway, so the rule costs nothing; what it buys is the `#elif` state machine of `compiler_src.cpp:1025`. G5. |
| `ppifnumber-condition` | 23-25 | `PPIfNumber -> Number \| AllIdent \| ! PPIfNumber` | open | k65/preprocessor-undefined-name | Four condition forms, of which K816's grammar has none: the condition is a K65 `_expression` today, so `#if !0` is `K816-E0001`. An undeclared name is an error in the original, measured — "Undeclared eval variable" — which is decision 9's rule. G1 adds the `!`, G5 evaluates it. |
| `topelem-eval-block` | 31 | `TopElem -> [ EvalCode ]` | open | legacy-k816/syntax-data-for-eval-range | A top-level evaluator block, run in source order, sharing one environment with every bracket. G4. |
| `topelem-var` | 33-35 | `TopElem -> var VarList`, `VarList -> Var \| VarList , Var` | open | k65/declaration-comma-lists | Comma lists declare consecutive variables. Cheap: a repetition in the grammar and a loop in `semantic.rs`. G2 (decision 13). |
| `var-declaration` | 36-39 | `Var -> Ident [BracketNumber] [= Number]` | open | k65/declaration-comma-lists | `= Number` is an **address**, not an initial value, and the allocator places an unaddressed variable right after the previous one. `BracketNumber` is the array size. K816 reads all three the same way; what is open is the comma list above and the marker below. G2. |
| `var-verbose-marker` | 40-41 | `Verbose -> <empty> \| ?` | open | k65/declaration-comma-lists | The `?` prints the variable's address at compile time and emits nothing (`compiler_src.cpp:446`); the golden shows it moves no byte. K816 accepts and ignores it — cheap, one optional token. G2 (decision 13). |
| `topelem-const` | 44-48 | `TopElem -> const ConstList`, `Const -> Ident [= Number]` | open | k65/declaration-comma-lists | Comma lists again, and a `const` with no value which is only an error at a use site. G2. |
| `topelem-bank` | 51 | `TopElem -> bank Ident` | intentionally-changed | — | Bank selection is a linker concept, and K816 does not own the linker: ld65 does. `K65_REMOVED_BANK` names `segment` and says where the memory map lives. Not cheap — an alias would give the word a meaning K816 cannot honour, which decision 0 forbids. `bank default` is refused with the rest, though it names the bank the original already compiled into and moves no byte: carving a name out of the rule reopens the silent acceptance the refusal was chosen to avoid. G2 (decision 10). |
| `topelem-image` | 54-55 | `TopElem -> image Ident = String \| image String = String` | intentionally-changed | legacy-k816/syntax-warn-malformed-image-binary | An asset pipeline is out of scope by `doc/design.md` §1, and a BMP decoder is not cheap by any reading. `K65_RESERVED_IMAGE` (`K816-K65-0001`) becomes a removal whose help names external conversion and `binary`. The fixture is the malformed-line one because that is where the rule bites: today a malformed `image` line is `K816-E0001`, the one shape decision 0 rules out. G1 and G6 (decision 7). |
| `topelem-binary` | 56 | `TopElem -> binary Ident = String` | open | legacy-k816/syntax-image-and-binary-top-level | Cheap: `.incbin` is exactly this, and ca65 already has it. G6 adds the existence check (`BINARY_NOT_FOUND`) and the include path. |
| `topelem-data` | 59-61 | `TopElem -> data SecStart Align OptNoCross DataList }`, `DataList` | compatible | k65/eval-forms | A named data section. K816 has it. |
| `data-arg` | 62 | `Data -> Arg` | compatible | k65/eval-forms | A literal data entry. Note that this path is `oparith(".byte", ...)`, *not* `datavalue`, which is why a section's `evaluator [ ]` transform does not reach it. |
| `data-address-word` | 63 | `Data -> && VarArg` | compatible | — | A label as two bytes, low then high. K816 lowers it to `.word .loword(sym)`, which is the same two bytes in the same order. The fixture is G6's. |
| `data-image-tiles` | 64-68, 105-116 | `Data -> ImgLoad ...`, `tiles`, `colormode`, `BitDir`, `BitSeq`, `RowDir`, `ImgOpt` | intentionally-changed | legacy-k816/syntax-data-binary-image-tiles | The bit-scanning image reader and its four direction terminals are the asset pipeline `doc/design.md` §1 excludes. `K816-K65-0001`, help naming external conversion. G6 (decision 7). |
| `data-imgwave` | 69, 81-87 | `Data -> ImgWaveStart OptWaveArgs`, `WaveArgs` | intentionally-changed | legacy-k816/syntax-data-binary-image-tiles | `imgwave` is a per-row displacement table for the image reader and shares its fate. Same code, same help. G6. |
| `data-binary` | 70-72 | `Data -> BinLoad`, `binary Ident \| binary String` | open | legacy-k816/syntax-data-binary-image-tiles | `.incbin` inside a data section. Note the original passes every byte through the section's `evaluator [ ]` transform, which `data-evaluator-entry` covers. G6. |
| `data-nocross-block` | 73 | `Data -> NoCross { DataList }` | open | — | A page-crossing restriction on part of a section. K816 has `nocross` on a whole section but does not parse it around a group of data entries. G6. |
| `data-undef` | 74 | `Data -> ?` | compatible | — | One byte with no stated value. K816 emits an explicit `$00` where the original emitted `.undef` and the linker filled zero, so the images agree; the fixture is G6's. |
| `data-repeat` | 75, 78 | `Data -> repeat Number { AsmLocation DataList }`, `AsmLocation` | compatible | — | Repeats the bytes a block produced N times. K816 emits the same bytes today, measured against the oracle; the fixture is G6's. |
| `data-code-block` | 76-77 | `Data -> code { Sequence }`, `NoCross code { ... }` | compatible | — | Instructions inside a data section. K816 emits the same bytes today, measured against the oracle; the `nocross` variant goes with `data-nocross-block`. The fixture is G6's. |
| `data-nocross-flag` | 89-90, 230 | `OptNoCross -> <empty> \| nocross`, `NoCross -> nocross` | compatible | golden/nocross-oversized.k816 | K816's `nocross` is the same restriction, and sub-plan D landed it. |
| `data-charset` | 92 | `Data -> charset String` | compatible | legacy-k816/syntax-data-charset-and-evaluator | The string maps each character to its index in it, and an unlisted character to zero. K816 emits the same bytes today, measured against the oracle. |
| `data-evaluator-entry` | 93 | `Data -> evaluator [ EvalCode ]` | open | k65/eval-forms | Measured in G0, and it is not what decision 5 assumed: `evaluator [ code ]` installs a per-byte transform on the *section*, binding `x` to each byte and masking the result to eight bits. It reaches `for ... eval` values, string characters, `binary` bytes and image tiles — every byte the section **generates** — and not literal data entries. The masking truncates where every other integer boundary rounds. Cheap, so decision 5's refusal is lifted. G6. |
| `data-string` | 94 | `Data -> String` | compatible | — | A string as bytes, through `charset` and, once G6 lands it, through the section's transform. The fixture is G6's. |
| `data-for-eval` | 95 | `Data -> for AllIdent = Number .. Number eval [ EvalCode ]` | open | legacy-k816/syntax-data-for-eval-range | K816 has the form and evaluates only the integer subset E needed. G4 moves it onto the real evaluator; G6 puts the section transform in front of it. |
| `data-label` | 96 | `Data -> Ident :` | compatible | golden/data-entries.k816 | A label inside a data section. |
| `data-preprocessor` | 98-101 | `Data -> PPIf DataList DataListPPElse PPEndif` | open | legacy-k816/syntax-preprocessor-sequence-and-data | The data-entry level of the three the preprocessor nests at. G5. |
| `topelem-inline` | 119 | `TopElem -> inline SecStart Sequence }` | compatible | golden/inline-expansion.k816 | An inline section, expanded at each call. Sub-plan E landed it. |
| `topelem-macro` | 120, 127-131 | `TopElem -> macro ...` and its argument list | compatible | — | Commented out in the original grammar: K65 has no macros, and neither does K816. The row exists so the walk covers the lines. |
| `topelem-func` | 121 | `TopElem -> func SecStart Align Sequence }` | compatible | golden/slice.k816 | A function, with an implicit `RTS`. |
| `topelem-naked` | 122 | `TopElem -> naked SecStart Align Sequence }` | compatible | — | A function without the implicit return. The fixture is G2's. |
| `topelem-main` | 123, 125 | `TopElem -> main { MainStart Align Sequence }`, `MainStart` | compatible | k65/label-offset-targets | The entry section. K816 names it `main` too, and `src/abi.rs` is what it means. |
| `secstart` | 124 | `SecStart -> Ident {` | compatible | golden/slice.k816 | The section name and its opening brace. |
| `align-directive` | 133-136 | `Align -> <empty> \| align Number \| align Number + Number \| address Number` | open | k65/align-and-address | Measured in G0: `align`/`address` are accepted at the start of `func`, `naked`, `main` **and** `data` bodies, and `align N + M` places the section at an address congruent to M modulo N — N a power of two in 1..65536, M less than N. K816 folds `align 256 + 8` into `align 264` today, and does not accept either inside a `func`. Decision 15's `.align N` + `.res M` gets the congruence right and emits M filler bytes where the original left the linker a gap, and the section's label follows the pair so it lands at the congruent address. Three further answers, G2: a placement directive is the **first statement** of a code section or an error naming that rule (data blocks unchanged); `align` adopts the original's two checks (power of two in 1..65536, `0 <= offset < boundary`), which narrows K816 — `align 3` compiles today and will not. G2. |
| `sequence` | 139-140 | `Sequence -> Sequence Instr \| <empty>` | compatible | golden/slice.k816 | A statement list. |
| `instr-preprocessor` | 142-145 | `Instr -> PPIf Sequence SeqListPPElse PPEndif` | open | legacy-k816/syntax-preprocessor-sequence-and-data | The statement level of the three. G5. |
| `instr-semicolon` | 149 | `Instr -> Instr ;` | compatible | — | A trailing semicolon after a statement, which K816 already accepts. The fixture is G2's. |
| `instr-nop-wait` | 151-153 | `Instr -> * \| * Number \| %` | intentionally-changed | k65/nop-and-wait | Measured in G0. `* N` waits N **cycles**, not N NOPs, and spends an odd cycle through `NOP $80` — opcode `$04`, which on the 65816 is `TSB dp`, a real instruction that reads and writes memory. `%` is `opnop(3)`, the same three-cycle wait, not `BRK`. Compatibility is not cheap here, it is impossible, so K816 keeps its documented meaning and `--strict-k65` warns. This is decision 11, the one recorded exception to decision 0. |
| `instr-call` | 154 | `Instr -> Arg` | open | k65/whole-program | A bare identifier at statement start is a call to an inline section. K816's `dp`/`abs`/`far` prefix rule swallows the following identifier, so `wsync hmove=a` — a call and a store on one line — is `K816-E0001`. A grammar defect with a corpus witness; the prefix may consume the next identifier only when it is one of those three spellings. G1 and G2 (decision 16). |
| `instr-far-call` | 155 | `Instr -> far Arg` | compatible | golden/far-calls.k816 | A far call. Sub-plan E landed it. |
| `instr-far-goto` | 156 | `Instr -> far goto Arg` | compatible | golden/far-calls.k816 | A far jump. |
| `instr-arith` | 157-159 | `Instr -> A\|X\|Y ArithOp Arg` | compatible | golden/hla-alu.k816 | The HLA arithmetic family. Sub-plan E landed it. |
| `instr-postfix` | 160 | `Instr -> Arg PostfixOp` | compatible | golden/hla-alu.k816 | Shifts, increments and decrements. |
| `instr-flag-set` | 161-162 | `Instr -> Flag + \| Flag -` | open | k65/nop-and-wait | `o-` is `CLV` and `compatible` as an alias (decision 14). `o+` is `SEV`, which the 65816 does not have — the original assembles `SEv` and dies without a diagnostic, a `legacy-defective` reading — so K816 refusing it is right. G2 splits the row if the two halves need different statuses. |
| `instr-stack-a` | 163-164 | `Instr -> A ?? \| A !!` | compatible | k65/nop-and-wait | `PLA` and `PHA`. The golden's `68 48`. |
| `instr-stack-flags` | 165-180 | `Instr -> z?? \| z!! \| c?? \| ...` and the two commented `Flag` forms | compatible | k65/nop-and-wait | Every flag letter's `??`/`!!` is `PLP`/`PHP` — the letter is decoration, and all fourteen spellings mean the same two instructions. The golden's `28 08`. |
| `instr-mov` | 181-183, 190-191 | `Instr -> MovSeq`, `MovSeq -> Arg = Arg \| MovSeq = Arg`, and the two commented forms | compatible | golden/hla-chains.k816 | Chained assignment, right to left. Sub-plan E landed it. |
| `instr-sax` | 184-185 | `Instr -> Arg = A & X \| Arg = X & A` | intentionally-changed | — | `SAX` is a 6502 undocumented opcode whose encodings are different, real instructions on the 65816. Compatibility is impossible, so decision 0's second clause applies: a named grammar rule (G1) and `K65_UNSUPPORTED_ILLEGAL_OPCODE`, whose help names the 65816 instruction the encoding would execute **and stops** — K816 assembles only the legal 6502/65C02/65816 instruction sets (`design.md` §1), so publishing a hand-rolled substitute would invite back what that boundary excludes, and for `SAX` and `AXS` there is no clean one. No oracle fixture measures the original's bytes: the argument is the boundary, not a measurement. The fixture is invented, under `tests/compat/k816/`. G2 (decision 12, corrected). |
| `instr-axs` | 186 | `Instr -> X &= A - Arg` | intentionally-changed | — | `AXS`, same reasoning as `instr-sax`. |
| `instr-return` | 187-188 | `Instr -> return \| return_i` | compatible | golden/slice.k816 | `RTS` and `RTI`. |
| `instr-branch-block` | 193-194, 196-209 | `BrInstr` and its `Br1`, `Br2`, `IfElse` forms — `{ } BranchOp`, `} always`, `} never`, `else` | compatible | golden/hla-branches.k816 | The block-and-branch family. Sub-plan E landed all of it. |
| `instr-goto` | 212-214 | `Instr -> goto VarArg \| BranchOp goto VarArg \| goto ( VarArg )` | compatible | golden/hla-branches.k816 | Direct and indirect jumps, conditional and not. |
| `instr-call-label` | 215 | `Instr -> call VarArg` | compatible | golden/hla-branches.k816 | `JSR` to a label. |
| `instr-label` | 216 | `Instr -> Ident :` | compatible | k65/label-offset-targets | A label in a statement sequence, local (`.name`) or not. |
| `instr-data-block` | 218-219 | `Instr -> data { DataList }`, `NoCross data { DataList }` | compatible | golden/data-entries.k816 | An anonymous data block inside a code section. |
| `instr-break` | 222-223 | `Instr -> break \| BranchOp break` | compatible | golden/hla-branches.k816 | Conditional and unconditional loop exit. |
| `instr-repeat` | 225-226 | `Instr -> repeat \| BranchOp repeat` | compatible | golden/hla-branches.k816 | Conditional and unconditional loop restart. |
| `putlabel` | 232 | `PutLabel -> <empty>` | compatible | — | A generated label, referenced by no rule in the shipped grammar. The row exists so the walk covers the line. |
| `allident` | 237-242 | `AllIdent -> Ident \| A \| X \| Y \| S \| Flag` | compatible | k65/declaration-comma-lists | The register and flag letters are tokens, so `A`, `X`, `Y`, `S`, `c`, `C`, `d`, `D`, `i`, `I`, `o`, `O` are not ordinary identifiers; `AllIdent` is where they are allowed to be names again — in the evaluator, and as a `for` variable. `const A` is a syntax error in the original for exactly this reason. |
| `register-tokens` | 244-251 | `A -> a\|A`, `X -> x\|X`, `Y -> y\|Y`, `S -> s\|S` | compatible | golden/slice.k816 | The register names, either case. |
| `flag-tokens` | 253-260 | `Flag -> C\|c\|D\|d\|I\|i\|O\|o` | open | k65/nop-and-wait | Four flags, either case, and `O`/`o` is the *overflow* flag rendered as `V`/`v`. K816 spells it `v`; the alias is cheap and `compatible` (decision 14). Note what is **not** here: no `z` or `n` flag, because the 6502 has no instruction to set or clear either. G2. |
| `arg-forms` | 264-276 | `Arg -> A \| X \| Y \| S \| VarArg \| Number \| &< VarArg \| &> VarArg \| VarArg , X \| VarArg , Y \| ( VarArg ) \| ( VarArg , X ) \| ( VarArg ) , Y` | compatible | golden/hla-alu.k816 | The operand family, including the `&<`/`&>` low and high byte forms. Sub-plan E landed it. |
| `vararg-label-offset` | 278-282 | `VarArg -> Ident \| Ident ± Ident \| Ident ± Number` | open | k65/label-offset-targets | A label with an offset, as a read or as a store target, and the original emits the store with no diagnostic — decision 17, measured. One gap recorded: K816 assembles `slot+1` as absolute where the original used zero page, even though it chooses direct page for `slot` itself. Settled for G2: an offset expression **inherits the base symbol's address size**, in reads as well as stores — wider than decision 17's text, which speaks only of store targets, because the two spellings of one expression cannot disagree about its encoding. |
| `arithop` | 286-287, 289-290, 293-295 | `ArithOp -> + \| & \| &? \| ? \| ^ \| - \| \|` | compatible | golden/hla-alu.k816 | `ADC`, `AND`, `BIT`, `CMP`, `EOR`, `SBC`, `ORA`. |
| `arithop-anc` | 288 | `ArithOp -> &.` | intentionally-changed | — | `ANC`, an undocumented 6502 opcode. Same reasoning as `instr-sax`. |
| `arithop-dcp` | 291 | `ArithOp -> ? --` | intentionally-changed | — | `DCP`, same reasoning. |
| `arithop-isc` | 292 | `ArithOp -> - ++` | intentionally-changed | — | `ISC`, same reasoning. |
| `postfixop` | 297-302 | `PostfixOp -> << \| -- \| ++ \| >> \| <<< \| >>>` | compatible | golden/hla-alu.k816 | `ASL`, `DEC`, `INC`, `LSR`, `ROL`, `ROR`. Note there is no `>` and no `<=`: those are the legacy K816's, and their rows are below. |
| `branchop` | 304-319 | `BranchOp -> >= \| < \| == \| <0 \| != \| >=0 \| >>= \| <<=` and the eight `c+?`-style spellings | compatible | golden/hla-branches.k816 | Sixteen spellings of eight branches. |
| `number` | 327-328 | `Number -> RawNumber \| [ EvalCode ]` | open | k65/eval-integer-boundary | A number is a literal or an evaluator bracket, everywhere a number appears — an operand, a data entry, an initializer, an `align`, a `for` bound, an `image` argument. G4. |
| `number-eval-bracket` | 330 | `BracketNumber -> [ EvalCode ]` | open | k65/eval-integer-boundary | The array-size bracket, the same evaluator. G4. |
| `ident` | 333-334 | `Ident -> IdentText \| . IdentText` | compatible | k65/label-offset-targets | A name, or a file-local one. The original mangles `.LT` to `__local1_LT`; K816 uses ca65's cheap-local scope, which is `backend-different` in the symbol table and identical in the bytes. |
| `evalcode` | 341-362 | The evaluator's stack-machine comment block, `EvalCode -> EvalInstr \| EvalCode EvalInstr \| EvalInstr ;` | open | k65/eval-statements | The evaluator is a stack machine and the grammar compiles to its opcodes; K816 evaluates the tree instead, which is invisible from outside. G4. |
| `evalinstr-statements` | 363-367 | `EvalInstr -> EvExpr \| { EvalCode } \| if \| if/else \| while` | open | k65/eval-statements | Cheap: with an interpreter in place, a loop is a jump in the same interpreter. `EVAL_BUDGET` bounds the `while`. G4 (decision 5). |
| `evexpr-operators` | 371-372, 374-385, 387, 389-433 | `EvExpr`, `EvSetVar` and its eleven compound assignments, the ternary, and the whole precedence chain down to `EvUnary` | open | legacy-k816/syntax-evaluator-expressions | Two corrections G0 measured. `%` is a **non-negative** modulo, `fmod(fmod(a,b)+b,b)`, so `(0-7) % 3` is 2. And there is **no `\|\|`**: `EvLogOr` and `EvLogAnd` are both spelled `&&` (`compiler.inc:390,393`), so a source containing `\|\|` does not compile at all. `^` is xor, which `doc/k65/todo.md` listed as unfinished and is not. G4. |
| `evelem-values` | 435-440 | `EvElem -> String \| RawNumber \| RawFloat \| pi \| ( EvExpr ) \| AllIdent` | open | k65/eval-sees-consts | A name resolves against the compiler's one symbol table: a `const` gives its value, a data label gives its **section**, an evaluator variable gives what was assigned. A `var` does not resolve. `pi` is `3.141593` — six decimals, not the machine constant. A string is a value inside the evaluator and cannot cross an integer boundary. G4. |
| `evelem-incdec` | 441-444 | `EvElem -> ++ AllIdent \| -- AllIdent \| AllIdent ++ \| AllIdent --` | open | — | Prefix and postfix increment on evaluator variables. Cheap. G4. |
| `evelem-index` | 445-446 | `EvElem -> EvElem [ EvSetVar ] \| EvElem [ EvSetVar , EvSetVar ]` | open | legacy-k816/syntax-evaluator-index | What is indexed is a **section**, not an array value, and `tab[i]` is exactly `index(tab, i)`. Out of range gives zero with no diagnostic; the two-dimensional form needs the section to carry image dimensions. There is no array literal in the original: `[10, 20, 30]` is the comma *pop* operator and evaluates to 30. G4. |
| `evelem-call` | 447, 452-456 | `EvElem -> AllIdent ( OptEvArgList )` and the argument list | open | k65/eval-refusals | A call, with the argument count encoded in the opcode name. The library is `max min floor ceil round frac sin cos rnd clamp color sqrt pow acos asin index addbyte size print error` — and **no `abs`**, which decision 5's list wrongly includes. G4. |
| `evelem-method` | 448-449, 459-462 | `EvElem -> EvElem . AllIdent [ ( OptEvArgList1 ) ]` and its argument list | open | k65/eval-forms | Measured in G0 and it is pure sugar: `x.f(a)` compiles to `f(x, a)` and `x.f` to `f(x)`. There is no method table. Cheap — a grammar rule — so decision 5's refusal is lifted and `K65_UNSUPPORTED_EVAL_FORM` loses this producer. G4. |
| `topelem-evalfunc` | 465-471 | `TopElem -> evalfunc AllIdent ( OptEvArgDefs ) [ EvalCode ] \| { EvalCode }`, `EvArgDefs` | open | k65/eval-statements | User-defined evaluator functions. Measured in G0: parameters are static slots allocated once at definition time, not a frame, and **a function that calls itself calls evaluator function zero** — the map entry exists before its `user_id` is assigned — so recursion returns another function's answer or dies. Recursion is therefore not a compatibility question at all: no working K65 program contains it, and K816 implementing it properly is a `K816-extension` row. G4. |
| `terminal-numbers` | 476-479, 490-493 | `RawNumber :: [0-9]+ \| 0x… \| 0b…`, `RawFloat :: [0-9]*[.][0-9]+`, and the four commented-out earlier forms | open | k65/eval-integer-boundary | Decimal, hex, binary and a fraction. There is **no** exponent form in the live terminals, so `1e3` does not compile, and no digit separators. One original defect measured: inside an evaluator bracket `0b1010` is **zero**, because the evaluator's own `ParseInt` handles `0x` and not `0b` (`convert_parse.cpp:36`) — outside a bracket the same literal is 10. `legacy-defective` when G4 settles it. |
| `terminal-ident` | 480, 495 | `IdentText :: [a-zA-Z_][a-zA-Z0-9_]*` | compatible | golden/slice.k816 | Identifiers. |
| `terminal-string` | 496-497 | `String :: "…"` and the commented-out `EvalString` | compatible | — | A double-quoted string with no escapes: the terminal is `"[^"]*"`, so a quote cannot appear inside one and a backslash is an ordinary character. |
| `terminal-comment` | 481-483, 498-500 | Whitespace, `//` and `/* */` | compatible | golden/slice.k816 | Comments and whitespace, and the same two comment forms K816 has. |

## K816 constructs that are not in the original grammar

Every row here is what `--strict-k65` rejects (decision 20). A construct in
this table has no K65 spelling, so `STRICT_K65_EXTENSION` names the construct
and nothing else.

| Row | Construct | Status | Fixture | Reason |
| --- | --- | --- | --- | --- |
| `k816-width-annotations` | `@a8`, `@a16`, `@i8`, `@i16` on a section | K816-extension | golden/native-modes.k816 | The 65816's register widths. K65 targets the 6502 and has no such axis. |
| `k816-far-function` | `far func`, `jsl`, `jml`, `rtl` | K816-extension | legacy-k816/peephole-tail-control | Long addressing. The original's `far` is a bank-switching stub the linker generates, not an instruction. |
| `k816-segment` | `segment NAME` | K816-extension | golden/layout-unit-a.k816 | The replacement `K65_REMOVED_BANK` points at. Named ld65 segments, not banks. |
| `k816-call-contracts` | `(a) -> a, c` on a function | K816-extension | golden/contract-unit-a.k816 | Register and flag call contracts. G9 adds the flags; the unlisted-output rule is a row of its own below. |
| `k816-unlisted-output` | An output a `->` clause does not list is undefined | K816-extension | — | Only for a function *with* a clause, so no K65 function changes meaning — K65 has no contracts. G9 (decision 19). |
| `k816-zero-store-stz` | `NAME = 0` rewritten to `STZ` | K816-extension | legacy-k816/peephole-stz-zero | The 65816's `STZ`; the original has no such instruction at all. |
| `k816-zero-store-target` | `$10 = 0` refused: the target must be a name | K816-extension | — | `postmortem.md` §E.3.3. The documented rule is honoured, and the original has no `STZ` to be compatible with (decision 25). |
| `k816-postfix-gt` | `} >` | K816-extension | legacy-k816/syntax-postfix-gt-efficiency-warning | Absent from the original's `BranchOp`. The legacy K816 invented it and warned that it expands to three branches; K816 expands it to two (`inx; beq exit; bcs loop`), so G8's warning states K816's own expansion (decision 23). |
| `k816-postfix-le` | `} <=` | K816-extension | legacy-k816/syntax-postfix-le-efficiency-warning | The same, and here the legacy wording — two branches, `BCC` + `BEQ` — is K816's expansion too. |
| `k816-hex-binary-char-literals` | `$FF`, `%1010`, `'a'`, string escapes | K816-extension | — | The original's terminals are `[0-9]+`, `0x…`, `0b…`, `[0-9]*[.][0-9]+` and `"[^"]*"` and nothing else. `%` in particular is the original's three-cycle wait, which is why `instr-nop-wait` is the exception it is. |
| `k816-unreachable-warning` | No diagnostic for dropped unreachable code | intentionally-changed | legacy-k816/peephole-warn-unreachable-after-always | The legacy K816 warned per dropped instruction; F decision 20 says the optimizer reports nothing and `emit-ir --after-opt --explain` answers on demand. Decision 18 keeps that. |
| `k816-tail-call` | No `jsr`+`rts` → `jmp` rewrite | intentionally-changed | legacy-k816/peephole-tail-control | Declined in F decision 17 and again in decision 19: the rewrite needs an ABI rule about who may read stack memory, and the corpus has nothing to say about it. |
| `k816-optimizer-dead-writes` | Dead register and flag writes removed | backend-different | legacy-k816/peephole-flag-reg-safety | K816's optimizer proves more writes dead than the legacy compiler's did. Same observable behaviour, fewer bytes. |
| `k816-entry-abi` | `--entry native,a8,i8`, and no mode prologue at a function entry | K816-extension | — | The legacy K816 emitted `sep #$30` at every `@a8 @i8` entry; K816 treats the annotation as an assertion about the environment (`src/abi.rs`, `K816-E0143`). Every imported fixture's bytes differ from the legacy ones by exactly that prologue. G11 lands the flag. |
| `k816-strict-k65` | `--strict-k65` | K816-extension | — | The switch this table exists for. G10. |
| `k65-corpus-whole-program` | — | open | k65/whole-program | Not a construct: the row the corpus's one whole program answers to. Its status is the conjunction of the rows it exercises, and it closes when they do. |
| `optimizer-dead-register-write` | — | backend-different | legacy-k816/peephole-cross-line-register-fold | Not a construct: the row the imported optimizer fixtures answer to where they agree with the legacy bytes after the entry prologue. |
| `optimizer-dead-flag-write` | — | backend-different | legacy-k816/peephole-flag-reg-safety | The same, for the flag half. |
| `optimizer-branch-to-next-label` | Pass 9, branch to the next label | open | legacy-k816/peephole-stz-flags | Ported from the legacy peephole (decision 18). G8. |
| `optimizer-duplicate-return` | Pass 10, adjacent duplicate returns | open | — | Ported from the legacy peephole (decision 18). The one case where a *user-written* operation is removed as unreachable, justified structurally: a return has no successors and no label lets an edge in. G8. |

## Lines the walk does not classify

`tests/compat.rs` checks that the `Lines` column above and the ranges here
together cover `compiler.inc` lines 1 to 515 exactly once. These are the lines
that carry no production:

    1, 3, 6, 11, 13-14, 19, 22, 26-28, 30, 32, 42-43, 49-50, 52-53, 57-58,
    79-80, 88, 91, 97, 102-104, 117-118, 126, 132, 137-138, 141, 146-148, 150,
    189, 192, 195, 210-211, 217, 220-221, 224, 227-229,
    231, 233-236, 243, 252, 261-263, 277, 283-285, 296, 303, 320-326, 329,
    331-332, 335-340, 368-370, 373, 386, 388, 434, 450-451, 457-458, 463-464,
    472-475, 484-489, 494, 501-502

blank lines and section comments, and

    503-515

the C++ epilogue after `$$H` — the `#include`s, the `ST` macro that makes a
preprocessor-disabled region skip its semantic action, and the generated
parser's entry point.
