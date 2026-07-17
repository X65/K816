Keeping `.k65` makes compatibility part of the language contract. K816 should be defined as:

> K816 = original K65 semantics + compatible extensions for the 65816, tooling, and OS development.

“Strict superset” should mean every valid original K65 program remains valid and behaves the same under K816. It does not necessarily require identical object bytes, since ca65/ld65 may encode metadata and layout differently. For documented HLA constructs, however, preserving the expected instruction expansion is valuable because timing and code size can be observable.

I would establish these rules before implementing the new parser:

* Reserve every original K65 keyword, operator, directive, and syntactic form immediately.
* If a legacy feature is recognized but not implemented, issue a specific compatibility diagnostic. Never reinterpret it as a new K816 construct.
* Preserve K65 expression precedence, implicit conversions, scoping, comments, literals, and even inconvenient quirks unless an explicit opt-in mode says otherwise.
* Default legacy code to 6502-compatible 8-bit accumulator and index semantics. Widening must require a K816 construct.
* Lower original K65 HLA syntax into the same native instruction sequence it historically implied, before the common peephole optimizer runs.
* Never pass ambiguous K65 syntax directly through to ca65. The frontend must translate it explicitly so ca65 cannot assign it different semantics.

For example:

```text
error[K816-K65-0041]: K65 `...` construct is not implemented yet
  --> example.k65:18:5
   |
18 |     ...
   |     ^ this syntax belongs to the original K65 language

note: the syntax is reserved for K65 compatibility
help: use `...` temporarily, or follow implementation status at <docs URL>
```

The compatibility work should have its own manifest:

| Status                 | Meaning                                    |
| ---------------------- | ------------------------------------------ |
| Compatible             | Implemented with original K65 semantics    |
| Reserved               | Recognized as K65, but not implemented yet |
| K816 extension         | New syntax that cannot conflict with K65   |
| Intentional difference | Exceptional, documented, and ideally gated |

I would also provide `--strict-k65`, but not make it a separate compiler architecture. Normal K816 mode accepts the whole superset; strict mode merely rejects K816 extensions so portable K65 programs can be checked.

Before the C rewrite progresses much further, freeze a reference snapshot of the [original K65 repository](https://github.com/Krzysiek-K/k65) and build a compatibility corpus from:

* Its tests and examples.
* Its documentation.
* Programs compiled by both implementations.
* Negative cases and parser edge cases.
* Real `.k65` sources, including current K816 sources.

The test hierarchy should distinguish:

1. Source compatibility: it parses.
2. Semantic compatibility: it behaves identically.
3. Expansion compatibility: documented HLA produces the expected assembly sequence.
4. Binary compatibility: checked where meaningful, but not universally promised.

This also improves the proposed C architecture. The frontend should not begin with “a convenient new grammar.” It should begin with a complete inventory of K65 grammar, including reserved-but-unimplemented nodes. Both original K65 and new K816 syntax then lower into the common assembly-shaped IR, where the effect-aware optimizer operates.

So the revised implementation order should be:

1. Freeze K65 reference, document the compatibility contract, and classify the golden tests.
2. Implement the complete lexical and syntactic namespace, including unsupported-feature nodes.
3. Restore core K65 behavior.
4. Add the common lowered IR and optimizer.
5. Add K816/65816 extensions without consuming any K65 namespace.
6. Emit ca65 source, debug annotations, and diagnostics.
7. Add LSP support over the same frontend.

That makes “K816” an honest continuation of K65 rather than merely another language that happens to use its filename extension.

