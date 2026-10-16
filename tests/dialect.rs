/*****************************************************************************/
/*                                                                           */
/*                                 dialect.rs                                */
/*                                                                           */
/*              K816 and K65 sources: what each dialect accepts              */
/*                                                                           */
/*                                                                           */
/* Copyright (c) 2026 Tomasz Sterna (smokku)                                 */
/*                                                                           */
/* Permission to use, copy, modify, and/or distribute this software for any  */
/* purpose with or without fee is hereby granted.                            */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  */
/* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   */
/* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            */
/*                                                                           */
/*****************************************************************************/

//! A source's dialect, and what each one accepts (`plans/14-readable-hla.md`).
//!
//! The grammar is one, so a K65 spelling parses in any file; the dialect is
//! what analysis is told, and it is read off the file's name.

use k816::source::{Dialect, SourceMap};
use k816::{Options, compile_to_ca65};

#[test]
fn a_sources_dialect_is_its_extension() {
    for (name, dialect) in [
        ("main.k65", Dialect::K65),
        ("MAIN.K65", Dialect::K65),
        ("file:///home/me/demo/main.k65", Dialect::K65),
        ("main.kk", Dialect::K816),
        ("main.k816", Dialect::K816),
        ("lib.k65/main.kk", Dialect::K816),
        ("main", Dialect::K816),
        ("main.k65.bak", Dialect::K816),
    ] {
        assert_eq!(Dialect::of_name(name), dialect, "{name}");
        let mut map = SourceMap::new();
        let file = map.add_file(name, "");
        assert_eq!(map.file(file).dialect(), dialect, "{name}");
    }
}

/// A program both dialects accept compiles to the same assembly under every
/// extension: the dialect decides what is refused, never what anything means.
///
/// Its branches are native: since sub-plan Q a condition has no spelling both
/// dialects take — K816 writes `if >= { … }` and a `.k65` source the bare
/// `>= { … }` (`plans/16-if-statement.md` decisions 2 and 10).
#[test]
fn a_program_both_dialects_accept_compiles_alike_under_every_extension() {
    const PROGRAM: &str = "\
var counter = $10
var flags = $11

func main @a8 @i8 {
    x = #0
    a?#4
    bcc .small
    counter++
.small:
    lda flags
    and #$0F
    bne .done
    flags = a = counter
.done:
}
";
    let assembly = |name: &str| {
        let mut map = SourceMap::new();
        let file = map.add_file(name, PROGRAM);
        let result = compile_to_ca65(&map, file, &Options::default());
        assert_eq!(result.diagnostics, vec![], "{name}");
        result.assembly.expect("compiles").replace(name, "UNIT")
    };
    let k65 = assembly("main.k65");
    assert_eq!(assembly("main.kk"), k65);
    assert_eq!(assembly("main.k816"), k65);
}

/// What `text` compiles to as a unit called `name`: the codes it was told,
/// and the assembly with comments and the unit's own name taken out, so two
/// spellings of one program compare equal.
fn code(name: &str, text: &str) -> (Vec<&'static str>, Vec<String>) {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let result = compile_to_ca65(&map, file, &Options::default());
    let codes = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    let assembly = result
        .assembly
        .unwrap_or_else(|| panic!("{name} compiles:\n{text}\n{:#?}", result.diagnostics));
    (codes, code_lines(&assembly, name))
}

/// One pair per inventory row the plan respells, K816's spelling first, and
/// every flag both ways in every place a condition goes.
fn respelled_pairs() -> Vec<(String, String)> {
    let mut pairs: Vec<(String, String)> = vec![
        ("loop { x-- } while !=".into(), "{ x-- } !=".into()),
        ("loop { x++ } while >=0".into(), "{ x++ } >=0".into()),
        (
            "loop { a = flags } always".into(),
            "{ a = flags } always".into(),
        ),
        (
            "loop { if < break } never".into(),
            "{ < break } never".into(),
        ),
        (
            "nocross loop { y++ } while !=".into(),
            "nocross { y++ } !=".into(),
        ),
        (
            "nocross loop { y++ } always".into(),
            "nocross { y++ } always".into(),
        ),
        (
            "loop { x++ \n if == repeat \n y++ } while !=".into(),
            "{ x++ \n == repeat \n y++ } !=".into(),
        ),
        (
            "loop { loop { x++ } while != \n y++ } while !=".into(),
            "{ { x++ } != \n y++ } !=".into(),
        ),
        ("if v { x++ }".into(), "<<={ x++ }".into()),
        ("if !v goto .out".into(), ">>= goto .out".into()),
        ("push a, x, y".into(), "a!! x!! y!!".into()),
        ("pull y, x, a".into(), "y?? x?? a??".into()),
        ("push p, dbr, dpr".into(), "p!! dbr!! dpr!!".into()),
        ("pull dpr, dbr, p".into(), "dpr?? dbr?? p??".into()),
        ("push a, $0012, x".into(), "a!! \n pea #$0012 \n x!!".into()),
        ("push LIMIT".into(), "pea #LIMIT".into()),
        ("push #LIMIT".into(), "pea #LIMIT".into()),
        ("push (&<flags):word".into(), "pea #(&<flags):word".into()),
    ];
    for flag in ['c', 'z', 'n', 'v'] {
        for (bare, signed) in [
            (format!("{flag}"), format!("{flag}+?")),
            (format!("!{flag}"), format!("{flag}-?")),
        ] {
            pairs.push((format!("if {bare} {{ x++ }}"), format!("{signed}{{ x++ }}")));
            pairs.push((
                format!("if {bare} goto .out"),
                format!("{signed} goto .out"),
            ));
            pairs.push((
                format!("loop {{ x++ \n if {bare} break }} always"),
                format!("{{ x++ \n {signed} break }} always"),
            ));
            pairs.push((
                format!("loop {{ x++ }} while {bare}"),
                format!("{{ x++ }} {signed}"),
            ));
        }
    }
    pairs
}

/// `body` as the function every pair is compiled in.
fn unit(body: &str) -> String {
    format!("var flags = $10\nconst LIMIT = $1234\n\nfunc work @a8 @i8 {{\n    {body}\n.out:\n}}\n")
}

/// Each of K816's spellings compiles to the bytes of the K65 spelling it
/// replaces: respelling changes nothing a program does
/// (`plans/14-readable-hla.md` decision 13).
#[test]
fn every_respelled_form_compiles_to_the_bytes_of_its_k65_spelling() {
    for (readable, k65) in respelled_pairs() {
        assert_eq!(
            code("unit.kk", &unit(&readable)),
            code("unit.k65", &unit(&k65)),
            "`{readable}` and `{k65}`"
        );
    }
}

/// `text` with every edit of every fix the compiler offered applied.
fn fixed(name: &str, text: &str) -> (Vec<&'static str>, String) {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let result = compile_to_ca65(&map, file, &Options::default());
    let codes = result.diagnostics.iter().map(|d| d.code.as_str()).collect();
    let mut edits: Vec<_> = result
        .diagnostics
        .iter()
        .flat_map(|d| d.fixes.iter())
        .flat_map(|fix| fix.edits.iter())
        .collect();
    edits.sort_by_key(|edit| std::cmp::Reverse((edit.span.start, edit.span.end)));
    let mut text = text.to_owned();
    for edit in edits {
        text.replace_range(
            edit.span.start as usize..edit.span.end as usize,
            &edit.replacement,
        );
    }
    (codes, text)
}

/// A K816 source refuses each K65 spelling the plan respells, a `.k65` source
/// takes it, and the refusal's fix writes K816 that compiles to the bytes the
/// K65 spelling compiles to in its own dialect (decision 11).
#[test]
fn a_k816_source_refuses_k65_spellings_and_their_fixes_change_no_byte() {
    for (_, k65) in respelled_pairs() {
        let source = unit(&k65);
        let (codes, repaired) = fixed("unit.kk", &source);
        let expected: &[&str] = if k65.contains("<<=") || k65.contains(">>=") {
            &["K816-K65-0009"]
        } else if k65.contains("+?") || k65.contains("-?") {
            &["K816-K65-0008"]
        } else if k65.contains("!!") || k65.contains("??") {
            &["K816-K65-0010"]
        } else if k65.contains('}') && !k65.contains("pea") {
            &["K816-K65-0007"]
        } else {
            &[]
        };
        for code in expected {
            assert!(codes.contains(code), "`{k65}` in a .kk: {codes:?}");
        }
        // Whatever else the program says, it says in both dialects.
        let in_k65 = code("unit.k65", &source).0;
        assert!(
            in_k65.iter().all(|code| !code.starts_with("K816-K65-")),
            "`{k65}` in a .k65: {in_k65:?}"
        );
        let besides: Vec<&str> = codes
            .iter()
            .filter(|code| !code.starts_with("K816-K65-"))
            .copied()
            .collect();
        assert_eq!(besides, in_k65, "`{k65}`");
        assert_eq!(
            code("unit.kk", &repaired),
            code("unit.k65", &source),
            "`{k65}` fixed to\n{repaired}"
        );
        assert_eq!(
            fixed("unit.k816", &source).0,
            codes,
            "`{k65}`: .k816 is .kk"
        );
    }
}

/// `readable-hla.kk` is `readable-hla.k65` respelled, and nothing else: the
/// two compile to the same program. The pair is the witness the manifest's
/// three readable-spelling rows name.
#[test]
fn the_readable_golden_is_its_k65_twin_respelled() {
    let golden = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden");
    let read = |name: &str| std::fs::read_to_string(golden.join(name)).expect("read golden");
    let readable = code("readable-hla.kk", &read("readable-hla.kk"));
    let k65 = code("readable-hla.k65", &read("readable-hla.k65"));
    assert_eq!(readable.0, Vec::<&str>::new(), "the K816 spelling warns");
    assert_eq!(readable, k65);
}

/// The codes `body` is told in a `@a16 @i16` function, the fixes they carry,
/// and the program the first fix of each makes, applied.
fn pushed(body: &str) -> (Vec<&'static str>, Vec<String>) {
    let text =
        format!("var mem = $1234\nconst LIMIT = 5\n\nfunc work @a16 @i16 {{\n    {body}\n}}\n");
    let mut map = SourceMap::new();
    let file = map.add_file("push.kk", text.as_str());
    let result = compile_to_ca65(&map, file, &Options::default());
    let codes = result.diagnostics.iter().map(|d| d.code.as_str()).collect();
    let fixes = result
        .diagnostics
        .iter()
        .flat_map(|d| d.fixes.iter())
        .flat_map(|fix| fix.edits.iter())
        .map(|edit| edit.replacement.clone())
        .collect();
    (codes, fixes)
}

/// A pushed constant has to be written as the word `PEA` pushes
/// (`plans/14-readable-hla.md` decision 9): one written narrower is warned
/// about and rewritten as four hex digits, one that is a byte by construction
/// is refused and expanded with `:word`, and a sizeless one is taken at its
/// word.
#[test]
fn a_pushed_constant_is_written_as_the_word_it_pushes() {
    for (body, codes, fixes) in [
        ("push $12", vec!["K816-W0174"], vec!["$0012"]),
        ("push $012", vec!["K816-W0174"], vec!["$0012"]),
        ("push 0x12", vec!["K816-W0174"], vec!["$0012"]),
        ("push 18", vec!["K816-W0174"], vec!["$0012"]),
        ("push 0", vec!["K816-W0174"], vec!["$0000"]),
        ("push %00010010", vec!["K816-W0174"], vec!["$0012"]),
        ("push ($12)", vec!["K816-W0174"], vec!["$0012"]),
        ("push #$12", vec!["K816-W0174"], vec!["$0012"]),
        ("push a, $12, x", vec!["K816-W0174"], vec!["$0012"]),
        ("push $0012", vec![], vec![]),
        ("push 300", vec![], vec![]),
        ("push %0000000000010010", vec![], vec![]),
        ("push LIMIT", vec![], vec![]),
        ("push LIMIT + 1", vec![], vec![]),
        ("push &&mem", vec![], vec![]),
        ("push (&<mem):word", vec![], vec![]),
        ("push &<mem", vec!["K816-E0175"], vec!["(&<mem):word"]),
        ("push &>mem", vec!["K816-E0175"], vec!["(&>mem):word"]),
        ("push (&<mem)", vec!["K816-E0175"], vec!["(&<mem):word"]),
        ("push 'a'", vec!["K816-E0175"], vec!["('a'):word"]),
        ("push LIMIT:byte", vec!["K816-E0175"], vec!["LIMIT:word"]),
        // A bare name is the place it names, as in `a = mem`, and `push`
        // takes no place; its address is said with the marker.
        ("push mem", vec!["K816-E0177"], vec![]),
        ("push work", vec!["K816-E0177"], vec![]),
        ("push #mem", vec![], vec![]),
        ("push #work", vec![], vec![]),
    ] {
        assert_eq!(
            pushed(body),
            (codes, fixes.into_iter().map(String::from).collect()),
            "{body}"
        );
    }
    // Each fix, applied, is a push the rule accepts — and the warning's
    // pushes the same word the narrow spelling did.
    for (fixed, narrow) in [("push $0012", "push $12"), ("push $0012", "push 18")] {
        assert_eq!(
            code(
                "unit.kk",
                &format!("func work @a16 @i16 {{\n    {fixed}\n}}\n")
            )
            .1,
            {
                let text = format!("func work @a16 @i16 {{\n    {narrow}\n}}\n");
                let mut map = SourceMap::new();
                let file = map.add_file("unit.kk", text.as_str());
                let result = compile_to_ca65(&map, file, &Options::default());
                code_lines(
                    &result.assembly.expect("a warning still compiles"),
                    "unit.kk",
                )
            },
            "{narrow}"
        );
    }
    for fixed in ["push (&<mem):word", "push ('a'):word", "push LIMIT:word"] {
        assert_eq!(pushed(fixed), (vec![], vec![]), "{fixed}");
    }
}

/// The assembly lines [`code`] compares, from an assembly text: no comments,
/// no unit name, and no source lines — those are debug information, not
/// bytes, and a K65 spelling can take more lines than the K816 one it is
/// compared with.
fn code_lines(assembly: &str, name: &str) -> Vec<String> {
    assembly
        .replace(name, "UNIT")
        .lines()
        .map(|line| line.split(';').next().unwrap_or_default().trim_end())
        .filter(|line| !line.trim().is_empty() && !line.trim_start().starts_with(".dbg"))
        .map(str::to_owned)
        .collect()
}

/// Rule 3: in a K816 source's HLA a number is always a value, so a number in
/// a memory shape is refused, with the native mnemonic as the fix where the
/// statement is one instruction (`plans/14-readable-hla.md` decision 10).
/// The fix compiles to the bytes the HLA statement compiles to in a `.k65`,
/// which keeps K65's reading. A displacement from `S`, an `&` offset, a
/// named place and a value are not numbers used as places.
#[test]
fn a_number_used_as_a_place_is_refused_and_its_fix_is_the_mnemonic() {
    for (statement, fix) in [
        ("a = $10,x", Some("lda $10,x")),
        ("a = ($10),y", Some("lda ($10),y")),
        ("a = ($10,x)", Some("lda ($10,x)")),
        ("a = dp $10", Some("lda dp $10")),
        ("$10 = a", Some("sta $10")),
        ("$0300,x = a", Some("sta $0300,x")),
        ("x = $10,y", Some("ldx $10,y")),
        ("$10++", Some("inc $10")),
        ("$10--", Some("dec $10")),
        ("$10<<", Some("asl $10")),
        ("$10>>>", Some("ror $10")),
        ("a + $10,x", Some("adc $10,x")),
        ("a?($10),y", Some("cmp ($10),y")),
        ("a&?$10,x", Some("bit $10,x")),
        ("a = [$10],y", None),
    ] {
        let source = unit(statement);
        let (codes, repaired) = fixed("unit.kk", &source);
        assert_eq!(codes, ["K816-E0176"], "`{statement}`");
        assert_eq!(
            code("unit.k65", &source).0,
            Vec::<&str>::new(),
            "`{statement}` in a .k65"
        );
        match fix {
            Some(fix) => {
                assert!(
                    repaired.contains(&format!("    {fix}\n")),
                    "`{statement}` fixed to\n{repaired}"
                );
                assert_eq!(
                    code("unit.kk", &repaired),
                    code("unit.k65", &source),
                    "`{statement}` and `{fix}`"
                );
            }
            None => assert_eq!(repaired, source, "`{statement}` was offered a fix"),
        }
    }
    // A chain of two instructions is refused at each number, with no fix:
    // what the instructions are is the author's to write.
    let (codes, repaired) = fixed("unit.kk", &unit("$11 = a = $10,x"));
    assert_eq!(codes, ["K816-E0176", "K816-E0176"]);
    assert_eq!(repaired, unit("$11 = a = $10,x"));
    for statement in [
        "a = 3,s",
        "a = (3,s),y",
        "a = &2,x",
        "a = flags,x",
        "a = (flags),y",
        "a = flags + 1",
        "a = $10",
        "a = $10 + 1",
        "a = &<flags",
        "a + $10",
        "x?$10",
    ] {
        assert_eq!(
            code("unit.kk", &unit(statement)).0,
            Vec::<&str>::new(),
            "`{statement}`"
        );
    }
    // The zero store keeps its own refusal of a numeric target.
    let (codes, _) = fixed("unit.kk", &unit("$10 = 0"));
    assert!(!codes.contains(&"K816-E0176"), "{codes:?}");
}

/// `loop`, `push` and `pull` are keywords only where their line says so — a
/// `{` after `loop`, an operand after `push` or `pull` — so a routine of each
/// name is declared, called by its bare name and jumped to as any other.
#[test]
fn a_routine_may_be_called_loop_push_or_pull() {
    let text = "\
func loop @a8 @i8 {
    nop
}

func push @a8 @i8 {
    nop
}

func pull @a8 @i8 {
    nop
}

func work @a8 @i8 {
    loop
    push
    pull
    jsr push
    jmp loop
}
";
    let (codes, lines) = code("unit.kk", text);
    assert_eq!(codes, Vec::<&str>::new());
    for wanted in ["jsr a:loop", "jsr a:push", "jsr a:pull", "jmp a:loop"] {
        assert!(
            lines
                .iter()
                .any(|line| line.split_whitespace().collect::<Vec<_>>().join(" ") == wanted),
            "no `{wanted}` in {lines:#?}"
        );
    }
}

/// A bare number in HLA is a value, in either dialect: `a + 1` adds one. A
/// compatibility fixture's note once said it read the byte at `$0001`, and
/// nothing checked the claim.
#[test]
fn a_bare_number_in_hla_is_a_value_in_either_dialect() {
    for name in ["unit.kk", "unit.k65"] {
        let (codes, lines) = code(
            name,
            "func work @a8 @i8 {\n    a + 1\n    a = $10\n    x?5\n    a & $0F\n}\n",
        );
        assert_eq!(codes, Vec::<&str>::new(), "{name}");
        let instructions: Vec<String> = lines
            .iter()
            .map(|line| line.split_whitespace().collect::<Vec<_>>().join(" "))
            .collect();
        for wanted in ["adc #$01", "lda #$10", "cpx #$05", "and #$0F"] {
            assert!(
                instructions.iter().any(|line| line == wanted),
                "{name}: no `{wanted}`"
            );
        }
    }
}

/// The help a K816 source is given names the K816 spelling: before sub-plan
/// O it said `p??`, `v-?` and `dbr!!`, which a K816 source now refuses.
#[test]
fn the_help_a_k816_source_gets_names_k816s_spelling() {
    for (body, code, wanted) in [
        ("z??", "K816-K65-0005", "pull p"),
        ("o-", "K816-K65-0006", "!v?"),
        ("b!!", "K816-E0158", "push dbr"),
    ] {
        let mut map = SourceMap::new();
        let text = format!("func work @a8 @i8 {{\n    {body}\n}}\n");
        let file = map.add_file("unit.kk", text.as_str());
        let result = compile_to_ca65(&map, file, &Options::default());
        let diagnostic = result
            .diagnostics
            .iter()
            .find(|diagnostic| diagnostic.code.as_str() == code)
            .unwrap_or_else(|| panic!("`{body}` did not report {code}"));
        let help = diagnostic.help.as_deref().unwrap_or_default();
        assert!(help.contains(wanted), "`{body}`: {help}");
    }
}

/// An HLA operand runs on across a line break and a `;` ends it, `push`'s
/// constants like every other: `push LIMIT` above `* 3` pushes `LIMIT * 3`,
/// as `a = LIMIT` above it loads it. Sub-plan O first ended a pushed constant
/// at its line, which made `push` the one HLA statement that read the line
/// break; the user: "If you want nops, you need to put ; between".
#[test]
fn an_hla_operand_runs_on_across_a_line_and_a_semicolon_ends_it() {
    let program = |body: &str| format!("const LIMIT = 2\n\nfunc work @a16 @i16 {{\n{body}\n}}\n");
    let instructions = |body: &str| -> Vec<String> {
        code("unit.kk", &program(body))
            .1
            .iter()
            .map(|line| line.split_whitespace().collect::<Vec<_>>().join(" "))
            .filter(|line| ["pea", "lda", "nop"].iter().any(|m| line.starts_with(m)))
            .collect()
    };
    assert_eq!(instructions("    push LIMIT\n    * 3"), ["pea $0006"]);
    assert_eq!(instructions("    a = LIMIT\n    * 3"), ["lda #$0006"]);
    assert_eq!(
        instructions("    push LIMIT; * 3"),
        ["pea $0002", "nop", "nop", "nop"]
    );
    assert_eq!(
        instructions("    a = LIMIT; * 3"),
        ["lda #$0002", "nop", "nop", "nop"]
    );
}

/// One pair per row of sub-plan Q's inventory, K816's `if` spelling first and
/// the K65 statements it stands for second (`plans/16-if-statement.md`).
fn if_pairs() -> Vec<(&'static str, &'static str)> {
    vec![
        // A conditional block, with and without `else`.
        ("if >= { x++ }", ">= { x++ }"),
        ("if !c { x++ }", "c-?{ x++ }"),
        ("if == { x++ } else { y++ }", "== { x++ } else { y++ }"),
        // `else if` is the `if` nested in the `else`, to any depth.
        (
            "if == { x++ } else if < { y++ } else { x-- }",
            "== { x++ } else { < { y++ } else { x-- } }",
        ),
        (
            "if == { x++ } else if < { y++ } else if v { x-- }",
            "== { x++ } else { < { y++ } else { <<={ x-- } } }",
        ),
        // The compare in the `if`, on one line or two in K65.
        ("if a?#10 < { x++ }", "a?#10\n < { x++ }"),
        ("if a?#10 < { x++ }", "a?#10 < { x++ }"),
        ("if a?flags+1 >= { x++ }", "a?flags+1\n >= { x++ }"),
        ("if x?#5 != { y++ }", "x?#5\n != { y++ }"),
        ("if a&?flags !z { y++ }", "a&?flags\n z-?{ y++ }"),
        ("if a?#10 <= { x++ }", "a?#10\n <= { x++ }"),
        // … and in an `else if`, where it runs only on that path.
        (
            "if == { x++ } else if a?#20 >= { y++ }",
            "== { x++ } else { a?#20\n >= { y++ } }",
        ),
        // A conditional jump of each kind.
        ("if < goto .out", "< goto .out"),
        ("if c goto .out", "c+? goto .out"),
        ("if a?flags == goto .out", "a?flags\n == goto .out"),
        (
            "loop { x++\n if == break } always",
            "{ x++\n == break } always",
        ),
        (
            "loop { x++\n if != repeat\n y++ } always",
            "{ x++\n != repeat\n y++ } always",
        ),
        (
            "loop { x++\n if x?#5 != repeat\n y++ } always",
            "{ x++\n x?#5\n != repeat\n y++ } always",
        ),
        // The compare in a loop's `while`: the body's last statement.
        ("loop { x++ } while x?#10 <", "{ x++\n x?#10 } <"),
        (
            "nocross loop { y++ } while y?#3 >=",
            "nocross { y++\n y?#3 } >=",
        ),
        (
            "loop { x++\n if == repeat\n y++ } while y?#3 >=",
            "{ x++\n == repeat\n y++\n y?#3 } >=",
        ),
        (
            "loop { x++\n if == break\n y++ } while a?flags !=",
            "{ x++\n == break\n y++\n a?flags } !=",
        ),
        (
            "loop { x++\n @a16 { a = #1 } } while x?#10 <",
            "{ x++\n @a16 { a = #1 }\n x?#10 } <",
        ),
    ]
}

/// Each `if` form compiles to the bytes of the K65 statements it stands for:
/// the builder makes the same AST of both, so nothing after the parser can
/// tell them apart (`plans/16-if-statement.md` decision 7).
#[test]
fn every_if_form_compiles_to_the_bytes_of_its_k65_spelling() {
    for (k816, k65) in if_pairs() {
        assert_eq!(
            code("unit.kk", &unit(k816)),
            code("unit.k65", &unit(k65)),
            "`{k816}` and `{k65}`"
        );
    }
}

/// A `.k65` source takes none of K816's respellings: each `if` form is
/// refused with `K816-E0180`, and applying the fixes writes K65 that
/// compiles to the bytes of the K65 twin (`plans/16-if-statement.md`
/// decision 10). The bare twin itself is still accepted there.
#[test]
fn a_k65_source_refuses_ifs_and_their_fixes_change_no_byte() {
    for (k816, k65) in if_pairs() {
        let (codes, repaired) = fixed("unit.k65", &unit(k816));
        assert!(
            codes.contains(&"K816-E0180"),
            "`{k816}` in a .k65: {codes:?}"
        );
        assert_eq!(
            code("unit.k65", &repaired),
            code("unit.k65", &unit(k65)),
            "`{k816}` fixed to\n{repaired}"
        );
        // The twin is accepted: whatever it is told is not a refusal (`<=`
        // still costs its two branches, `K816-W0157`).
        let told = code("unit.k65", &unit(k65)).0;
        assert!(
            told.iter().all(|code| code.starts_with("K816-W")),
            "`{k65}` in a .k65: {told:?}"
        );
    }
}

/// And a K816 source refuses each bare twin, with `if` as the fix
/// (`K816-K65-0011`).
#[test]
fn a_k816_source_refuses_bare_conditions_and_their_fix_changes_no_byte() {
    for (k816, k65) in if_pairs() {
        // A compare on a line of its own, or in a loop's body, stays where
        // it is under the fix: a different text with the same bytes, which
        // is what is compared.
        let (codes, repaired) = fixed("unit.kk", &unit(k65));
        // A `while` compare's twin is K65's postfix loop, refused as that
        // (`K816-K65-0007`); every other twin has a bare condition.
        let refused = if k816.contains("while") {
            "K816-K65-0007"
        } else {
            "K816-K65-0011"
        };
        assert!(codes.contains(&refused), "`{k65}`: {codes:?}");
        assert_eq!(
            code("unit.kk", &repaired),
            code("unit.kk", &unit(k816)),
            "`{k65}` fixed to\n{repaired}"
        );
    }
}

/// The compare slot holds a compare — `a?`, `x?`, `y?` or `a&?` — in an
/// `if`, an `else if`, before a jump and in a `while`, in either dialect
/// (`K816-E0179`, decision 5).
#[test]
fn a_compare_slot_takes_only_a_compare() {
    for body in [
        "if a+#1 == { x++ }",
        "if == { x++ } else if a-#1 < { y++ }",
        "if a|#1 != goto .out",
        "loop { x++ } while a^#1 !=",
    ] {
        for name in ["unit.kk", "unit.k65"] {
            let (codes, _) = fixed(name, &unit(body));
            assert!(
                codes.contains(&"K816-E0179"),
                "`{body}` in {name}: {codes:?}"
            );
        }
    }
    for body in [
        "if a?#1 == { x++ }",
        "if a&?flags !z goto .out",
        "loop { x++ } while y?#2 <",
    ] {
        let (codes, _) = fixed("unit.kk", &unit(body));
        assert!(!codes.contains(&"K816-E0179"), "`{body}`: {codes:?}");
    }
}

/// A `.k65` source takes none of sub-plan O's respellings either — a `loop`
/// statement, a flag test without its sign, a `push` / `pull` list — and each
/// fix writes the K65 twin's bytes (`plans/16-if-statement.md` decision 11:
/// a `.k65` source accepts what K816 adds and nothing it respells).
#[test]
fn a_k65_source_refuses_k816s_respellings_and_their_fixes_change_no_byte() {
    for (readable, k65) in respelled_pairs() {
        let (codes, repaired) = fixed("unit.k65", &unit(&readable));
        assert!(
            codes.contains(&"K816-E0180"),
            "`{readable}` in a .k65: {codes:?}"
        );
        assert_eq!(
            code("unit.k65", &repaired),
            code("unit.k65", &unit(&k65)),
            "`{readable}` fixed to\n{repaired}"
        );
    }
}

/// Sub-plan R: a flag test by its letter after `if` or `while`, against the
/// K65 statements it stands for — every flag, both senses, every position a
/// condition takes, and after a compare in the slot
/// (`plans/17-flag-tests.md` decision 5).
fn letter_pairs() -> Vec<(String, String)> {
    let mut pairs = Vec::new();
    for flag in ['c', 'z', 'n', 'v'] {
        for (letter, signed) in [
            (format!("{flag}"), format!("{flag}+?")),
            (format!("!{flag}"), format!("{flag}-?")),
        ] {
            pairs.push((
                format!("if {letter} {{ x++ }}"),
                format!("{signed}{{ x++ }}"),
            ));
            pairs.push((
                format!("if {letter} goto .out"),
                format!("{signed} goto .out"),
            ));
            pairs.push((
                format!("loop {{ x++\n if {letter} break }} always"),
                format!("{{ x++\n {signed} break }} always"),
            ));
            pairs.push((
                format!("loop {{ x++\n if {letter} repeat\n y++ }} always"),
                format!("{{ x++\n {signed} repeat\n y++ }} always"),
            ));
            pairs.push((
                format!("loop {{ x++ }} while {letter}"),
                format!("{{ x++ }} {signed}"),
            ));
            pairs.push((
                format!("if a&?flags {letter} {{ y++ }}"),
                format!("a&?flags\n {signed}{{ y++ }}"),
            ));
            pairs.push((
                format!("loop {{ x++ }} while a?flags {letter}"),
                format!("{{ x++\n a?flags }} {signed}"),
            ));
        }
    }
    pairs
}

#[test]
fn every_letter_flag_test_compiles_to_the_bytes_of_its_k65_spelling() {
    let pairs = letter_pairs();
    assert_eq!(pairs.len(), 4 * 2 * 7);
    for (letter, k65) in pairs {
        assert_eq!(
            code("unit.kk", &unit(&letter)),
            code("unit.k65", &unit(&k65)),
            "`{letter}` and `{k65}`"
        );
    }
}

/// Sub-plan R: `c?` after `if` or `while` is refused in K816 with a fix that
/// drops the `?`, and the fix compiles to the bytes of the letter form —
/// every pair of `letter_pairs`, written with the `?`
/// (`plans/17-flag-tests.md` decision 2).
#[test]
fn a_marked_flag_test_is_refused_and_its_fix_drops_the_mark() {
    for (letter, _) in letter_pairs() {
        let marked = ["c", "z", "n", "v"]
            .iter()
            .fold(letter.clone(), |text, flag| {
                text.replace(&format!(" {flag} "), &format!(" {flag}? "))
                    .replace(&format!("!{flag} "), &format!("!{flag}? "))
                    .replace(&format!(" {flag}\n"), &format!(" {flag}?\n"))
                    .replace(&format!("!{flag}\n"), &format!("!{flag}?\n"))
            });
        let marked = if marked.ends_with(|c: char| "cznv".contains(c)) {
            format!("{marked}?")
        } else {
            marked
        };
        assert_ne!(marked, letter, "the `?` was not written into `{letter}`");
        let (codes, repaired) = fixed("unit.kk", &unit(&marked));
        assert!(codes.contains(&"K816-E0182"), "`{marked}`: {codes:?}");
        assert_eq!(
            code("unit.kk", &repaired),
            code("unit.kk", &unit(&letter)),
            "`{marked}` fixed to\n{repaired}"
        );
    }
}

/// Where a K65 flag test stands with no `if` — `c+?{ x++ }` — the condition's
/// `K816-K65-0011` writes `if` and the letter in one edit, and the flag
/// test's own refusal offers no fix: two fixes over one span would collide,
/// and either alone would leave source that does not parse.
#[test]
fn a_bare_k65_flag_test_is_fixed_once() {
    for body in ["c+?{ x++ }", "z-? goto .out", "<<={ x++ }"] {
        let mut map = SourceMap::new();
        let file = map.add_file("unit.kk", unit(body));
        let result = compile_to_ca65(&map, file, &Options::default());
        let fixes: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| !d.fixes.is_empty())
            .map(|d| d.code.as_str())
            .collect();
        assert_eq!(fixes, ["K816-K65-0011"], "`{body}`");
        let (_, repaired) = fixed("unit.kk", &unit(body));
        assert_eq!(
            code("unit.kk", &repaired).0,
            Vec::<&str>::new(),
            "`{body}` → {repaired}"
        );
    }
}
