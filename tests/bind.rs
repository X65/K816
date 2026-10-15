/*****************************************************************************/
/*                                                                           */
/*                                  bind.rs                                  */
/*                                                                           */
/*          `k816 bind`: the K816 side of a program's ca65 modules           */
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

//! `k816 bind` reads what ld65 recorded about a program's ca65 modules and
//! writes the K816 declarations a unit beside them needs. The claims here
//! are checked against the real tools: a two-module ca65 program is
//! assembled with `-g` and linked with `--dbgfile`, `bind` is run over the
//! result, and the declarations it writes are compiled by K816 and *linked
//! against the very objects they describe* — which is the only test of an
//! `extern` that means anything.

mod common;

use std::path::{Path, PathBuf};
use std::process::Command;

use common::{run_ok, tool, work_dir};

const CONFIG: &str = "\
MEMORY {
    ZP:  start = $0000, size = $0100;
    RAM: start = $8000, size = $7000, file = %O;
}
SEGMENTS {
    ZEROPAGE: load = ZP,  type = zp, optional = yes;
    CODE:     load = RAM, type = ro;
    RODATA:   load = RAM, type = ro, optional = yes;
    DATA:     load = RAM, type = rw, optional = yes;
    BSS:      load = RAM, type = bss, optional = yes;
}
";

/// The library every program here links: one of each kind of symbol a
/// ca65 module can export, and two `.struct`s.
const LIB: &str = "\
.setcpu \"65816\"
.export lib_fn, lib_table, lib_counter
.export K = 25
.exportzp lib_zp

.struct REGS
    .org $FF80
    in0  .byte
    out0 .byte
    pair .word
.endstruct

.struct REC
    tag   .byte
    count .word
    name  .byte 4
.endstruct

.zeropage
lib_zp:      .res 3

.data
lib_counter: .res 2

.rodata
lib_table:   .byte 1, 2, 3

.code
.a16
.i8
lib_fn:
    rts
lib_private:
    rts
";

/// A ca65 module that imports every kind.
const APP: &str = "\
.setcpu \"65816\"
.import lib_fn, lib_table, lib_counter
.importzp lib_zp, K
.export start
.code
start:
    jsr lib_fn
    lda lib_table
    lda #K
    lda lib_zp
    lda lib_counter
    rts
";

/// A program's build: the sources at their paths, the objects, and the
/// debug file.
struct Build {
    dir: PathBuf,
    app: PathBuf,
    objects: Vec<PathBuf>,
    dbg: PathBuf,
}

fn assemble(dir: &Path, source: &Path, debug: bool) -> PathBuf {
    let object = dir
        .join(source.file_stem().expect("a stem"))
        .with_extension("o");
    let mut command = Command::new(tool("CA65", "ca65"));
    command.arg("--cpu").arg("65816");
    if debug {
        command.arg("-g");
    }
    command.arg(source).arg("-o").arg(&object);
    run_ok(&mut command, "ca65");
    object
}

fn link(dir: &Path, objects: &[PathBuf], name: &str) -> PathBuf {
    let config = dir.join("link.cfg");
    std::fs::write(&config, CONFIG).expect("write the linker config");
    let dbg = dir.join(format!("{name}.dbg"));
    run_ok(
        Command::new(tool("LD65", "ld65"))
            .arg("-C")
            .arg(&config)
            .arg("-o")
            .arg(dir.join(format!("{name}.bin")))
            .arg("--dbgfile")
            .arg(&dbg)
            .args(objects),
        "ld65",
    );
    dbg
}

/// The two-module ca65 program, built with debug information.
fn ca65_program(test: &str) -> Build {
    let dir = work_dir(test);
    let lib = dir.join("lib.asm");
    let app = dir.join("app.asm");
    std::fs::write(&lib, LIB).expect("write lib.asm");
    std::fs::write(&app, APP).expect("write app.asm");
    let objects = vec![assemble(&dir, &app, true), assemble(&dir, &lib, true)];
    let dbg = link(&dir, &objects, "program");
    Build {
        dir,
        app,
        objects,
        dbg,
    }
}

/// Runs `k816 bind` with `args`, returning stdout and whether it succeeded;
/// stderr is panicked with on failure unless `expect_failure`.
fn bind(args: &[&str], expect_failure: bool) -> String {
    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("bind")
        .args(args)
        .output()
        .expect("run k816 bind");
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    if expect_failure {
        assert!(!output.status.success(), "bind succeeded: {stderr}");
        return stderr;
    }
    assert!(output.status.success(), "bind failed: {stderr}");
    String::from_utf8(output.stdout).expect("UTF-8")
}

/// Compiles a K816 unit at `path` to assembly beside it.
fn compile(path: &Path) -> PathBuf {
    let assembly = path.with_extension("s");
    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("compile")
        .arg(path)
        .arg("-o")
        .arg(&assembly)
        .output()
        .expect("run k816 compile");
    assert!(
        output.status.success(),
        "k816 compile {} failed:\n{}",
        path.display(),
        String::from_utf8_lossy(&output.stderr)
    );
    assembly
}

/// The declaration lines of a generated unit: no comments, no blanks,
/// trailing comments stripped.
fn declarations(text: &str) -> Vec<String> {
    text.lines()
        .map(|line| line.split("//").next().unwrap_or("").trim_end().to_owned())
        .filter(|line| !line.is_empty())
        .collect()
}

/// `--for` writes exactly what that module imports, each with the shape
/// ld65 recorded: a function, a data label, a zero-page label with a size,
/// a sized data label, and an equate.
#[test]
fn an_interface_for_one_module_declares_exactly_what_it_imports() {
    let build = ca65_program("bind-for");
    let text = bind(
        &[
            "--for",
            build.app.to_str().unwrap(),
            build.dbg.to_str().unwrap(),
        ],
        false,
    );
    let lines = declarations(&text);
    for expected in [
        "extern func lib_fn",
        "extern var lib_table",
        "extern var lib_counter : word",
        "extern var dp lib_zp : far",
        // `.export K = 25` is zeropage to ca65, and the import says so.
        "extern const K:byte",
    ] {
        assert!(
            lines.iter().any(|line| line == expected),
            "missing `{expected}` in:\n{text}"
        );
    }
    assert!(
        !text.contains("lib_private"),
        "a symbol the module does not import was declared:\n{text}"
    );
    // The function's comment quotes the ca65 width state above it, and the
    // declaration itself carries no contract: a width is a claim this tool
    // has no basis to make.
    let function = text
        .lines()
        .find(|line| line.starts_with("extern func lib_fn"))
        .expect("the function line");
    assert!(
        function.contains("ca65 state above it: .a16 .i8"),
        "no width hint: {function}"
    );
    assert!(
        !function.contains('@'),
        "a contract was invented: {function}"
    );
    // And where it was defined.
    assert!(function.contains("lib.asm:"), "{function}");
}

/// The generated interface compiles, and the object K816 produces over it
/// links against the ca65 objects it describes with every address size
/// agreed — which is the one thing an `extern` is for.
#[test]
fn the_generated_interface_links_against_the_objects_it_describes() {
    let build = ca65_program("bind-link");
    let interface = build.dir.join("iface.k816");
    bind(
        &[
            "--for",
            build.app.to_str().unwrap(),
            build.dbg.to_str().unwrap(),
            "-o",
            interface.to_str().unwrap(),
        ],
        false,
    );
    assert!(interface.exists());

    let unit = build.dir.join("use.k816");
    std::fs::write(
        &unit,
        "import \"iface.k816\"\n\
         func main @a8 @i8 {\n\
         \x20   jsr lib_fn\n\
         \x20   lda lib_table\n\
         \x20   lda #K\n\
         \x20   sta lib_table\n\
         \x20   lda [lib_zp]\n\
         \x20   @a16 { lda lib_counter }\n\
         \x20   return\n\
         }\n",
    )
    .expect("write use.k816");
    let assembly = compile(&unit);
    let text = std::fs::read_to_string(&assembly).expect("read the assembly");
    // `lda #K` is stored before the next load, so the optimizer keeps it and
    // ca65 has to accept it: against the absolute `.import K` this tool used
    // to write, an 8-bit immediate was a range error nothing assembled.
    for directive in [".import lib_fn", ".importzp lib_zp", ".importzp K", "lda     #K"] {
        assert!(text.contains(directive), "no `{directive}` in:\n{text}");
    }
    let object = assemble(&build.dir, &assembly, false);
    // `lib.o` is the second object; `app.o` defines `start`, which nothing
    // here needs, and is left out.
    let objects = vec![object, build.objects[1].clone()];
    link(&build.dir, &objects, "mixed");
    // And agreed is checked, not assumed: ld65 links a mismatched address
    // size with a warning, which is what an absolute `K` against ca65's
    // zeropage export was until `extern const` could say `:byte`.
    let output = Command::new(tool("LD65", "ld65"))
        .arg("-C")
        .arg(build.dir.join("link.cfg"))
        .arg("-o")
        .arg(build.dir.join("mixed-quiet.bin"))
        .args(&objects)
        .output()
        .expect("run ld65");
    let said = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success() && said.is_empty(),
        "ld65 said:\n{said}"
    );
}

/// Without `--for`, every export some module uses, grouped by the file
/// that defines it; with `--all`, the unused ones too.
#[test]
fn without_a_module_every_used_export_is_declared_and_all_adds_the_rest() {
    let build = ca65_program("bind-all");
    let used = bind(&[build.dbg.to_str().unwrap()], false);
    assert!(used.contains("// From lib.asm"), "{used}");
    assert!(declarations(&used).contains(&"extern func lib_fn".to_owned()));
    assert!(!used.contains("lib_private"), "{used}");
    // `start` is exported by `app.asm` and imported by nobody.
    assert!(!used.contains("extern func start"), "{used}");

    let all = bind(&["--all", build.dbg.to_str().unwrap()], false);
    assert!(all.contains("extern func lib_private"), "{all}");
    assert!(all.contains("extern func start"), "{all}");
}

/// `--layouts` writes a `.struct` with an `.org` as a fixed-address field
/// layout, one without as an `abstract` layout, and `--flat` the former as
/// one equate per member — the form that an importing unit can use.
#[test]
fn layouts_come_out_fixed_abstract_or_flat_and_compile() {
    let build = ca65_program("bind-layouts");
    let layouts = bind(&["--layouts", build.dbg.to_str().unwrap()], false);
    let lines = declarations(&layouts);
    let fixed = lines
        .iter()
        .position(|line| line == "var REGS[")
        .expect("the fixed layout");
    assert_eq!(
        &lines[fixed..fixed + 5],
        &[
            "var REGS[",
            "    .in0 :byte",
            "    .out0 :byte",
            "    .pair :word",
            "] = $FF80",
        ],
        "{layouts}"
    );
    let abstract_ = lines
        .iter()
        .position(|line| line == "abstract var REC[")
        .expect("the abstract layout");
    assert_eq!(
        &lines[abstract_..abstract_ + 5],
        &[
            "abstract var REC[",
            "    .tag :byte",
            "    .count :word",
            "    .name :byte [4]",
            "]",
        ],
        "{layouts}"
    );

    // Both compile as a unit, and the abstract one crosses an import.
    let file = build.dir.join("layouts.k816");
    std::fs::write(&file, &layouts).expect("write layouts.k816");
    compile(&file);
    let unit = build.dir.join("sizes.k816");
    std::fs::write(
        &unit,
        "import \"layouts.k816\"\nfunc main @a8 @i8 {\n    lda #REC:sizeof\n    ldx #REC.name:offsetof\n    return\n}\n",
    )
    .expect("write sizes.k816");
    let text = std::fs::read_to_string(compile(&unit)).expect("read");
    assert!(text.contains("lda     #$07"), "{text}");
    assert!(text.contains("ldx     #$03"), "{text}");

    // Flat: one exported equate per member, which an importer reads by name.
    let flat = bind(&["--layouts", "--flat", build.dbg.to_str().unwrap()], false);
    let lines = declarations(&flat);
    for expected in [
        "var REGS_in0 :byte = $FF80",
        "var REGS_out0:byte = $FF81",
        "var REGS_pair:word = $FF82",
        "abstract var REC[",
    ] {
        assert!(
            lines
                .iter()
                .any(|line| line.split_whitespace().collect::<Vec<_>>()
                    == expected.split_whitespace().collect::<Vec<_>>()),
            "missing `{expected}` in:\n{flat}"
        );
    }
    let file = build.dir.join("flat.k816");
    std::fs::write(&file, &flat).expect("write flat.k816");
    let unit = build.dir.join("useflat.k816");
    std::fs::write(
        &unit,
        "import \"flat.k816\"\nfunc main @a8 @i8 {\n    lda REGS_in0\n    sta REGS_out0\n    return\n}\n",
    )
    .expect("write useflat.k816");
    let text = std::fs::read_to_string(compile(&unit)).expect("read");
    assert!(text.contains("lda     a:REGS_in0"), "{text}");
}

/// A symbol a K816 unit defines is reached by `import`, never declared: its
/// contract is derived from its body, which is better than anything this
/// tool could write. The path is relative to the generated file.
#[test]
fn a_k816_unit_in_the_link_is_imported_rather_than_declared() {
    let dir = work_dir("bind-mixed");
    let lib = dir.join("lib.asm");
    std::fs::write(&lib, LIB).expect("write lib.asm");
    let k = dir.join("k.k816");
    std::fs::write(&k, "func k_fn @a8 @i8 {\n    nop\n    return\n}\n").expect("write k.k816");
    let app = dir.join("app2.asm");
    std::fs::write(
        &app,
        ".setcpu \"65816\"\n.import k_fn, lib_fn\n.export start\n.code\nstart:\n    jsr k_fn\n    jsr lib_fn\n    rts\n",
    )
    .expect("write app2.asm");
    let objects = vec![
        assemble(&dir, &app, true),
        assemble(&dir, &compile(&k), true),
        assemble(&dir, &lib, true),
    ];
    let dbg = link(&dir, &objects, "mixed");

    let sub = dir.join("sub");
    std::fs::create_dir_all(&sub).expect("mkdir sub");
    let out = sub.join("iface.k816");
    bind(
        &[
            "--for",
            app.to_str().unwrap(),
            dbg.to_str().unwrap(),
            "-o",
            out.to_str().unwrap(),
        ],
        false,
    );
    let text = std::fs::read_to_string(&out).expect("read the interface");
    assert!(text.contains("import \"../k.k816\""), "{text}");
    assert!(
        !text.contains("k_fn"),
        "a K816 symbol was declared extern:\n{text}"
    );
    assert!(
        declarations(&text).contains(&"extern func lib_fn".to_owned()),
        "{text}"
    );

    // `--for` finds the K816 module by either of its spellings.
    let by_k816 = bind(&["--for", "k.k816", dbg.to_str().unwrap()], false);
    assert!(
        declarations(&by_k816).is_empty(),
        "k.k816 imports nothing:\n{by_k816}"
    );
}

/// A debug file from a build without `-g` names no symbols, and the
/// message says which flag to add rather than writing an empty interface.
#[test]
fn a_debug_file_without_symbols_names_the_flag_to_add() {
    let dir = work_dir("bind-no-g");
    let lib = dir.join("lib.asm");
    let app = dir.join("app.asm");
    std::fs::write(&lib, LIB).expect("write lib.asm");
    std::fs::write(&app, APP).expect("write app.asm");
    let objects = vec![assemble(&dir, &app, false), assemble(&dir, &lib, false)];
    let dbg = link(&dir, &objects, "program");
    let stderr = bind(&[dbg.to_str().unwrap()], true);
    assert!(stderr.contains("-g"), "{stderr}");

    // And a module the program was not built from is named with what the
    // program was built from.
    let build = ca65_program("bind-no-module");
    let stderr = bind(&["--for", "nothing.asm", build.dbg.to_str().unwrap()], true);
    assert!(
        stderr.contains("nothing.asm") && stderr.contains("lib.asm"),
        "{stderr}"
    );
}
