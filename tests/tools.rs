/*****************************************************************************/
/*                                                                           */
/*                                  tools.rs                                 */
/*                                                                           */
/*       The conversion tools under tools/, run over programs they read      */
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

//! The two Python tools under `tools/` are part of the ca65-to-K816
//! conversion kit and make claims about real programs: which symbols of two
//! builds differ, and what register widths a ca65 source assembled with.
//! They are held to those claims here, over programs the test assembles.
//!
//! They need `python3`. Where it is absent the tests say so and pass,
//! because the compiler's own gates must not depend on a second language's
//! interpreter being installed; where it is present — every machine that
//! runs the conversion, and the CI — they run.

mod common;

use std::path::{Path, PathBuf};
use std::process::Command;

use common::{run_ok, tool, work_dir};

fn python() -> Option<PathBuf> {
    let candidate = PathBuf::from("python3");
    Command::new(&candidate)
        .arg("--version")
        .output()
        .ok()
        .filter(|output| output.status.success())
        .map(|_| candidate)
}

fn tool_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tools")
        .join(name)
}

/// Runs a tool with `-I` (isolated: no site packages, no current-directory
/// imports), returning stdout and the exit code.
fn run_tool(python: &Path, name: &str, args: &[&str]) -> (String, i32) {
    let output = Command::new(python)
        .arg("-I")
        .arg(tool_path(name))
        .args(args)
        .output()
        .expect("run the tool");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.code().is_some_and(|code| code < 2),
        "{name} failed:\n{stderr}"
    );
    (
        String::from_utf8(output.stdout).expect("UTF-8"),
        output.status.code().unwrap_or(2),
    )
}

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
}
";

/// Assembles and links `sources` (name, text) in `dir`, with debug
/// information; returns the debug file.
fn build(dir: &Path, sources: &[(&str, &str)]) -> PathBuf {
    let mut objects = Vec::new();
    for (name, text) in sources {
        let source = dir.join(name);
        std::fs::write(&source, text).expect("write a source");
        let object = source.with_extension("o");
        run_ok(
            Command::new(tool("CA65", "ca65"))
                .args(["--cpu", "65816", "-g"])
                .arg(&source)
                .arg("-o")
                .arg(&object),
            "ca65",
        );
        objects.push(object);
    }
    let config = dir.join("link.cfg");
    std::fs::write(&config, CONFIG).expect("write the config");
    let dbg = dir.join("program.dbg");
    run_ok(
        Command::new(tool("LD65", "ld65"))
            .arg("-C")
            .arg(&config)
            .arg("-o")
            .arg(dir.join("program.bin"))
            .arg("--dbgfile")
            .arg(&dbg)
            .args(&objects),
        "ld65",
    );
    dbg
}

const LIB_BEFORE: &str = "\
.setcpu \"65816\"
.export lib_fn, lib_table, lib_zp_ptr
.zeropage
lib_zp_ptr: .res 3
.rodata
lib_table:  .byte 1, 2, 3
.code
.a8
.i8
lib_fn:
    rts
";

/// `lib_fn` grew a 16-bit load between width masks; a byte of the table
/// changed; the pointer did not.
const LIB_AFTER: &str = "\
.setcpu \"65816\"
.export lib_fn, lib_table, lib_zp_ptr
.zeropage
lib_zp_ptr: .res 3
.rodata
lib_table:  .byte 1, 9, 3
.code
.a8
.i8
lib_fn:
    rep #$20
    .a16
    lda #$1234
    sep #$20
    .a8
    rts
";

const APP: &str = "\
.setcpu \"65816\"
.import lib_fn, lib_table
.importzp lib_zp_ptr
.export start
.code
start:
    jsr lib_fn
    lda lib_table
    lda [lib_zp_ptr]
    rts
";

/// The compare tool names every symbol whose bytes changed and no other,
/// shows code as width-tracked instructions and data as bytes, and exits
/// non-zero exactly when something differs.
#[test]
fn the_compare_tool_reports_the_changed_symbols_as_instructions_or_bytes() {
    let Some(python) = python() else {
        eprintln!("python3 not found: the tools are not exercised on this machine");
        return;
    };
    let old = build(
        &work_dir("tools-compare-old"),
        &[("app.asm", APP), ("lib.asm", LIB_BEFORE)],
    );
    let new = build(
        &work_dir("tools-compare-new"),
        &[("app.asm", APP), ("lib.asm", LIB_AFTER)],
    );

    let (report, code) = run_tool(
        &python,
        "k816-compare.py",
        &[old.to_str().unwrap(), new.to_str().unwrap()],
    );
    assert_eq!(code, 1, "{report}");
    // The routine: both sides disassembled, and the 16-bit immediate read
    // as one because the `rep #$20` before it was followed.
    assert!(
        report.contains("lib_fn") && report.contains("DIFFERS"),
        "{report}"
    );
    assert!(report.contains("LDA #$1234"), "{report}");
    assert!(report.contains("REP #$20"), "{report}");
    // The table: bytes, not instructions.
    let table = report
        .lines()
        .skip_while(|line| !line.starts_with("lib_table"))
        .take(3)
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        table.contains("01 02 03") && table.contains("01 09 03"),
        "{table}"
    );
    assert!(!table.contains("ORA"), "data was disassembled:\n{table}");
    // The pointer did not change and is not listed; `start` did, because
    // the table it names moved.
    assert!(!report.contains("lib_zp_ptr"), "{report}");
    assert!(report.contains("3 differ"), "{report}");

    // The same build twice: nothing differs, and the exit says so.
    let (report, code) = run_tool(
        &python,
        "k816-compare.py",
        &[old.to_str().unwrap(), old.to_str().unwrap()],
    );
    assert_eq!(code, 0, "{report}");
    assert!(report.contains("0 differ"), "{report}");
}

/// The disassembler reads every addressing mode the 65816 has, with the
/// immediate widths the tracked flags say.
#[test]
fn the_disassembler_reads_every_addressing_mode() {
    let Some(python) = python() else {
        eprintln!("python3 not found: the tools are not exercised on this machine");
        return;
    };
    let dir = work_dir("tools-disassemble");
    let source = "\
.setcpu \"65816\"
.export start
.code
start:
    rep #$30
    .a16
    .i16
    lda #$1234
    ldx #$0102
    sep #$30
    .a8
    .i8
    lda #$12
    cpy #$34
    lda $12,s
    lda ($12,s),y
    lda [$12]
    lda [$12],y
    lda f:$123456
    lda f:$123456,x
    lda ($12),y
    lda ($12,x)
    sta a:$1234,y
    mvn #$01,#$02
    pea $1234
    pei ($12)
    phd
    pld
    jmp ($1234)
    jmp ($1234,x)
    jml [$1234]
    bra start
    brl start
    xba
    xce
    cop #$05
    rts
";
    let dbg = build(&dir, &[("all.asm", source)]);
    // Load the tool as a module and disassemble the CODE segment from the
    // widths the test starts at.
    let snippet = format!(
        "import importlib.machinery, importlib.util\n\
         spec = importlib.util.spec_from_loader('cmp', importlib.machinery.SourceFileLoader('cmp', '{tool}'))\n\
         cmp = importlib.util.module_from_spec(spec); spec.loader.exec_module(cmp)\n\
         db = cmp.Database('{dbg}')\n\
         seg = next(s for s in db.segments.values() if s['name'] == 'CODE')\n\
         data = db.image_bytes(None)[cmp.parse_number(seg['id'])]\n\
         w = cmp.Widths(8, 8)\n\
         for at, raw, text in cmp.disassemble(data, cmp.parse_number(seg['start']), w):\n\
         \x20   print(text)\n",
        tool = tool_path("k816-compare.py").to_str().unwrap(),
        dbg = dbg.to_str().unwrap(),
    );
    let output = Command::new(&python)
        .arg("-I")
        .arg("-c")
        .arg(&snippet)
        .output()
        .expect("run python");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let text = String::from_utf8(output.stdout).expect("UTF-8");
    let lines: Vec<&str> = text.lines().collect();
    assert_eq!(
        lines,
        [
            "REP #$30",
            "LDA #$1234",
            "LDX #$0102",
            "SEP #$30",
            "LDA #$12",
            "CPY #$34",
            "LDA $12,s",
            "LDA ($12,s),y",
            "LDA [$12]",
            "LDA [$12],y",
            "LDA $123456",
            "LDA $123456,x",
            "LDA ($12),y",
            "LDA ($12,x)",
            "STA $1234,y",
            "MVN $01,$02",
            "PEA $1234",
            "PEI ($12)",
            "PHD",
            "PLD",
            "JMP ($1234)",
            "JMP ($1234,x)",
            "JML [$1234]",
            "BRA $8000",
            "BRL $8000",
            "XBA",
            "XCE",
            "COP #$05",
            "RTS",
        ],
        "{text}"
    );
}

/// The width annotator reports the state ca65 assembled each label with —
/// sticky directives, masks, `xce` — and the call sites that disagree with
/// their target's definition.
#[test]
fn the_width_annotator_reports_what_ca65_assembled_with_and_who_disagrees() {
    let Some(python) = python() else {
        eprintln!("python3 not found: the tools are not exercised on this machine");
        return;
    };
    let dir = work_dir("tools-widths");
    let kernel = dir.join("kernel.asm");
    std::fs::write(
        &kernel,
        "\
.setcpu \"65816\"
.a16
.i16
wide:
    jsr narrow      ; disagrees: narrow is defined under .a8 .i8
    rts
.a8
.i8
narrow:
    jsr wide        ; disagrees the other way
    rep #$20
    jsr mixed       ; agrees: mixed is defined after the same mask
mixed:
    rts
boot:
    clc
    xce
    sei
    sec
    xce             ; emulation: both forced to 8
emu:
    rts
",
    )
    .expect("write kernel.asm");
    let caller = dir.join("shell.asm");
    std::fs::write(
        &caller,
        ".setcpu \"65816\"\n.import mixed\n.a16\n.i16\nshell:\n    jsr mixed       ; disagrees on the index width\n    rts\n",
    )
    .expect("write shell.asm");

    let (report, code) = run_tool(
        &python,
        "ca65-widths.py",
        &[
            "--calls",
            kernel.to_str().unwrap(),
            caller.to_str().unwrap(),
        ],
    );
    assert_eq!(code, 1, "{report}");
    let line = |label: &str| {
        report
            .lines()
            .find(|line| line.contains(&format!("  {label}:")))
            .unwrap_or_else(|| panic!("no line for {label}:\n{report}"))
            .to_owned()
    };
    assert!(line("wide").ends_with("@a16 @i16"), "{}", line("wide"));
    assert!(line("narrow").ends_with("@a8 @i8"), "{}", line("narrow"));
    assert!(line("mixed").ends_with("@a16 @i8"), "{}", line("mixed"));
    assert!(
        line("emu").ends_with("@a8 @i8 emulation"),
        "{}",
        line("emu")
    );

    let table = report
        .split("Targets whose callers")
        .nth(1)
        .expect("the disagreement table");
    assert!(
        table.contains("narrow  defined") && table.contains("kernel.asm:5 at @a16 @i16"),
        "{table}"
    );
    assert!(
        table.contains("wide  defined") && table.contains("kernel.asm:10 at @a8 @i8"),
        "{table}"
    );
    assert!(
        table.contains("mixed  defined") && table.contains("shell.asm:6 at @a16 @i16"),
        "{table}"
    );
    // A target that disagrees with any caller is shown with every caller,
    // the agreeing one included: the reader decides which side is right.
    assert!(table.contains("kernel.asm:12 at @a16 @i8"), "{table}");

    // A program whose calls all agree exits 0 and says so.
    let agreed = dir.join("agreed.asm");
    std::fs::write(
        &agreed,
        ".setcpu \"65816\"\n.a16\n.i16\nf:\n    jsr g\n    rts\ng:\n    rts\n",
    )
    .expect("write agreed.asm");
    let (report, code) = run_tool(
        &python,
        "ca65-widths.py",
        &["--summary", agreed.to_str().unwrap()],
    );
    assert_eq!(code, 0, "{report}");
    assert!(report.contains("Every call site agrees"), "{report}");
}
