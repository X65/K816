use assert_cmd::Command;
use predicates::str::contains;
use std::time::{SystemTime, UNIX_EPOCH};

#[test]
fn no_args_prints_banner_and_help() {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.assert()
        .success()
        .stdout(contains("K816 HLA, version"))
        .stdout(contains("Usage: k816"))
        .stdout(contains("compile"))
        .stdout(contains("link"));
}

#[test]
fn help_flag_prints_help() {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.arg("--help")
        .assert()
        .success()
        .stdout(contains("High-level assembler for the WDC 65816"))
        .stdout(contains("Usage: k816"))
        .stdout(contains("compile"))
        .stdout(contains("link"));
}

#[test]
fn compile_and_link_subcommands_work() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");

    let object_dir = root.join("demo.k816obj");
    let out_base = root.join("game");

    let mut compile = Command::new(env!("CARGO_BIN_EXE_k816"));
    compile
        .arg("compile")
        .arg(&input)
        .arg("-o")
        .arg(&object_dir)
        .assert()
        .success();
    assert!(object_dir.join("manifest.txt").exists());

    let mut link = Command::new(env!("CARGO_BIN_EXE_k816"));
    link.arg("link")
        .arg(&object_dir)
        .arg("-o")
        .arg(&out_base)
        .assert()
        .success();

    assert!(root.join("game.bin").exists());
    assert!(root.join("game.lst").exists());
}
