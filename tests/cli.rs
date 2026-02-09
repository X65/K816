use assert_cmd::Command;
use predicates::str::contains;

#[test]
fn no_args_prints_banner_and_help() {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.assert()
        .success()
        .stdout(contains("K816 HLA, version"))
        .stdout(contains("Usage: k816"))
        .stdout(contains("INPUT"));
}

#[test]
fn help_flag_prints_help() {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.arg("--help")
        .assert()
        .success()
        .stdout(contains("High-level assembler for the WDC 65816"))
        .stdout(contains("Usage: k816"));
}
