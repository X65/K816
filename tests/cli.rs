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

    let object_file = root.join("demo.o65");
    let out_base = root.join("game");

    let mut compile = Command::new(env!("CARGO_BIN_EXE_k816"));
    compile
        .arg("compile")
        .arg(&input)
        .arg("-o")
        .arg(&object_file)
        .assert()
        .success();
    assert!(object_file.exists());

    let mut link = Command::new(env!("CARGO_BIN_EXE_k816"));
    link.arg("link")
        .arg(&object_file)
        .arg("-o")
        .arg(&out_base)
        .assert()
        .success();

    assert!(root.join("game.main.bin").exists());
    assert!(root.join("game.lst").exists());
}

#[test]
fn link_subcommand_emits_xex_when_configured() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-xex-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");

    let object_file = root.join("demo.o65");
    let out_base = root.join("game");
    let config = root.join("link.k816ld.ron");

    std::fs::write(
        &config,
        r#"(
  format: "o65-link",
  target: Some("cli-xex"),
  memory: [
    (
      name: "MAIN",
      start: 0,
      size: 65536,
      kind: ReadWrite,
      fill: Some(0),
      out_file: Some("main.bin"),
    ),
  ],
  segments: [
    (
      name: "DEFAULT",
      load: "MAIN",
      run: None,
      align: Some(1),
      start: None,
      offset: None,
      optional: false,
      segment: None,
    ),
  ],
  symbols: [],
  outputs: [
    (
      name: "xex",
      kind: Xex,
      default_file: Some("game.xex"),
    ),
  ],
  entry: None,
)"#,
    )
    .expect("failed to write linker config");

    let mut compile = Command::new(env!("CARGO_BIN_EXE_k816"));
    compile
        .arg("compile")
        .arg(&input)
        .arg("-o")
        .arg(&object_file)
        .assert()
        .success();
    assert!(object_file.exists());

    let mut link = Command::new(env!("CARGO_BIN_EXE_k816"));
    link.arg("link")
        .arg(&object_file)
        .arg("-T")
        .arg(&config)
        .arg("-o")
        .arg(&out_base)
        .assert()
        .success();

    let xex_path = root.join("game.game.xex");
    assert!(xex_path.exists());
    let xex = std::fs::read(&xex_path).expect("failed to read xex output");
    assert!(xex.starts_with(&[0xFF, 0xFF]));
    assert!(root.join("game.lst").exists());
}
