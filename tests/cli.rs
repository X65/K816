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
    let out_file = root.join("game.bin");

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
        .arg(&out_file)
        .assert()
        .success();

    assert!(out_file.exists());
    assert!(!root.join("game.lst").exists());
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
    let out_file = root.join("game.xex");
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
  output: (
    kind: Xex,
    file: None,
  ),
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
        .arg(&out_file)
        .arg("--listing")
        .assert()
        .success();

    assert!(out_file.exists());
    let xex = std::fs::read(&out_file).expect("failed to read xex output");
    assert!(xex.starts_with(&[0xFF, 0xFF]));
    assert!(root.join("game.lst").exists());
}

#[test]
fn link_uses_output_file_from_config_when_o_missing() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-config-out-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");

    let object_file = root.join("demo.o65");
    let config_dir = root.join("cfg");
    std::fs::create_dir_all(&config_dir).expect("failed to create config dir");
    let config = config_dir.join("link.k816ld.ron");

    std::fs::write(
        &config,
        r#"(
  format: "o65-link",
  target: Some("cli-config-output"),
  memory: [
    (
      name: "MAIN",
      start: 0,
      size: 65536,
      kind: ReadWrite,
      fill: Some(0),
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
  output: (
    kind: RawBinary,
    file: Some("game-from-config.bin"),
  ),
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
        .assert()
        .success();

    assert!(config_dir.join("game-from-config.bin").exists());
}

#[test]
fn link_requires_output_file_when_not_provided() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-no-output-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");

    let object_file = root.join("demo.o65");
    let config = root.join("link.k816ld.ron");

    std::fs::write(
        &config,
        r#"(
  format: "o65-link",
  target: Some("cli-no-output"),
  memory: [
    (
      name: "MAIN",
      start: 0,
      size: 65536,
      kind: ReadWrite,
      fill: Some(0),
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
  output: (
    kind: RawBinary,
    file: None,
  ),
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
        .assert()
        .failure()
        .stderr(contains(
            "output file must be provided via linker config output.file or -o",
        ));
}

#[test]
fn link_writes_listing_to_explicit_path() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-listing-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");

    let object_file = root.join("demo.o65");
    let out_file = root.join("game.bin");
    let listing_file = root.join("custom.lst");

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
        .arg(&out_file)
        .arg("--listing")
        .arg(&listing_file)
        .assert()
        .success();

    assert!(out_file.exists());
    assert!(listing_file.exists());
}
