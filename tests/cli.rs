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
        .stdout(contains("link"))
        .stdout(contains("lsp"));
}

#[test]
fn help_flag_prints_help() {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.arg("--help")
        .assert()
        .success()
        .stdout(contains("High-level assembler for the WDC 65816"))
        .stdout(contains("Usage: k816"))
        .stdout(contains("--listing"))
        .stdout(contains("compile"))
        .stdout(contains("link"))
        .stdout(contains("lsp"));
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
fn compile_subcommand_rejects_link_only_options() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-compile-link-only-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.arg("compile")
        .arg(&input)
        .arg("--listing")
        .assert()
        .failure()
        .stderr(contains("link-only option"));
}

#[test]
fn build_command_defaults_to_xex_output() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-build-xex-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");
    let out_file = root.join("demo.xex");

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.arg(&input).assert().success();

    assert!(out_file.exists());
    let xex = std::fs::read(&out_file).expect("failed to read xex output");
    assert!(xex.starts_with(&[0xFF, 0xFF]));
}

#[test]
fn build_command_accepts_output_option_for_linked_artifact() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-build-output-option-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");
    let out_file = root.join("custom-output.bin");

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.arg(&input).arg("-o").arg(&out_file).assert().success();

    assert!(out_file.exists());
}

#[test]
fn build_command_writes_auto_listing_when_requested() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-build-listing-auto-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.arg(&input).arg("--listing").assert().success();

    let out_file = root.join("demo.xex");
    let listing_file = root.join("demo.lst");
    assert!(out_file.exists());
    assert!(listing_file.exists());
}

#[test]
fn build_command_writes_listing_to_explicit_path_when_requested() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-build-listing-explicit-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");
    let listing_file = root.join("custom.lst");

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.arg(&input)
        .arg("--listing")
        .arg(&listing_file)
        .assert()
        .success();

    let out_file = root.join("demo.xex");
    assert!(out_file.exists());
    assert!(listing_file.exists());
}

#[test]
fn build_command_accepts_config_parameter() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-build-config-param-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");

    let config = root.join("custom.ld.ron");
    std::fs::write(
        &config,
        r#"(
  format: "o65-link",
  target: Some("cli-build-config-param"),
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
      id: "DEFAULT",
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
    file: Some("from-config.bin"),
  ),
  entry: None,
)"#,
    )
    .expect("failed to write linker config");

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.arg("-T").arg(&config).arg(&input).assert().success();

    assert!(root.join("from-config.bin").exists());
}

#[test]
fn build_infers_xex_from_output_extension_when_switch_not_present() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-build-ext-xex-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");

    let config = root.join("custom.ld.ron");
    std::fs::write(
        &config,
        r#"(
  format: "o65-link",
  target: Some("cli-build-ext-xex"),
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
      id: "DEFAULT",
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
    file: Some("from-config.xex"),
  ),
  entry: None,
)"#,
    )
    .expect("failed to write linker config");

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.arg("-T").arg(&config).arg(&input).assert().success();

    let linked = std::fs::read(root.join("from-config.xex")).expect("failed to read linked output");
    assert!(linked.starts_with(&[0xFF, 0xFF]));
}

#[test]
fn build_output_format_switch_overrides_extension() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-build-format-switch-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");

    let config = root.join("custom.ld.ron");
    std::fs::write(
        &config,
        r#"(
  format: "o65-link",
  target: Some("cli-build-format-switch"),
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
      id: "DEFAULT",
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
    file: Some("from-config.bin"),
  ),
  entry: None,
)"#,
    )
    .expect("failed to write linker config");

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.arg("--output-format")
        .arg("xex")
        .arg("-T")
        .arg(&config)
        .arg(&input)
        .assert()
        .success();

    let linked = std::fs::read(root.join("from-config.bin")).expect("failed to read linked output");
    assert!(linked.starts_with(&[0xFF, 0xFF]));
}

#[test]
fn build_command_uses_adjacent_config_and_rejects_ambiguous_raw_binary() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-build-adjacent-config-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(
        &input,
        "main {\n  nop\n}\n\ndata INFO {\n  segment INFO\n  \"I\"\n}\n",
    )
    .expect("failed to write input");

    let config = root.join("demo.ld.ron");
    std::fs::write(
        &config,
        r#"(
  format: "o65-link",
  target: Some("cli-build-adjacent-config"),
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
      id: "DEFAULT",
      load: "MAIN",
      run: None,
      align: Some(1),
      start: Some(0x0200),
      offset: None,
      optional: false,
      segment: Some("default"),
    ),
    (
      id: "INFO",
      load: "MAIN",
      run: None,
      align: Some(1),
      start: Some(0xFC00),
      offset: None,
      optional: false,
      segment: Some("INFO"),
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

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_k816"));
    cmd.arg(&input)
        .assert()
        .failure()
        .stderr(contains("raw binary output is ambiguous"));
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
    let config = root.join("link.ld.ron");

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
      id: "DEFAULT",
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
fn link_infers_xex_from_output_extension_when_switch_not_present() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-link-ext-xex-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");
    let object_file = root.join("demo.o65");
    let out_file = root.join("game.xex");
    let config = root.join("link.ld.ron");

    std::fs::write(
        &config,
        r#"(
  format: "o65-link",
  target: Some("cli-link-ext-xex"),
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
      id: "DEFAULT",
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
        .arg("-o")
        .arg(&out_file)
        .assert()
        .success();

    let linked = std::fs::read(&out_file).expect("failed to read linked output");
    assert!(linked.starts_with(&[0xFF, 0xFF]));
}

#[test]
fn link_output_format_switch_overrides_extension() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-cli-link-format-switch-{unique}"));
    std::fs::create_dir_all(&root).expect("failed to create temp root");

    let input = root.join("demo.k65");
    std::fs::write(&input, "main {\n  nop\n}\n").expect("failed to write input");
    let object_file = root.join("demo.o65");
    let out_file = root.join("game.bin");
    let config = root.join("link.ld.ron");

    std::fs::write(
        &config,
        r#"(
  format: "o65-link",
  target: Some("cli-link-format-switch"),
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
      id: "DEFAULT",
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
    link.arg("--output-format")
        .arg("xex")
        .arg("link")
        .arg(&object_file)
        .arg("-T")
        .arg(&config)
        .arg("-o")
        .arg(&out_file)
        .assert()
        .success();

    let linked = std::fs::read(&out_file).expect("failed to read linked output");
    assert!(linked.starts_with(&[0xFF, 0xFF]));
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
    let config = config_dir.join("link.ld.ron");

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
      id: "DEFAULT",
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
    let config = root.join("link.ld.ron");

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
      id: "DEFAULT",
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

#[test]
fn init_scaffolds_project_and_builds_default_target() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let workspace = std::env::temp_dir().join(format!("k816-cli-project-init-{unique}"));
    std::fs::create_dir_all(&workspace).expect("failed to create temp root");

    let mut init = Command::new(env!("CARGO_BIN_EXE_k816"));
    init.current_dir(&workspace)
        .arg("init")
        .arg("hello")
        .assert()
        .success();

    let project = workspace.join("hello");
    assert!(project.join("k816.toml").exists());
    assert!(project.join("src/main.k65").exists());
    assert!(project.join("link.ron").exists());
    assert!(project.join(".gitignore").exists());

    let mut build = Command::new(env!("CARGO_BIN_EXE_k816"));
    build.current_dir(&project).arg("build").assert().success();

    assert!(project.join("target/debug/hello.xex").exists());
    assert!(project.join("target/debug/obj/main.o65").exists());
}

#[test]
fn build_discovers_nested_k65_sources() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let workspace = std::env::temp_dir().join(format!("k816-cli-project-discovery-{unique}"));
    std::fs::create_dir_all(&workspace).expect("failed to create temp root");

    let mut init = Command::new(env!("CARGO_BIN_EXE_k816"));
    init.current_dir(&workspace)
        .arg("init")
        .arg("discovery")
        .assert()
        .success();

    let project = workspace.join("discovery");
    let nested = project.join("src/modules/extra.k65");
    std::fs::create_dir_all(nested.parent().expect("nested file should have parent"))
        .expect("failed to create nested source dir");
    std::fs::write(&nested, "func helper {\n  nop\n}\n").expect("failed to write nested source");

    let mut build = Command::new(env!("CARGO_BIN_EXE_k816"));
    build.current_dir(&project).arg("build").assert().success();

    assert!(project.join("target/debug/discovery.xex").exists());
    assert!(project.join("target/debug/obj/modules/extra.o65").exists());
}

#[test]
fn run_uses_configured_runner_and_forwards_args() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let workspace = std::env::temp_dir().join(format!("k816-cli-project-run-{unique}"));
    std::fs::create_dir_all(&workspace).expect("failed to create temp root");

    let mut init = Command::new(env!("CARGO_BIN_EXE_k816"));
    init.current_dir(&workspace)
        .arg("init")
        .arg("runner")
        .assert()
        .success();

    let project = workspace.join("runner");
    std::fs::write(
        project.join("k816.toml"),
        r#"[package]
name = "runner"
version = "0.1.0"

[link]
script = "link.ron"

[run]
runner = "bash"
args = ["-c", "printf '%s\n' \"$@\" > runner.args", "--"]
"#,
    )
    .expect("failed to write project manifest");

    let mut run = Command::new(env!("CARGO_BIN_EXE_k816"));
    run.current_dir(&project)
        .arg("run")
        .arg("--")
        .arg("foo")
        .arg("bar")
        .assert()
        .success();

    assert!(project.join("target/debug/runner.xex").exists());

    let recorded = std::fs::read_to_string(project.join("runner.args"))
        .expect("runner should emit forwarded args");
    let mut args = recorded.lines();
    assert_eq!(args.next(), Some("target/debug/runner.xex"));
    assert_eq!(args.next(), Some("foo"));
    assert_eq!(args.next(), Some("bar"));
    assert_eq!(args.next(), None);
}

#[test]
fn run_requires_runner_configuration() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let workspace = std::env::temp_dir().join(format!("k816-cli-project-run-missing-{unique}"));
    std::fs::create_dir_all(&workspace).expect("failed to create temp root");

    let mut init = Command::new(env!("CARGO_BIN_EXE_k816"));
    init.current_dir(&workspace)
        .arg("init")
        .arg("missing-runner")
        .assert()
        .success();

    let project = workspace.join("missing-runner");
    let mut run = Command::new(env!("CARGO_BIN_EXE_k816"));
    run.current_dir(&project)
        .arg("run")
        .assert()
        .failure()
        .stderr(contains(
            "No runner configured. Set [run].runner in k816.toml.",
        ));
}

#[test]
fn clean_removes_project_target_directory() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let workspace = std::env::temp_dir().join(format!("k816-cli-project-clean-{unique}"));
    std::fs::create_dir_all(&workspace).expect("failed to create temp root");

    let mut init = Command::new(env!("CARGO_BIN_EXE_k816"));
    init.current_dir(&workspace)
        .arg("init")
        .arg("cleanup")
        .assert()
        .success();

    let project = workspace.join("cleanup");
    let mut build = Command::new(env!("CARGO_BIN_EXE_k816"));
    build.current_dir(&project).arg("build").assert().success();
    assert!(project.join("target").exists());

    let mut clean = Command::new(env!("CARGO_BIN_EXE_k816"));
    clean.current_dir(&project).arg("clean").assert().success();
    assert!(!project.join("target").exists());
}
