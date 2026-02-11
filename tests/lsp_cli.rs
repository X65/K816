use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use serde_json::{Value, json};

struct LspProcess {
    child: Child,
    stdin: ChildStdin,
    messages: Receiver<Value>,
    reader_handle: thread::JoinHandle<()>,
}

impl LspProcess {
    fn spawn() -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_k816"))
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("failed to spawn k816 lsp");

        let stdin = child.stdin.take().expect("stdin");
        let stdout = child.stdout.take().expect("stdout");
        let (tx, rx) = mpsc::channel();
        let reader_handle = thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            while let Ok(message) = read_lsp_message(&mut reader) {
                if tx.send(message).is_err() {
                    break;
                }
            }
        });

        Self {
            child,
            stdin,
            messages: rx,
            reader_handle,
        }
    }

    fn send(&mut self, value: &Value) {
        let payload = serde_json::to_vec(value).expect("serialize json");
        let header = format!("Content-Length: {}\r\n\r\n", payload.len());
        self.stdin
            .write_all(header.as_bytes())
            .expect("write header");
        self.stdin.write_all(&payload).expect("write payload");
        self.stdin.flush().expect("flush lsp request");
    }

    fn recv(&self, timeout: Duration) -> Value {
        self.messages
            .recv_timeout(timeout)
            .expect("timed out waiting for LSP message")
    }

    fn recv_until<F>(&self, timeout: Duration, mut predicate: F) -> Value
    where
        F: FnMut(&Value) -> bool,
    {
        let start = std::time::Instant::now();
        while start.elapsed() < timeout {
            let remaining = timeout
                .checked_sub(start.elapsed())
                .unwrap_or_else(|| Duration::from_millis(1));
            let message = self.recv(remaining);
            if predicate(&message) {
                return message;
            }
        }
        panic!("timed out waiting for expected LSP message");
    }

    fn shutdown(mut self) {
        self.send(&json!({
            "jsonrpc": "2.0",
            "id": 99,
            "method": "shutdown",
            "params": null
        }));
        let _ = self.messages.recv_timeout(Duration::from_secs(3));

        self.send(&json!({
            "jsonrpc": "2.0",
            "method": "exit",
            "params": null
        }));
        drop(self.stdin);

        let mut status = None;
        for _ in 0..50 {
            if let Some(found) = self.child.try_wait().expect("failed to poll lsp process") {
                status = Some(found);
                break;
            }
            thread::sleep(Duration::from_millis(100));
        }
        let status = if let Some(status) = status {
            status
        } else {
            self.child.kill().expect("failed to terminate lsp process");
            self.child
                .wait()
                .expect("failed to wait after terminating lsp process")
        };
        assert!(status.success(), "lsp exited with status {status}");
        let _ = self.reader_handle.join();
    }
}

fn read_lsp_message(reader: &mut impl BufRead) -> std::io::Result<Value> {
    let mut content_length = None;

    loop {
        let mut line = String::new();
        let bytes = reader.read_line(&mut line)?;
        if bytes == 0 {
            return Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "eof while reading lsp headers",
            ));
        }

        if line == "\r\n" {
            break;
        }

        if let Some(value) = line.strip_prefix("Content-Length:") {
            content_length = Some(value.trim().parse::<usize>().map_err(|error| {
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("invalid content length: {error}"),
                )
            })?);
        }
    }

    let Some(content_length) = content_length else {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "missing Content-Length header",
        ));
    };

    let mut body = vec![0_u8; content_length];
    reader.read_exact(&mut body)?;
    let message = serde_json::from_slice::<Value>(&body).map_err(|error| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("invalid json payload: {error}"),
        )
    })?;
    Ok(message)
}

#[test]
fn lsp_initialize_diagnostics_definition_and_hover() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-lsp-it-{unique}"));
    let src_dir = root.join("src");
    std::fs::create_dir_all(&src_dir).expect("failed to create src dir");
    std::fs::write(
        root.join("k816.toml"),
        "[package]\nname = \"lsp-it\"\nversion = \"0.1.0\"\n",
    )
    .expect("failed to write manifest");

    let source = "main {\nstart:\n  call missing\n  bra start\n}\n";
    let source_path = src_dir.join("main.k65");
    std::fs::write(&source_path, source).expect("failed to write source");

    let root_uri = url::Url::from_file_path(&root)
        .expect("root URI")
        .to_string();
    let file_uri = url::Url::from_file_path(&source_path)
        .expect("file URI")
        .to_string();

    let mut lsp = LspProcess::spawn();
    lsp.send(&json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "processId": null,
            "rootUri": root_uri,
            "capabilities": {}
        }
    }));
    let initialize_response = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("id") == Some(&json!(1))
    });
    assert!(initialize_response.get("result").is_some());

    lsp.send(&json!({
        "jsonrpc": "2.0",
        "method": "initialized",
        "params": {}
    }));
    lsp.send(&json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
            "textDocument": {
                "uri": file_uri,
                "languageId": "k65",
                "version": 1,
                "text": source
            }
        }
    }));

    let diagnostics_notification = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("method") == Some(&json!("textDocument/publishDiagnostics"))
            && message.get("params").and_then(|params| params.get("uri")) == Some(&json!(file_uri))
    });
    let diagnostics = diagnostics_notification["params"]["diagnostics"]
        .as_array()
        .expect("diagnostics array");
    assert!(
        diagnostics.iter().any(|diag| diag["message"]
            .as_str()
            .unwrap_or("")
            .contains("unknown function")),
        "expected unknown function diagnostic, got: {diagnostics:?}"
    );

    lsp.send(&json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "textDocument/definition",
        "params": {
            "textDocument": { "uri": file_uri },
            "position": { "line": 3, "character": 8 }
        }
    }));
    let definition_response = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("id") == Some(&json!(2))
    });
    let locations = definition_response["result"]
        .as_array()
        .expect("definition array");
    assert_eq!(locations.len(), 1);
    assert_eq!(locations[0]["range"]["start"]["line"], json!(1));

    lsp.send(&json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "textDocument/hover",
        "params": {
            "textDocument": { "uri": file_uri },
            "position": { "line": 3, "character": 3 }
        }
    }));
    let hover_response = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("id") == Some(&json!(3))
    });
    let hover_text = hover_response["result"]["contents"]["value"]
        .as_str()
        .expect("hover markdown");
    assert!(
        hover_text.contains("opcode"),
        "expected opcode hover, got: {hover_text}"
    );

    lsp.shutdown();
}

#[test]
fn lsp_completion_symbols_and_formatting_idempotence() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-lsp-it-completion-{unique}"));
    let src_dir = root.join("src");
    std::fs::create_dir_all(&src_dir).expect("failed to create src dir");
    std::fs::write(
        root.join("k816.toml"),
        "[package]\nname = \"lsp-it-completion\"\nversion = \"0.1.0\"\n",
    )
    .expect("failed to write manifest");

    let source = "main {\nstart:\ncall start\n}\n";
    let source_path = src_dir.join("main.k65");
    std::fs::write(&source_path, source).expect("failed to write source");

    let root_uri = url::Url::from_file_path(&root)
        .expect("root URI")
        .to_string();
    let file_uri = url::Url::from_file_path(&source_path)
        .expect("file URI")
        .to_string();

    let mut lsp = LspProcess::spawn();
    lsp.send(&json!({
        "jsonrpc": "2.0",
        "id": 10,
        "method": "initialize",
        "params": {
            "processId": null,
            "rootUri": root_uri,
            "capabilities": {}
        }
    }));
    let _ = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("id") == Some(&json!(10))
    });

    lsp.send(&json!({
        "jsonrpc": "2.0",
        "method": "initialized",
        "params": {}
    }));
    lsp.send(&json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
            "textDocument": {
                "uri": file_uri,
                "languageId": "k65",
                "version": 1,
                "text": source
            }
        }
    }));

    let _ = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("method") == Some(&json!("textDocument/publishDiagnostics"))
            && message.get("params").and_then(|params| params.get("uri")) == Some(&json!(file_uri))
    });

    lsp.send(&json!({
        "jsonrpc": "2.0",
        "id": 11,
        "method": "textDocument/completion",
        "params": {
            "textDocument": { "uri": file_uri },
            "position": { "line": 2, "character": 5 }
        }
    }));
    let completion_response = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("id") == Some(&json!(11))
    });
    let completion_items = completion_response["result"]
        .as_array()
        .expect("completion array");
    assert!(
        completion_items
            .iter()
            .any(|item| item["label"].as_str() == Some("start")),
        "expected completion entry for local label, got: {completion_items:?}"
    );

    lsp.send(&json!({
        "jsonrpc": "2.0",
        "id": 12,
        "method": "textDocument/documentSymbol",
        "params": {
            "textDocument": { "uri": file_uri }
        }
    }));
    let symbol_response = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("id") == Some(&json!(12))
    });
    let symbols = symbol_response["result"]
        .as_array()
        .expect("document symbols");
    assert!(
        symbols
            .iter()
            .any(|symbol| symbol["name"].as_str() == Some("main")),
        "expected function symbol, got: {symbols:?}"
    );
    let main_symbol = symbols
        .iter()
        .find(|symbol| symbol["name"].as_str() == Some("main"))
        .expect("main symbol");
    let main_children = main_symbol["children"].as_array().expect("main children");
    assert!(
        main_children
            .iter()
            .any(|symbol| symbol["name"].as_str() == Some("start")),
        "expected nested label symbol, got: {main_children:?}"
    );

    lsp.send(&json!({
        "jsonrpc": "2.0",
        "id": 13,
        "method": "textDocument/formatting",
        "params": {
            "textDocument": { "uri": file_uri },
            "options": { "tabSize": 2, "insertSpaces": true }
        }
    }));
    let formatting_response = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("id") == Some(&json!(13))
    });
    let edits = formatting_response["result"]
        .as_array()
        .expect("formatting edits");
    assert!(!edits.is_empty(), "expected non-empty formatting edits");
    let formatted = edits[0]["newText"]
        .as_str()
        .expect("formatted text")
        .to_string();

    lsp.send(&json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
            "textDocument": {
                "uri": file_uri,
                "version": 2
            },
            "contentChanges": [
                { "text": formatted }
            ]
        }
    }));

    lsp.send(&json!({
        "jsonrpc": "2.0",
        "id": 14,
        "method": "textDocument/formatting",
        "params": {
            "textDocument": { "uri": file_uri },
            "options": { "tabSize": 2, "insertSpaces": true }
        }
    }));
    let formatting_second_response = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("id") == Some(&json!(14))
    });
    let second_edits = formatting_second_response["result"]
        .as_array()
        .expect("second formatting edits");
    assert!(
        second_edits.is_empty(),
        "expected idempotent formatting, got: {second_edits:?}"
    );

    lsp.shutdown();
}

#[test]
fn lsp_did_change_publishes_debounced_diagnostics() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("k816-lsp-it-debounce-{unique}"));
    let src_dir = root.join("src");
    std::fs::create_dir_all(&src_dir).expect("failed to create src dir");
    std::fs::write(
        root.join("k816.toml"),
        "[package]\nname = \"lsp-it-debounce\"\nversion = \"0.1.0\"\n",
    )
    .expect("failed to write manifest");

    let source = "main {\nstart:\n  bra start\n}\n";
    let source_path = src_dir.join("main.k65");
    std::fs::write(&source_path, source).expect("failed to write source");

    let root_uri = url::Url::from_file_path(&root)
        .expect("root URI")
        .to_string();
    let file_uri = url::Url::from_file_path(&source_path)
        .expect("file URI")
        .to_string();

    let mut lsp = LspProcess::spawn();
    lsp.send(&json!({
        "jsonrpc": "2.0",
        "id": 20,
        "method": "initialize",
        "params": {
            "processId": null,
            "rootUri": root_uri,
            "capabilities": {}
        }
    }));
    let _ = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("id") == Some(&json!(20))
    });
    lsp.send(&json!({
        "jsonrpc": "2.0",
        "method": "initialized",
        "params": {}
    }));
    lsp.send(&json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
            "textDocument": {
                "uri": file_uri,
                "languageId": "k65",
                "version": 1,
                "text": source
            }
        }
    }));
    let _ = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("method") == Some(&json!("textDocument/publishDiagnostics"))
            && message.get("params").and_then(|params| params.get("uri")) == Some(&json!(file_uri))
    });

    let changed_source = "main {\nstart:\n  call missing\n}\n";
    lsp.send(&json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
            "textDocument": {
                "uri": file_uri,
                "version": 2
            },
            "contentChanges": [
                { "text": changed_source }
            ]
        }
    }));

    let diagnostics_notification = lsp.recv_until(Duration::from_secs(5), |message| {
        message.get("method") == Some(&json!("textDocument/publishDiagnostics"))
            && message.get("params").and_then(|params| params.get("uri")) == Some(&json!(file_uri))
            && message
                .get("params")
                .and_then(|params| params.get("version"))
                == Some(&json!(2))
    });
    let diagnostics = diagnostics_notification["params"]["diagnostics"]
        .as_array()
        .expect("diagnostics array");
    assert!(
        diagnostics.iter().any(|diag| diag["message"]
            .as_str()
            .unwrap_or("")
            .contains("unknown function")),
        "expected unknown function diagnostic after didChange, got: {diagnostics:?}"
    );

    lsp.shutdown();
}
