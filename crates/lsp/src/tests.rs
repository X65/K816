use std::path::Path;
use std::str::FromStr;

use super::*;
use lsp_types::{DiagnosticSeverity, Uri};
use serde_json::json;

fn initialize_params(value: serde_json::Value) -> InitializeParams {
    serde_json::from_value(value).expect("valid initialize params")
}

fn file_uri(path: &Path) -> String {
    url::Url::from_file_path(path)
        .expect("file uri")
        .to_string()
}

#[test]
fn line_index_round_trip_handles_utf16_columns() {
    let text = "ab\nz🙂x\n";
    let index = LineIndex::new(text);

    let offset = text.find('x').expect("x offset");
    let position = index.to_position(text, offset);
    assert_eq!(position.line, 1);
    assert_eq!(position.character, 3);

    let back = index
        .to_offset(text, position)
        .expect("position should map to offset");
    assert_eq!(back, offset);
}

#[test]
fn converts_diagnostics_with_related_info() {
    let uri = Uri::from_str("file:///tmp/test.k65").expect("uri");
    let text = "func main {\n  nop\n}\n";
    let index = LineIndex::new(text);
    let source_id = k816_core::span::SourceId(0);
    let primary = k816_core::span::Span::new(source_id, 2, 6);
    let related = k816_core::span::Span::new(source_id, 9, 12);
    let diagnostic = k816_core::diag::Diagnostic::error(primary, "oops")
        .with_label(related, "related")
        .with_help("fix it");

    let converted = diagnostic_to_lsp(&diagnostic, &uri, &index, text);
    assert_eq!(converted.severity, Some(DiagnosticSeverity::ERROR));
    assert!(converted.message.contains("help: fix it"));
    assert_eq!(
        converted
            .related_information
            .as_ref()
            .expect("related info")
            .len(),
        1
    );
}

#[test]
fn resolves_local_label_definition_in_scope() {
    let uri = Uri::from_str("file:///tmp/test.k65").expect("uri");
    let text = "func main {\n.loop:\n  bra .loop\n}\n".to_string();
    let mut state = ServerState::new(PathBuf::from("/tmp"));
    state
        .upsert_document(uri.clone(), text, 1, true)
        .expect("document inserted");

    let doc = state.documents.get(&uri).expect("doc");
    let offset = doc.text.find(".loop").expect("loop");
    let position = doc.line_index.to_position(&doc.text, offset + 16);
    let response = state
        .definition(&uri, position)
        .expect("definition should resolve");

    match response {
        GotoDefinitionResponse::Array(locations) => assert_eq!(locations.len(), 1),
        _ => panic!("unexpected response shape"),
    }
}

#[test]
fn resolves_cross_file_function_definition() {
    let uri_main = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text_main = "func main {\n  call app_init\n}\n".to_string();

    let uri_func = Uri::from_str("file:///project/src/func.k65").expect("uri");
    let text_func = "func app_init @a8 @i8 {\n  lda #1\n  ldx #2\n}\n".to_string();

    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri_func.clone(), text_func, 1, false)
        .expect("func doc");
    state
        .upsert_document(uri_main.clone(), text_main.clone(), 1, true)
        .expect("main doc");

    let doc = state.documents.get(&uri_main).expect("main doc");
    let offset = text_main.find("app_init").expect("app_init");
    let position = doc.line_index.to_position(&doc.text, offset);
    let response = state
        .definition(&uri_main, position)
        .expect("definition should resolve to func.k65");

    match response {
        GotoDefinitionResponse::Array(locations) => {
            assert_eq!(locations.len(), 1, "expected exactly one definition");
            assert_eq!(
                locations[0].uri, uri_func,
                "definition should be in func.k65"
            );
        }
        _ => panic!("unexpected response shape"),
    }
}

#[test]
fn resolves_cross_file_variable_definition() {
    let uri_vars = Uri::from_str("file:///project/src/vars.k65").expect("uri");
    let text_vars = "var foo = $1234\n".to_string();

    let uri_test = Uri::from_str("file:///project/src/test.k65").expect("uri");
    let text_test = "func test @a8 {\n  lda foo\n}\n".to_string();

    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri_vars.clone(), text_vars, 1, false)
        .expect("vars doc");
    state
        .upsert_document(uri_test.clone(), text_test.clone(), 1, true)
        .expect("test doc");

    let doc = state.documents.get(&uri_test).expect("test doc");
    let foo_offset = text_test.find("foo").expect("foo");
    let position = doc.line_index.to_position(&doc.text, foo_offset);
    let response = state
        .definition(&uri_test, position)
        .expect("definition should resolve to vars.k65");

    match response {
        GotoDefinitionResponse::Array(locations) => {
            assert_eq!(locations.len(), 1);
            assert_eq!(locations[0].uri, uri_vars);
        }
        _ => panic!("unexpected response shape"),
    }
}

#[test]
fn suppresses_cross_module_undefined_symbol_diagnostic() {
    let uri_vars = Uri::from_str("file:///project/src/vars.k65").expect("uri");
    let text_vars = "var foo = $1234\n".to_string();

    let uri_test = Uri::from_str("file:///project/src/test.k65").expect("uri");
    let text_test = "func test @a8 {\n  lda foo\n}\n".to_string();

    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri_vars.clone(), text_vars, 1, false)
        .expect("vars doc");
    state
        .upsert_document(uri_test.clone(), text_test, 1, true)
        .expect("test doc");

    let diagnostics = state.lsp_diagnostics(&uri_test);
    let has_foo_error = diagnostics
        .iter()
        .any(|d| d.message.contains("unknown identifier 'foo'"));
    assert!(
        !has_foo_error,
        "cross-module reference to 'foo' should not produce a diagnostic, got: {:?}",
        diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn symbols_survive_parse_failure() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let good_text = "func main {\n  nop\n}\n".to_string();

    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), good_text, 1, true)
        .expect("good doc");
    assert!(
        state.symbols.contains_key("main"),
        "main should be in the symbol index after good parse"
    );

    // Introduce a syntax error — symbols should be preserved
    let bad_text = "func main {\n  <<<broken>>>\n}\n".to_string();
    state
        .upsert_document(uri.clone(), bad_text, 2, true)
        .expect("bad doc");
    assert!(
        state.symbols.contains_key("main"),
        "main should still be in the symbol index after failed parse"
    );
}

#[test]
fn symbols_extracted_from_file_opened_with_errors() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    // File has a valid func definition followed by a syntax error
    let text = "func main {\n  nop\n}\n\nfunc broken {\n  <<<bad>>>\n}\n".to_string();

    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text, 1, true)
        .expect("doc");

    // The parser should recover and still extract "main" (and possibly "broken")
    assert!(
        state.symbols.contains_key("main"),
        "main should be in the symbol index even when file has errors, got: {:?}",
        state.symbols.keys().collect::<Vec<_>>()
    );
}

#[test]
fn symbols_extracted_despite_lex_errors() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    // File has a valid func followed by an unrecognized token (~ is not in the lexer)
    let text = "func main {\n  nop\n}\n\nfunc other {\n  lda ~bogus\n}\n".to_string();

    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text, 1, true)
        .expect("doc");

    assert!(
        state.symbols.contains_key("main"),
        "main should be in the symbol index despite lex errors, got: {:?}",
        state.symbols.keys().collect::<Vec<_>>()
    );
}

#[test]
fn symbols_extracted_with_double_colon_errors() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    // Uses TCB::name syntax that produces parse errors
    let text = "\
func test {
    nop
}

func main @a16 @i16 {
    call task_init
    lda #dbg_task
    sta reg_pc,x
    @a8 {
        lda #0
        sta TASKS+TCB::name+0,y
        sta TASKS+TCB::state,y
    }
    cli
    {} always
}
"
    .to_string();

    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text, 1, true)
        .expect("doc");

    assert!(
        state.symbols.contains_key("test"),
        "test should be in the symbol index despite :: errors, got: {:?}",
        state.symbols.keys().collect::<Vec<_>>()
    );
    assert!(
        state.symbols.contains_key("main"),
        "main should be in the symbol index despite :: errors, got: {:?}",
        state.symbols.keys().collect::<Vec<_>>()
    );
}

#[test]
fn token_match_includes_mode_directives() {
    let text = "@a16\n";
    let token = token_at_offset(text, 2).expect("token");
    assert_eq!(token.text, "@a16");
    assert_eq!(token_prefix_at_offset(text, 3), "@a1");
}

#[test]
fn completion_context_prefers_symbols_in_operand_positions() {
    let text = "func main {\n  bra\n  bra start\n}\n";
    let statement_start_offset = text.find("  bra").expect("statement start") + 2;
    let operand_offset = text.find("bra start").expect("operand") + 4;

    assert!(!in_symbol_completion_context(text, statement_start_offset));
    assert!(in_symbol_completion_context(text, operand_offset));
}

#[test]
fn const_hover_shows_decimal_and_hex_for_integer_value() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "const FOO = $2A\nfunc main {\n  lda #FOO\n}\n".to_string();

    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text.clone(), 1, true)
        .expect("doc");

    let doc = state.documents.get(&uri).expect("doc");
    let offset = text.find("FOO").expect("FOO");
    let position = doc.line_index.to_position(&doc.text, offset);
    let hover = state.hover(&uri, position).expect("hover");
    let HoverContents::Markup(markup) = hover.contents else {
        panic!("expected markdown hover");
    };
    assert!(markup.value.contains("**constant** `FOO`"));
    assert!(markup.value.contains("- decimal: `42`"));
    assert!(markup.value.contains("- hex: `$2A`"));
}

#[test]
fn numeric_literal_hover_shows_decimal_hex_and_binary_for_u8_range() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "func main {\n  lda #42\n}\n".to_string();

    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text.clone(), 1, true)
        .expect("doc");

    let doc = state.documents.get(&uri).expect("doc");
    let offset = text.find("42").expect("42");
    let position = doc.line_index.to_position(&doc.text, offset);
    let hover = state.hover(&uri, position).expect("hover");
    let HoverContents::Markup(markup) = hover.contents else {
        panic!("expected markdown hover");
    };
    assert!(markup.value.contains("- decimal: `42`"));
    assert!(markup.value.contains("- hex: `$2A`"));
    assert!(markup.value.contains("- binary: `%00101010`"));
}

#[test]
fn numeric_literal_hover_omits_binary_for_values_over_255() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "func main {\n  lda #$1234\n}\n".to_string();

    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text.clone(), 1, true)
        .expect("doc");

    let doc = state.documents.get(&uri).expect("doc");
    let offset = text.find("$1234").expect("$1234");
    let position = doc.line_index.to_position(&doc.text, offset);
    let hover = state.hover(&uri, position).expect("hover");
    let HoverContents::Markup(markup) = hover.contents else {
        panic!("expected markdown hover");
    };
    assert!(markup.value.contains("- decimal: `4660`"));
    assert!(markup.value.contains("- hex: `$1234`"));
    assert!(!markup.value.contains("binary"));
}

#[test]
fn const_hover_formats_negative_integer_as_signed_magnitude_hex() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "const NEG = 0 - 42\nfunc main {\n  lda #NEG\n}\n".to_string();

    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text.clone(), 1, true)
        .expect("doc");

    let doc = state.documents.get(&uri).expect("doc");
    let offset = text.find("NEG").expect("NEG");
    let position = doc.line_index.to_position(&doc.text, offset);
    let hover = state.hover(&uri, position).expect("hover");
    let HoverContents::Markup(markup) = hover.contents else {
        panic!("expected markdown hover");
    };
    assert!(markup.value.contains("- decimal: `-42`"));
    assert!(markup.value.contains("- hex: `-$2A`"));
}

#[test]
fn query_memory_map_reports_unavailable_when_no_link_layout() {
    let state = ServerState::new(PathBuf::from("/project"));
    let result = state.query_memory_map(&QueryMemoryMapParams::default());

    assert_eq!(result.status, QueryMemoryMapStatus::Unavailable);
    assert!(result.memories.is_empty());
    assert!(result.runs.is_empty());
}

#[test]
fn query_memory_map_reports_area_usage() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "func main {\n  nop\n}\n".to_string();
    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri, text, 1, true)
        .expect("document inserted");

    let result = state.query_memory_map(&QueryMemoryMapParams::default());
    assert_eq!(result.status, QueryMemoryMapStatus::Ok);
    let main = result
        .memories
        .iter()
        .find(|memory| memory.name == "MAIN")
        .expect("MAIN memory row");
    assert!(main.used > 0, "used bytes should be greater than zero");
    assert_eq!(main.free + main.used, main.size);
    let expected = (f64::from(main.used) * 100.0) / f64::from(main.size);
    assert!((main.utilization_percent - expected).abs() < 0.0001);
}

#[test]
fn query_memory_map_filters_by_memory_name() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "func main {\n  nop\n}\n".to_string();
    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri, text, 1, true)
        .expect("document inserted");

    let result = state.query_memory_map(&QueryMemoryMapParams {
        memory_name: Some("MAIN".to_string()),
        detail: QueryMemoryMapDetail::Summary,
    });

    assert_eq!(result.status, QueryMemoryMapStatus::Ok);
    assert_eq!(result.memories.len(), 1);
    assert_eq!(result.memories[0].name, "MAIN");
}

#[test]
fn query_memory_map_includes_run_rows_with_detail_runs() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "func main {\n  nop\n}\n".to_string();
    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri, text, 1, true)
        .expect("document inserted");

    let result = state.query_memory_map(&QueryMemoryMapParams {
        memory_name: None,
        detail: QueryMemoryMapDetail::Runs,
    });

    assert_eq!(result.status, QueryMemoryMapStatus::Ok);
    assert!(!result.runs.is_empty(), "expected run rows");
    for run in &result.runs {
        let expected_end = if run.size == 0 {
            run.start
        } else {
            run.start + run.size - 1
        };
        assert_eq!(run.end, expected_end);
    }
}

#[test]
fn resolve_inline_symbols_resolves_variables_with_read_size_hint() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "var foo = $1234\nfunc main {\n  lda foo\n}\n".to_string();
    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text, 1, true)
        .expect("document inserted");

    let result = state.resolve_inline_symbols(
        &uri,
        &ResolveInlineSymbolsParams {
            uri: uri.to_string(),
            start_line: 2,
            end_line: 2,
        },
    );

    let foo = result
        .symbols
        .iter()
        .find(|symbol| symbol.name == "foo")
        .expect("foo symbol");
    assert_eq!(foo.category, "variable");
    assert_eq!(foo.address, 0x1234);
    assert_eq!(foo.read_size_hint, Some(1));
}

#[test]
fn resolve_inline_symbols_resolves_labels_and_functions() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "func main {\nstart:\n  nop\n}\n".to_string();
    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text, 1, true)
        .expect("document inserted");

    let result = state.resolve_inline_symbols(
        &uri,
        &ResolveInlineSymbolsParams {
            uri: uri.to_string(),
            start_line: 0,
            end_line: 1,
        },
    );

    let start = result
        .symbols
        .iter()
        .find(|symbol| symbol.name == "start")
        .expect("start label");
    assert_eq!(start.category, "label");

    let main = result
        .symbols
        .iter()
        .find(|symbol| symbol.name == "main")
        .expect("main function");
    assert_eq!(main.category, "function");
}

#[test]
fn resolve_inline_symbols_skips_unresolved_identifiers() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "func main {\n  lda missing\n}\n".to_string();
    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text, 1, true)
        .expect("document inserted");

    let result = state.resolve_inline_symbols(
        &uri,
        &ResolveInlineSymbolsParams {
            uri: uri.to_string(),
            start_line: 1,
            end_line: 1,
        },
    );

    assert!(
        !result.symbols.iter().any(|symbol| symbol.name == "missing"),
        "unresolved token must not produce inline symbol"
    );
}

#[test]
fn references_include_declaration_and_usage() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "func main {\nstart:\n  bra start\n}\n".to_string();
    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text.clone(), 1, true)
        .expect("document inserted");

    let doc = state.documents.get(&uri).expect("doc");
    let offset = text
        .match_indices("start")
        .nth(1)
        .map(|(idx, _)| idx)
        .expect("start usage");
    let position = doc.line_index.to_position(&doc.text, offset);
    let refs = state
        .references(
            &uri,
            position,
            &ReferenceContext {
                include_declaration: true,
            },
        )
        .expect("references");

    assert!(
        refs.len() >= 2,
        "expected declaration + usage references, got: {refs:?}"
    );
}

#[test]
fn rename_updates_symbol_occurrences_across_files() {
    let uri_main = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text_main = "func main {\n  call app_init\n}\n".to_string();
    let uri_func = Uri::from_str("file:///project/src/func.k65").expect("uri");
    let text_func = "func app_init {\n  nop\n}\n".to_string();

    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri_func.clone(), text_func, 1, false)
        .expect("func doc");
    state
        .upsert_document(uri_main.clone(), text_main.clone(), 1, true)
        .expect("main doc");

    let doc = state.documents.get(&uri_main).expect("main doc");
    let offset = text_main.find("app_init").expect("app_init usage");
    let position = doc.line_index.to_position(&doc.text, offset);
    let edit = state
        .rename(&uri_main, position, "boot")
        .expect("workspace edit");
    let main_edits = edit
        .changes
        .as_ref()
        .expect("changes")
        .get(&uri_main)
        .expect("main file edits");
    let func_edits = edit
        .changes
        .as_ref()
        .expect("changes")
        .get(&uri_func)
        .expect("func file edits");
    assert!(
        main_edits.iter().any(|edit| edit.new_text == "boot"),
        "main file rename edit missing"
    );
    assert!(
        func_edits.iter().any(|edit| edit.new_text == "boot"),
        "func file rename edit missing"
    );
}

#[test]
fn semantic_tokens_mark_unresolved_identifiers() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "func main {\n  lda missing\n}\n".to_string();
    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text, 1, true)
        .expect("doc");

    let tokens = state.semantic_tokens(&uri).expect("semantic tokens");
    assert!(
        !tokens.data.is_empty(),
        "semantic tokens should not be empty"
    );
}

#[test]
fn signature_help_resolves_evaluator_function() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "func main {\n  lda #[clamp(10, 0, 255)]\n}\n".to_string();
    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text.clone(), 1, true)
        .expect("doc");

    let doc = state.documents.get(&uri).expect("doc");
    let offset = text.find("0, 255").expect("call arg");
    let position = doc.line_index.to_position(&doc.text, offset);
    let help = state
        .signature_help(&uri, position)
        .expect("signature help");
    assert_eq!(help.active_signature, Some(0));
    assert_eq!(help.active_parameter, Some(1));
    assert!(
        help.signatures[0].label.contains("clamp"),
        "expected clamp signature"
    );
}

#[test]
fn code_lens_includes_reference_count_for_label() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "func main {\nstart:\n  bra start\n}\n".to_string();
    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text, 1, true)
        .expect("doc");

    let lenses = state.code_lenses(&uri).expect("code lenses");
    assert!(
        lenses.iter().any(|lens| lens
            .command
            .as_ref()
            .is_some_and(|cmd| cmd.title.contains("reference"))),
        "expected at least one reference count lens"
    );
}

#[test]
fn folding_ranges_include_brace_blocks() {
    let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
    let text = "func main {\n  nop\n}\n".to_string();
    let mut state = ServerState::new(PathBuf::from("/project"));
    state
        .upsert_document(uri.clone(), text, 1, true)
        .expect("doc");

    let ranges = state.folding_ranges(&uri).expect("folding ranges");
    assert!(!ranges.is_empty(), "expected folding ranges");
}

#[test]
fn workspace_root_prefers_first_workspace_folder() {
    let temp = std::env::temp_dir();
    let workspace_root = temp.join("k816-lsp-workspace");
    let root_uri = temp.join("k816-lsp-root-uri");

    let params = initialize_params(json!({
        "capabilities": {},
        "rootUri": file_uri(&root_uri),
        "workspaceFolders": [
            {
                "uri": file_uri(&workspace_root),
                "name": "workspace"
            }
        ]
    }));

    let resolved = determine_workspace_root(&params).expect("workspace root");
    assert_eq!(resolved, workspace_root);
}

#[test]
fn workspace_root_errors_when_client_omits_root_hints() {
    let params = initialize_params(json!({
        "capabilities": {}
    }));

    let error = determine_workspace_root(&params).expect_err("missing workspace root");
    assert!(
        error
            .to_string()
            .contains("workspaceFolders/rootUri/rootPath"),
        "unexpected error: {error}"
    );
}
