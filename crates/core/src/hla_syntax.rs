/// Rewrites HLA shorthand into the native mnemonic form understood by parsing/lowering.
pub fn desugar_hla_syntax(source: &str) -> String {
    let had_trailing_newline = source.ends_with('\n');
    let mut out_lines = Vec::new();
    let mut loop_stack = Vec::new();
    let mut counter = 0usize;
    let mut in_named_data_block = false;

    for line in source.lines() {
        let indent_len = line.len() - line.trim_start().len();
        let indent = &line[..indent_len];
        let (code, comment) = split_comment(line);
        let code_trimmed = code.trim();

        if in_named_data_block {
            if code_trimmed == "}" {
                in_named_data_block = false;
                continue;
            }

            if code_trimmed.is_empty() {
                out_lines.push(line.to_string());
                continue;
            }

            if let Some(bytes) = parse_string_literal(code_trimmed) {
                out_lines.push(format!("{indent}.byte {bytes}"));
                continue;
            }

            if let Some(bytes) = parse_space_separated_byte_literals(code_trimmed) {
                if let Some(comment) = comment {
                    out_lines.push(format!("{indent}.byte {bytes} {comment}"));
                } else {
                    out_lines.push(format!("{indent}.byte {bytes}"));
                }
                continue;
            }

            out_lines.push(line.to_string());
            continue;
        }

        if let Some(name) = parse_named_data_block_start(code_trimmed) {
            in_named_data_block = true;
            out_lines.push(format!("{indent}{name}:"));
            if let Some(comment) = comment {
                out_lines.push(format!("{indent}{comment}"));
            }
            continue;
        }

        if let Some(symbol) = parse_wait_loop(code_trimmed) {
            let label = format!(".__k816_wait_{counter}");
            counter += 1;
            out_lines.push(format!("{indent}{label}:"));
            out_lines.push(format!("{indent}lda {symbol}"));
            if let Some(comment) = comment {
                out_lines.push(format!("{indent}bpl {label} {comment}"));
            } else {
                out_lines.push(format!("{indent}bpl {label}"));
            }
            continue;
        }

        if is_do_while_open(code_trimmed) {
            let label = format!(".__k816_loop_{counter}");
            counter += 1;
            loop_stack.push(label.clone());
            if let Some(comment) = comment {
                out_lines.push(format!("{indent}{label}: {comment}"));
            } else {
                out_lines.push(format!("{indent}{label}:"));
            }
            continue;
        }

        if is_do_while_close_a_ne_0(code_trimmed) {
            if let Some(label) = loop_stack.pop() {
                out_lines.push(format!("{indent}cmp #0"));
                if let Some(comment) = comment {
                    out_lines.push(format!("{indent}bne {label} {comment}"));
                } else {
                    out_lines.push(format!("{indent}bne {label}"));
                }
                continue;
            }
        }

        if let Some(rhs) = parse_x_immediate_assignment(code_trimmed) {
            if let Some(comment) = comment {
                out_lines.push(format!("{indent}ldx {rhs} {comment}"));
            } else {
                out_lines.push(format!("{indent}ldx {rhs}"));
            }
            continue;
        }

        if is_x_increment(code_trimmed) {
            if let Some(comment) = comment {
                out_lines.push(format!("{indent}inx {comment}"));
            } else {
                out_lines.push(format!("{indent}inx"));
            }
            continue;
        }

        if let Some((dest, rhs)) = parse_store_from_a_assignment(code_trimmed) {
            out_lines.push(format!("{indent}lda {rhs}"));
            if let Some(comment) = comment {
                out_lines.push(format!("{indent}sta {dest} {comment}"));
            } else {
                out_lines.push(format!("{indent}sta {dest}"));
            }
            continue;
        }

        out_lines.push(line.to_string());
    }

    let mut output = out_lines.join("\n");
    if had_trailing_newline {
        output.push('\n');
    }
    output
}

fn split_comment(line: &str) -> (&str, Option<&str>) {
    if let Some(index) = line.find("//") {
        (&line[..index], Some(&line[index..]))
    } else {
        (line, None)
    }
}

fn parse_named_data_block_start(code: &str) -> Option<&str> {
    if !code.ends_with('{') {
        return None;
    }
    let before_brace = code[..code.len() - 1].trim_end();
    let mut parts = before_brace.split_whitespace();
    let head = parts.next()?;
    let name = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    if !head.eq_ignore_ascii_case("data") || !is_identifier(name) {
        return None;
    }
    Some(name)
}

fn parse_wait_loop(code: &str) -> Option<String> {
    let compact: String = code.chars().filter(|ch| !ch.is_whitespace()).collect();
    let prefix = "{a&?";
    let suffix = "}n-?";
    if !compact.starts_with(prefix) || !compact.ends_with(suffix) {
        return None;
    }
    let symbol = &compact[prefix.len()..compact.len() - suffix.len()];
    if is_identifier(symbol) {
        Some(symbol.to_string())
    } else {
        None
    }
}

fn is_do_while_open(code: &str) -> bool {
    code == "{"
}

fn is_do_while_close_a_ne_0(code: &str) -> bool {
    let compact: String = code.chars().filter(|ch| !ch.is_whitespace()).collect();
    compact == "}a?0!="
}

fn parse_x_immediate_assignment(code: &str) -> Option<&str> {
    let mut parts = code.split('=');
    let lhs = parts.next()?.trim();
    let rhs = parts.next()?.trim();
    if parts.next().is_some() {
        return None;
    }
    if lhs.eq_ignore_ascii_case("x") && rhs.starts_with('#') {
        Some(rhs)
    } else {
        None
    }
}

fn is_x_increment(code: &str) -> bool {
    let compact: String = code.chars().filter(|ch| !ch.is_whitespace()).collect();
    compact.eq_ignore_ascii_case("x++")
}

fn parse_store_from_a_assignment(code: &str) -> Option<(String, String)> {
    let compact: String = code.chars().filter(|ch| !ch.is_whitespace()).collect();
    let marker = "=a=";
    let split = compact.find(marker)?;
    let dest = &compact[..split];
    let rhs = &compact[split + marker.len()..];
    if !is_identifier(dest) || rhs.is_empty() {
        return None;
    }
    Some((dest.to_string(), rhs.to_string()))
}

fn parse_string_literal(code: &str) -> Option<String> {
    if !(code.starts_with('"') && code.ends_with('"') && code.len() >= 2) {
        return None;
    }
    let content = &code[1..code.len() - 1];
    let mut bytes = Vec::new();
    let mut chars = content.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            let escaped = chars.next()?;
            let resolved = match escaped {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                other => other,
            };
            let mut encoded = [0u8; 4];
            let utf8 = resolved.encode_utf8(&mut encoded);
            bytes.extend_from_slice(utf8.as_bytes());
        } else {
            let mut encoded = [0u8; 4];
            let utf8 = ch.encode_utf8(&mut encoded);
            bytes.extend_from_slice(utf8.as_bytes());
        }
    }

    Some(
        bytes
            .iter()
            .map(|value| format!("${value:02X}"))
            .collect::<Vec<_>>()
            .join(", "),
    )
}

fn parse_space_separated_byte_literals(code: &str) -> Option<String> {
    let parts = code
        .split_whitespace()
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>();
    if parts.is_empty() {
        return None;
    }

    if !parts.iter().all(|part| is_byte_literal_token(part)) {
        return None;
    }

    Some(parts.join(", "))
}

fn is_byte_literal_token(token: &str) -> bool {
    if let Some(hex) = token.strip_prefix('$') {
        return !hex.is_empty() && hex.chars().all(|ch| ch.is_ascii_hexdigit());
    }
    token.chars().all(|ch| ch.is_ascii_digit())
}

fn is_identifier(value: &str) -> bool {
    let mut chars = value.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    if !(first.is_ascii_alphabetic() || first == '_' || first == '.') {
        return false;
    }
    chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '.')
}

#[cfg(test)]
mod tests {
    use super::desugar_hla_syntax;

    #[test]
    fn desugars_hello_uart_hla_constructs() {
        let source = "main {\n  x = #0\n  {\n    { a&?UART_READY } n-?\n    UART_DATA = a = text,x\n    x++\n  } a?0 !=\n  API_OP = a = #$FF\n}\n\ndata text {\n  \"Hello, World!\"\n  $0D $0A $00\n}\n\ndata INFO {\n  segment INFO\n  \"Hello UART example for X65 RIA\"\n}\n";
        let expected = "main {\n  ldx #0\n  .__k816_loop_0:\n    .__k816_wait_1:\n    lda UART_READY\n    bpl .__k816_wait_1\n    lda text,x\n    sta UART_DATA\n    inx\n  cmp #0\n  bne .__k816_loop_0\n  lda #$FF\n  sta API_OP\n}\n\ntext:\n  .byte $48, $65, $6C, $6C, $6F, $2C, $20, $57, $6F, $72, $6C, $64, $21\n  .byte $0D, $0A, $00\n\nINFO:\n  segment INFO\n  .byte $48, $65, $6C, $6C, $6F, $20, $55, $41, $52, $54, $20, $65, $78, $61, $6D, $70, $6C, $65, $20, $66, $6F, $72, $20, $58, $36, $35, $20, $52, $49, $41\n";
        assert_eq!(desugar_hla_syntax(source), expected);
    }
}
