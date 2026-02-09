use crate::ast::{
    BankDecl, BlockKind, CallStmt, CodeBlock, DataArg, DataBlock, DataCommand, Expr, File,
    Instruction, Item, LabelDecl, Operand, Stmt, VarDecl,
};
use crate::diag::Diagnostic;
use crate::lexer::{Token, TokenKind, lex};
use crate::span::{SourceId, Span, Spanned};
use chumsky::{
    Parser as _, extra,
    input::{Input as _, Stream, ValueInput},
    prelude::{Rich, SimpleSpan, end, just},
};

pub fn parse(source_id: SourceId, source_text: &str) -> Result<File, Vec<Diagnostic>> {
    let tokens = lex(source_id, source_text)?;
    let mut parser = Parser::new(source_id, tokens);
    parser.parse_file()
}

pub fn parse_expression_fragment(
    source_id: SourceId,
    source_text: &str,
) -> Result<Spanned<Expr>, Diagnostic> {
    let tokens = match lex(source_id, source_text) {
        Ok(tokens) => tokens,
        Err(mut diagnostics) => {
            return Err(diagnostics
                .pop()
                .expect("lexer should produce at least one diagnostic"));
        }
    };

    let end_offset = tokens.last().map(|token| token.span.end).unwrap_or(0);
    let token_stream = Stream::from_iter(tokens.into_iter().map(|token| {
        let span = (token.span.start..token.span.end).into();
        (token.kind, span)
    }))
    .map((end_offset..end_offset).into(), |(kind, span): (_, _)| {
        (kind, span)
    });

    expression_fragment_parser()
        .parse(token_stream)
        .into_result()
        .map(|(expr, span)| {
            let range = span.into_range();
            Spanned::new(expr, Span::new(source_id, range.start, range.end))
        })
        .map_err(|errors| expression_fragment_error(source_id, errors))
}

fn expression_fragment_parser<'tokens, I>()
-> impl chumsky::Parser<'tokens, I, (Expr, SimpleSpan), extra::Err<Rich<'tokens, TokenKind>>>
where
    I: ValueInput<'tokens, Token = TokenKind, Span = SimpleSpan>,
{
    let expr = chumsky::select! {
        TokenKind::Number(value) => Expr::Number(value),
        TokenKind::Ident(value) => Expr::Ident(value),
        TokenKind::Eval(value) => Expr::EvalText(value),
    }
    .map_with(|expr, e| (expr, e.span()));

    just(TokenKind::Newline)
        .repeated()
        .ignore_then(expr)
        .then_ignore(just(TokenKind::Newline).repeated())
        .then_ignore(end())
}

fn expression_fragment_error<'tokens>(
    source_id: SourceId,
    mut errors: Vec<Rich<'tokens, TokenKind>>,
) -> Diagnostic {
    let error = errors
        .pop()
        .expect("chumsky should return at least one parse error");
    let range = error.span().into_range();
    let span = Span::new(source_id, range.start, range.end);
    Diagnostic::error(span, format!("invalid expression fragment: {error:?}"))
}

struct Parser {
    source_id: SourceId,
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(source_id: SourceId, tokens: Vec<Token>) -> Self {
        Self {
            source_id,
            tokens,
            pos: 0,
        }
    }

    fn parse_file(&mut self) -> Result<File, Vec<Diagnostic>> {
        let mut items = Vec::new();
        let mut diagnostics = Vec::new();

        while !self.is_eof() {
            self.skip_separators();
            if self.is_eof() {
                break;
            }

            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(diag) => {
                    diagnostics.push(diag);
                    self.recover_to_boundary();
                }
            }
        }

        if diagnostics.is_empty() {
            Ok(File { items })
        } else {
            Err(diagnostics)
        }
    }

    fn parse_item(&mut self) -> Result<Spanned<Item>, Diagnostic> {
        if self.at_kind(&TokenKind::Bank) {
            let start = self.bump().expect("token exists").span.start;
            let name = self.expect_ident("expected bank name")?;
            let span = Span::new(self.source_id, start, self.prev_end());
            return Ok(Spanned::new(Item::Bank(BankDecl { name }), span));
        }

        if self.at_kind(&TokenKind::Var) {
            let var = self.parse_var_decl()?;
            return Ok(var.map(Item::Var));
        }

        if self.at_kind(&TokenKind::Data) {
            let data = self.parse_data_block_item()?;
            return Ok(data.map(Item::DataBlock));
        }

        let mut is_far = false;
        let mut is_naked = false;
        let mut is_inline = false;
        let modifier_start = self.current_span().start;

        loop {
            if self.at_kind(&TokenKind::Far) {
                is_far = true;
                self.bump();
                continue;
            }
            if self.at_kind(&TokenKind::Naked) {
                is_naked = true;
                self.bump();
                continue;
            }
            if self.at_kind(&TokenKind::Inline) {
                is_inline = true;
                self.bump();
                continue;
            }
            break;
        }

        if self.at_kind(&TokenKind::Func) || self.at_kind(&TokenKind::Main) {
            let block = self.parse_code_block(is_far, is_naked, is_inline, modifier_start)?;
            return Ok(block.map(Item::CodeBlock));
        }

        if is_far || is_naked || is_inline {
            return Err(Diagnostic::error(
                self.current_span(),
                "code block modifier must be followed by 'func' or 'main'",
            ));
        }

        let stmt = self.parse_stmt()?;
        Ok(stmt.map(Item::Statement))
    }

    fn parse_code_block(
        &mut self,
        is_far: bool,
        is_naked: bool,
        is_inline: bool,
        modifier_start: usize,
    ) -> Result<Spanned<CodeBlock>, Diagnostic> {
        let (kind, name, start) = if self.at_kind(&TokenKind::Main) {
            let token = self.bump().expect("token exists");
            (BlockKind::Main, "main".to_string(), token.span.start)
        } else {
            let token = self.expect_kind(&TokenKind::Func, "expected 'func' or 'main'")?;
            let name = self.expect_ident("expected function name")?;
            (BlockKind::Func, name, token.span.start)
        };

        self.expect_kind(&TokenKind::LBrace, "expected '{' to start code block")?;
        let mut body = Vec::new();

        while !self.is_eof() && !self.at_kind(&TokenKind::RBrace) {
            self.skip_separators();
            if self.at_kind(&TokenKind::RBrace) {
                break;
            }
            let stmt = self.parse_stmt()?;
            body.push(stmt);
            self.skip_separators();
        }

        self.expect_kind(&TokenKind::RBrace, "expected '}' to close code block")?;
        let block_start = if modifier_start < start {
            modifier_start
        } else {
            start
        };
        let span = Span::new(self.source_id, block_start, self.prev_end());

        Ok(Spanned::new(
            CodeBlock {
                name,
                kind,
                is_far,
                is_naked,
                is_inline,
                body,
            },
            span,
        ))
    }

    fn parse_data_block_item(&mut self) -> Result<Spanned<DataBlock>, Diagnostic> {
        let start = self
            .expect_kind(&TokenKind::Data, "expected 'data'")?
            .span
            .start;
        let block = self.parse_data_block(start)?;
        Ok(block)
    }

    fn parse_data_block(&mut self, start: usize) -> Result<Spanned<DataBlock>, Diagnostic> {
        self.expect_kind(&TokenKind::LBrace, "expected '{' after data")?;
        let mut commands = Vec::new();

        while !self.is_eof() && !self.at_kind(&TokenKind::RBrace) {
            self.skip_separators();
            if self.at_kind(&TokenKind::RBrace) {
                break;
            }
            let command = self.parse_data_command()?;
            commands.push(command);
            self.skip_separators();
        }

        self.expect_kind(&TokenKind::RBrace, "expected '}' to close data block")?;
        let span = Span::new(self.source_id, start, self.prev_end());
        Ok(Spanned::new(DataBlock { commands }, span))
    }

    fn parse_data_command(&mut self) -> Result<Spanned<DataCommand>, Diagnostic> {
        let token = self
            .bump()
            .ok_or_else(|| Diagnostic::error(self.current_span(), "expected data command"))?;

        let start = token.span.start;
        let command = match token.kind {
            TokenKind::Align => {
                let value = self.expect_number("expected alignment value")?;
                let align = u16::try_from(value).map_err(|_| {
                    Diagnostic::error(self.current_span(), "align value must fit in u16")
                })?;
                DataCommand::Align(align)
            }
            TokenKind::Address => {
                let value = self.expect_number("expected address value")?;
                let address = u32::try_from(value).map_err(|_| {
                    Diagnostic::error(self.current_span(), "address value must fit in u32")
                })?;
                DataCommand::Address(address)
            }
            TokenKind::Nocross => {
                let value = self.expect_number("expected nocross value")?;
                let boundary = u16::try_from(value).map_err(|_| {
                    Diagnostic::error(self.current_span(), "nocross value must fit in u16")
                })?;
                DataCommand::Nocross(boundary)
            }
            TokenKind::Ident(kind) => {
                self.expect_kind(&TokenKind::LParen, "expected '(' after converter kind")?;
                let mut args = Vec::new();
                if !self.at_kind(&TokenKind::RParen) {
                    loop {
                        args.push(self.parse_data_arg()?);
                        if self.at_kind(&TokenKind::Comma) {
                            self.bump();
                            continue;
                        }
                        break;
                    }
                }
                self.expect_kind(&TokenKind::RParen, "expected ')' after converter args")?;
                DataCommand::Convert { kind, args }
            }
            other => {
                return Err(Diagnostic::error(
                    token.span,
                    format!("unexpected data command token: {other:?}"),
                ));
            }
        };

        let span = Span::new(self.source_id, start, self.prev_end());
        Ok(Spanned::new(command, span))
    }

    fn parse_data_arg(&mut self) -> Result<DataArg, Diagnostic> {
        let token = self
            .bump()
            .ok_or_else(|| Diagnostic::error(self.current_span(), "expected converter argument"))?;
        match token.kind {
            TokenKind::Number(value) => Ok(DataArg::Int(value)),
            TokenKind::String(value) => Ok(DataArg::Str(value)),
            _ => Err(Diagnostic::error(
                token.span,
                "converter arguments must be string or integer literals",
            )),
        }
    }

    fn parse_var_decl(&mut self) -> Result<Spanned<VarDecl>, Diagnostic> {
        let start = self
            .expect_kind(&TokenKind::Var, "expected 'var'")?
            .span
            .start;
        let name = self.expect_ident("expected variable name")?;
        let array_len = if matches!(self.current_kind(), Some(TokenKind::Eval(_))) {
            Some(self.parse_bracket_expr("invalid var array length expression")?)
        } else {
            None
        };
        let initializer = if self.at_kind(&TokenKind::Eq) {
            self.bump();
            Some(self.parse_expr()?.node)
        } else {
            None
        };
        let span = Span::new(self.source_id, start, self.prev_end());
        Ok(Spanned::new(
            VarDecl {
                name,
                array_len,
                initializer,
            },
            span,
        ))
    }

    fn parse_bracket_expr(&mut self, message: &str) -> Result<Expr, Diagnostic> {
        let token = self
            .bump()
            .ok_or_else(|| Diagnostic::error(self.current_span(), message))?;
        let span = token.span;
        let text = match token.kind {
            TokenKind::Eval(value) => value,
            _ => return Err(Diagnostic::error(span, message)),
        };

        parse_expression_fragment(self.source_id, &text)
            .map(|expr| expr.node)
            .map_err(|err| {
                Diagnostic::error(span, format!("{message}: {}", err.message))
                    .with_label(span, format!("inside brackets: [{text}]"))
            })
    }

    fn parse_stmt(&mut self) -> Result<Spanned<Stmt>, Diagnostic> {
        if self.at_kind(&TokenKind::Var) {
            let var = self.parse_var_decl()?;
            return Ok(var.map(Stmt::Var));
        }

        if self.at_kind(&TokenKind::Data) {
            let start = self.bump().expect("token exists").span.start;
            let block = self.parse_data_block(start)?;
            return Ok(block.map(Stmt::DataBlock));
        }

        if self.at_kind(&TokenKind::Call) {
            let start = self.bump().expect("token exists").span.start;
            let target = self.expect_ident("expected function name after call")?;
            let span = Span::new(self.source_id, start, self.prev_end());
            return Ok(Spanned::new(Stmt::Call(CallStmt { target }), span));
        }

        if self.at_ident_label() {
            let name_token = self.bump().expect("token exists");
            let name = match name_token.kind {
                TokenKind::Ident(name) => name,
                _ => unreachable!(),
            };
            self.expect_kind(&TokenKind::Colon, "expected ':' after label")?;
            let span = Span::new(self.source_id, name_token.span.start, self.prev_end());
            return Ok(Spanned::new(Stmt::Label(LabelDecl { name }), span));
        }

        let mnemonic_token = self
            .bump()
            .ok_or_else(|| Diagnostic::error(self.current_span(), "expected statement"))?;

        let mnemonic = match mnemonic_token.kind {
            TokenKind::Ident(value) => value,
            other => {
                return Err(Diagnostic::error(
                    mnemonic_token.span,
                    format!("unexpected token in statement: {other:?}"),
                ));
            }
        };

        if mnemonic == ".byte" {
            let mut values = Vec::new();
            loop {
                let expr = self.parse_expr()?;
                values.push(expr.node);
                if self.at_kind(&TokenKind::Comma) {
                    self.bump();
                    continue;
                }
                break;
            }
            let span = Span::new(self.source_id, mnemonic_token.span.start, self.prev_end());
            return Ok(Spanned::new(Stmt::Bytes(values), span));
        }

        let operand = if self.is_operand_boundary() {
            None
        } else if self.at_kind(&TokenKind::Hash) {
            self.bump();
            let expr = self.parse_expr()?;
            Some(Operand::Immediate(expr.node))
        } else {
            let force_far = if self.at_kind(&TokenKind::Far) {
                self.bump();
                true
            } else {
                false
            };
            let expr = self.parse_expr()?;
            Some(Operand::Value {
                expr: expr.node,
                force_far,
            })
        };

        let span = Span::new(self.source_id, mnemonic_token.span.start, self.prev_end());
        Ok(Spanned::new(
            Stmt::Instruction(Instruction { mnemonic, operand }),
            span,
        ))
    }

    fn parse_expr(&mut self) -> Result<Spanned<Expr>, Diagnostic> {
        let token = self
            .bump()
            .ok_or_else(|| Diagnostic::error(self.current_span(), "expected expression"))?;
        let node = match token.kind {
            TokenKind::Number(value) => Expr::Number(value),
            TokenKind::Ident(value) => Expr::Ident(value),
            TokenKind::Eval(value) => Expr::EvalText(value),
            _ => return Err(Diagnostic::error(token.span, "expected expression")),
        };
        Ok(Spanned::new(node, token.span))
    }

    fn expect_number(&mut self, message: &str) -> Result<i64, Diagnostic> {
        let token = self
            .bump()
            .ok_or_else(|| Diagnostic::error(self.current_span(), message))?;
        match token.kind {
            TokenKind::Number(value) => Ok(value),
            _ => Err(Diagnostic::error(token.span, message)),
        }
    }

    fn expect_ident(&mut self, message: &str) -> Result<String, Diagnostic> {
        let token = self
            .bump()
            .ok_or_else(|| Diagnostic::error(self.current_span(), message))?;
        match token.kind {
            TokenKind::Ident(value) => Ok(value),
            _ => Err(Diagnostic::error(token.span, message)),
        }
    }

    fn expect_kind(&mut self, kind: &TokenKind, message: &str) -> Result<Token, Diagnostic> {
        let token = self
            .bump()
            .ok_or_else(|| Diagnostic::error(self.current_span(), message))?;
        if Self::same_variant(&token.kind, kind) {
            Ok(token)
        } else {
            Err(Diagnostic::error(token.span, message))
        }
    }

    fn is_operand_boundary(&self) -> bool {
        self.is_eof() || self.at_kind(&TokenKind::Newline) || self.at_kind(&TokenKind::RBrace)
    }

    fn at_ident_label(&self) -> bool {
        matches!(
            (self.current_kind(), self.peek_kind()),
            (Some(TokenKind::Ident(_)), Some(TokenKind::Colon))
        )
    }

    fn skip_separators(&mut self) {
        while self.at_kind(&TokenKind::Newline) {
            self.bump();
        }
    }

    fn recover_to_boundary(&mut self) {
        while !self.is_eof() {
            if self.at_kind(&TokenKind::Newline) || self.at_kind(&TokenKind::RBrace) {
                break;
            }
            self.bump();
        }
    }

    fn at_kind(&self, kind: &TokenKind) -> bool {
        self.current_kind()
            .map(|current| Self::same_variant(current, kind))
            .unwrap_or(false)
    }

    fn current_kind(&self) -> Option<&TokenKind> {
        self.tokens.get(self.pos).map(|token| &token.kind)
    }

    fn peek_kind(&self) -> Option<&TokenKind> {
        self.tokens.get(self.pos + 1).map(|token| &token.kind)
    }

    fn bump(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.pos).cloned();
        if token.is_some() {
            self.pos += 1;
        }
        token
    }

    fn prev_end(&self) -> usize {
        if self.pos == 0 {
            0
        } else {
            self.tokens
                .get(self.pos.saturating_sub(1))
                .map(|token| token.span.end)
                .unwrap_or(0)
        }
    }

    fn current_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|token| token.span)
            .unwrap_or_else(|| Span::new(self.source_id, self.prev_end(), self.prev_end()))
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn same_variant(a: &TokenKind, b: &TokenKind) -> bool {
        std::mem::discriminant(a) == std::mem::discriminant(b)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_far_function_and_call() {
        let source = "far func target {\n nop\n}\nmain {\n call target\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 2);
    }

    #[test]
    fn parses_expression_fragment() {
        let expr = parse_expression_fragment(SourceId(0), "0x10").expect("parse");
        assert!(matches!(expr.node, Expr::Number(16)));
    }

    #[test]
    fn parses_expression_fragment_with_newline_padding() {
        let expr = parse_expression_fragment(SourceId(0), "\n0x10\n").expect("parse");
        assert!(matches!(expr.node, Expr::Number(16)));
    }

    #[test]
    fn parses_var_array_length() {
        let source = "var tiles[16]\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);

        let Item::Var(var) = &file.items[0].node else {
            panic!("expected var item");
        };

        assert!(matches!(var.array_len, Some(Expr::Number(16))));
        assert!(var.initializer.is_none());
    }
}
