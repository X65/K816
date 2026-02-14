use crate::{EvalContext, EvalError, EvalOutcome, Number, functions, number_from_f64};

#[derive(Debug, Clone, PartialEq)]
enum TokenKind {
    Number(Number),
    Ident(String),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    PlusPlus,
    MinusMinus,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    Shl,
    Shr,
    ShlEq,
    ShrEq,
    Amp,
    AmpEq,
    Pipe,
    PipeEq,
    Caret,
    CaretEq,
    AndAnd,
    OrOr,
    Bang,
    Tilde,
    Eq,
    EqEq,
    BangEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    QLess,
    QGreater,
    Question,
    Colon,
    Comma,
    Dot,
    LParen,
    RParen,
    LBracket,
    RBracket,
    End,
}

#[derive(Debug, Clone, PartialEq)]
struct Token {
    kind: TokenKind,
    start: usize,
    end: usize,
}

#[derive(Debug)]
struct Lexer<'a> {
    input: &'a str,
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            bytes: input.as_bytes(),
            pos: 0,
        }
    }

    fn lex_all(&mut self) -> Result<Vec<Token>, EvalError> {
        let mut out = Vec::new();
        loop {
            let token = self.next_token()?;
            let is_end = matches!(token.kind, TokenKind::End);
            out.push(token);
            if is_end {
                break;
            }
        }
        Ok(out)
    }

    fn next_token(&mut self) -> Result<Token, EvalError> {
        self.skip_ws();
        if self.pos >= self.bytes.len() {
            return Ok(Token {
                kind: TokenKind::End,
                start: self.pos,
                end: self.pos,
            });
        }

        let start = self.pos;

        macro_rules! punct {
            ($literal:literal, $kind:expr) => {
                if self.starts_with($literal) {
                    self.pos += $literal.len();
                    return Ok(Token {
                        kind: $kind,
                        start,
                        end: self.pos,
                    });
                }
            };
        }

        punct!("++", TokenKind::PlusPlus);
        punct!("--", TokenKind::MinusMinus);
        punct!("+=", TokenKind::PlusEq);
        punct!("-=", TokenKind::MinusEq);
        punct!("*=", TokenKind::StarEq);
        punct!("/=", TokenKind::SlashEq);
        punct!("%=", TokenKind::PercentEq);
        punct!("<<=", TokenKind::ShlEq);
        punct!(">>=", TokenKind::ShrEq);
        punct!("<<", TokenKind::Shl);
        punct!(">>", TokenKind::Shr);
        punct!("&&", TokenKind::AndAnd);
        punct!("||", TokenKind::OrOr);
        punct!("&=", TokenKind::AmpEq);
        punct!("|=", TokenKind::PipeEq);
        punct!("^=", TokenKind::CaretEq);
        punct!("==", TokenKind::EqEq);
        punct!("!=", TokenKind::BangEq);
        punct!("<=", TokenKind::LtEq);
        punct!(">=", TokenKind::GtEq);
        punct!("?<", TokenKind::QLess);
        punct!("?>", TokenKind::QGreater);

        let ch = self.bytes[self.pos] as char;
        match ch {
            '+' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Plus,
                    start,
                    end: self.pos,
                })
            }
            '-' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Minus,
                    start,
                    end: self.pos,
                })
            }
            '*' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Star,
                    start,
                    end: self.pos,
                })
            }
            '/' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Slash,
                    start,
                    end: self.pos,
                })
            }
            '%' if matches!(self.peek_char(1), Some('0' | '1')) => self.lex_prefixed_binary(),
            '%' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Percent,
                    start,
                    end: self.pos,
                })
            }
            '&' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Amp,
                    start,
                    end: self.pos,
                })
            }
            '|' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Pipe,
                    start,
                    end: self.pos,
                })
            }
            '^' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Caret,
                    start,
                    end: self.pos,
                })
            }
            '!' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Bang,
                    start,
                    end: self.pos,
                })
            }
            '~' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Tilde,
                    start,
                    end: self.pos,
                })
            }
            '=' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Eq,
                    start,
                    end: self.pos,
                })
            }
            '<' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Lt,
                    start,
                    end: self.pos,
                })
            }
            '>' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Gt,
                    start,
                    end: self.pos,
                })
            }
            '?' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Question,
                    start,
                    end: self.pos,
                })
            }
            ':' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Colon,
                    start,
                    end: self.pos,
                })
            }
            ',' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Comma,
                    start,
                    end: self.pos,
                })
            }
            '.' if matches!(self.peek_char(1), Some(next) if next.is_ascii_digit()) => {
                self.lex_decimal_or_float()
            }
            '.' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::Dot,
                    start,
                    end: self.pos,
                })
            }
            '(' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::LParen,
                    start,
                    end: self.pos,
                })
            }
            ')' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::RParen,
                    start,
                    end: self.pos,
                })
            }
            '[' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::LBracket,
                    start,
                    end: self.pos,
                })
            }
            ']' => {
                self.pos += 1;
                Ok(Token {
                    kind: TokenKind::RBracket,
                    start,
                    end: self.pos,
                })
            }
            '$' => self.lex_prefixed_hex(),
            '0' if matches!(self.peek_char(1), Some('x' | 'X')) => self.lex_base_prefixed_hex(),
            '0' if matches!(self.peek_char(1), Some('b' | 'B')) => self.lex_base_prefixed_binary(),
            '0'..='9' => self.lex_decimal_or_float(),
            'a'..='z' | 'A'..='Z' | '_' => self.lex_ident(),
            '\'' => self.lex_char_literal(),
            _ => Err(EvalError::UnexpectedToken {
                column: start + 1,
                token: format_token_for_message(&ch.to_string()),
            }),
        }
    }

    fn lex_char_literal(&mut self) -> Result<Token, EvalError> {
        let start = self.pos;
        self.pos += 1; // consume opening '
        if self.pos >= self.bytes.len() {
            return Err(EvalError::UnexpectedToken {
                column: start + 1,
                token: "'".to_string(),
            });
        }
        let ch = if self.bytes[self.pos] == b'\\' {
            self.pos += 1;
            let escaped = self.bytes.get(self.pos).copied().ok_or(EvalError::UnexpectedToken {
                column: start + 1,
                token: "'\\".to_string(),
            })?;
            self.pos += 1;
            match escaped {
                b'n' => b'\n',
                b'r' => b'\r',
                b't' => b'\t',
                b'\\' => b'\\',
                b'\'' => b'\'',
                b'0' => 0,
                other => other,
            }
        } else {
            let b = self.bytes[self.pos];
            self.pos += 1;
            b
        };
        if self.bytes.get(self.pos).copied() != Some(b'\'') {
            return Err(EvalError::UnexpectedToken {
                column: start + 1,
                token: format!("'{}", ch as char),
            });
        }
        self.pos += 1; // consume closing '
        Ok(Token {
            kind: TokenKind::Number(Number::Int(ch as i64)),
            start,
            end: self.pos,
        })
    }

    fn lex_prefixed_hex(&mut self) -> Result<Token, EvalError> {
        let start = self.pos;
        self.pos += 1;
        let digits_start = self.pos;
        while matches!(self.peek_char(0), Some(ch) if ch.is_ascii_hexdigit()) {
            self.pos += 1;
        }
        if self.pos == digits_start {
            return Err(EvalError::InvalidNumber {
                literal: self.input[start..self.pos].to_string(),
            });
        }
        let literal = &self.input[digits_start..self.pos];
        let value = i64::from_str_radix(literal, 16).map_err(|_| EvalError::InvalidNumber {
            literal: self.input[start..self.pos].to_string(),
        })?;
        Ok(Token {
            kind: TokenKind::Number(Number::Int(value)),
            start,
            end: self.pos,
        })
    }

    fn lex_base_prefixed_hex(&mut self) -> Result<Token, EvalError> {
        let start = self.pos;
        self.pos += 2;
        let digits_start = self.pos;
        while matches!(self.peek_char(0), Some(ch) if ch.is_ascii_hexdigit()) {
            self.pos += 1;
        }
        if self.pos == digits_start {
            return Err(EvalError::InvalidNumber {
                literal: self.input[start..self.pos].to_string(),
            });
        }
        let literal = &self.input[digits_start..self.pos];
        let value = i64::from_str_radix(literal, 16).map_err(|_| EvalError::InvalidNumber {
            literal: self.input[start..self.pos].to_string(),
        })?;
        Ok(Token {
            kind: TokenKind::Number(Number::Int(value)),
            start,
            end: self.pos,
        })
    }

    fn lex_prefixed_binary(&mut self) -> Result<Token, EvalError> {
        let start = self.pos;
        self.pos += 1;
        let digits_start = self.pos;
        while matches!(self.peek_char(0), Some('0' | '1')) {
            self.pos += 1;
        }
        if self.pos == digits_start {
            return Err(EvalError::InvalidNumber {
                literal: self.input[start..self.pos].to_string(),
            });
        }
        let literal = &self.input[digits_start..self.pos];
        let value = i64::from_str_radix(literal, 2).map_err(|_| EvalError::InvalidNumber {
            literal: self.input[start..self.pos].to_string(),
        })?;
        Ok(Token {
            kind: TokenKind::Number(Number::Int(value)),
            start,
            end: self.pos,
        })
    }

    fn lex_base_prefixed_binary(&mut self) -> Result<Token, EvalError> {
        let start = self.pos;
        self.pos += 2;
        let digits_start = self.pos;
        while matches!(self.peek_char(0), Some('0' | '1')) {
            self.pos += 1;
        }
        if self.pos == digits_start {
            return Err(EvalError::InvalidNumber {
                literal: self.input[start..self.pos].to_string(),
            });
        }
        let literal = &self.input[digits_start..self.pos];
        let value = i64::from_str_radix(literal, 2).map_err(|_| EvalError::InvalidNumber {
            literal: self.input[start..self.pos].to_string(),
        })?;
        Ok(Token {
            kind: TokenKind::Number(Number::Int(value)),
            start,
            end: self.pos,
        })
    }

    fn lex_decimal_or_float(&mut self) -> Result<Token, EvalError> {
        let start = self.pos;
        let mut saw_dot = false;
        let mut saw_exp = false;

        if matches!(self.peek_char(0), Some('.')) {
            saw_dot = true;
            self.pos += 1;
            while matches!(self.peek_char(0), Some(ch) if ch.is_ascii_digit()) {
                self.pos += 1;
            }
        } else {
            while matches!(self.peek_char(0), Some(ch) if ch.is_ascii_digit()) {
                self.pos += 1;
            }

            if matches!(self.peek_char(0), Some('.'))
                && matches!(self.peek_char(1), Some(ch) if ch.is_ascii_digit())
            {
                saw_dot = true;
                self.pos += 1;
                while matches!(self.peek_char(0), Some(ch) if ch.is_ascii_digit()) {
                    self.pos += 1;
                }
            }
        }

        if matches!(self.peek_char(0), Some('e' | 'E')) {
            let exp_start = self.pos;
            self.pos += 1;
            if matches!(self.peek_char(0), Some('+' | '-')) {
                self.pos += 1;
            }
            let digit_start = self.pos;
            while matches!(self.peek_char(0), Some(ch) if ch.is_ascii_digit()) {
                self.pos += 1;
            }
            if digit_start == self.pos {
                self.pos = exp_start;
            } else {
                saw_exp = true;
            }
        }

        let literal = &self.input[start..self.pos];
        let number = if saw_dot || saw_exp {
            let value = literal
                .parse::<f64>()
                .map_err(|_| EvalError::InvalidNumber {
                    literal: literal.to_string(),
                })?;
            number_from_f64(value)?
        } else {
            let value = literal
                .parse::<i64>()
                .map_err(|_| EvalError::InvalidNumber {
                    literal: literal.to_string(),
                })?;
            Number::Int(value)
        };

        Ok(Token {
            kind: TokenKind::Number(number),
            start,
            end: self.pos,
        })
    }

    fn lex_ident(&mut self) -> Result<Token, EvalError> {
        let start = self.pos;
        while matches!(self.peek_char(0), Some(ch) if ch.is_ascii_alphanumeric() || ch == '_') {
            self.pos += 1;
        }
        Ok(Token {
            kind: TokenKind::Ident(self.input[start..self.pos].to_string()),
            start,
            end: self.pos,
        })
    }

    fn skip_ws(&mut self) {
        while matches!(self.peek_char(0), Some(ch) if ch.is_ascii_whitespace()) {
            self.pos += 1;
        }
    }

    fn starts_with(&self, value: &str) -> bool {
        self.input[self.pos..].starts_with(value)
    }

    fn peek_char(&self, offset: usize) -> Option<char> {
        self.bytes.get(self.pos + offset).map(|b| *b as char)
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Number(Number),
    Ident {
        name: String,
        start: usize,
        end: usize,
    },
    ArrayLiteral(Vec<Expr>),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Conditional {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Assign {
        target: String,
        target_start: usize,
        target_end: usize,
        op: AssignOp,
        value: Box<Expr>,
    },
    Update {
        target: String,
        target_start: usize,
        target_end: usize,
        delta: i8,
        prefix: bool,
    },
    Call {
        name: String,
        args: Vec<Expr>,
    },
    Sequence(Vec<Expr>),
}

#[derive(Debug, Clone, Copy)]
enum UnaryOp {
    Plus,
    Minus,
    LogicalNot,
    BitNot,
}

#[derive(Debug, Clone, Copy)]
enum BinaryOp {
    Mul,
    Div,
    Rem,
    Add,
    Sub,
    Shl,
    Shr,
    Lt,
    Le,
    Gt,
    Ge,
    MaxSelect,
    MinSelect,
    Eq,
    Ne,
    BitAnd,
    BitXor,
    BitOr,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, Copy)]
enum AssignOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
    BitAnd,
    BitXor,
    BitOr,
}

#[derive(Debug)]
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn parse(&mut self) -> Result<Expr, EvalError> {
        let expr = self.parse_comma()?;
        if !matches!(self.current().kind, TokenKind::End) {
            return Err(self.unexpected_token());
        }
        Ok(expr)
    }

    fn parse_comma(&mut self) -> Result<Expr, EvalError> {
        let first = self.parse_assignment()?;
        let mut values = vec![first];
        while matches!(self.current().kind, TokenKind::Comma) {
            self.bump();
            values.push(self.parse_assignment()?);
        }
        if values.len() == 1 {
            Ok(values.pop().expect("single element"))
        } else {
            Ok(Expr::Sequence(values))
        }
    }

    fn parse_assignment(&mut self) -> Result<Expr, EvalError> {
        let lhs = self.parse_conditional()?;
        let Some(op) = self.consume_assign_op() else {
            return Ok(lhs);
        };
        let Expr::Ident {
            name: target,
            start: target_start,
            end: target_end,
        } = lhs
        else {
            return Err(EvalError::InvalidAssignmentTarget);
        };
        let rhs = self.parse_assignment()?;
        Ok(Expr::Assign {
            target,
            target_start,
            target_end,
            op,
            value: Box::new(rhs),
        })
    }

    fn parse_conditional(&mut self) -> Result<Expr, EvalError> {
        let condition = self.parse_logical_or()?;
        if !matches!(self.current().kind, TokenKind::Question) {
            return Ok(condition);
        }
        self.bump();
        let then_expr = self.parse_assignment()?;
        if !matches!(self.current().kind, TokenKind::Colon) {
            return Err(self.unexpected_token());
        }
        self.bump();
        let else_expr = self.parse_conditional()?;
        Ok(Expr::Conditional {
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        })
    }

    fn parse_logical_or(&mut self) -> Result<Expr, EvalError> {
        let mut lhs = self.parse_logical_and()?;
        while matches!(self.current().kind, TokenKind::OrOr) {
            self.bump();
            let rhs = self.parse_logical_and()?;
            lhs = Expr::Binary {
                op: BinaryOp::LogicalOr,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, EvalError> {
        let mut lhs = self.parse_bit_or()?;
        while matches!(self.current().kind, TokenKind::AndAnd) {
            self.bump();
            let rhs = self.parse_bit_or()?;
            lhs = Expr::Binary {
                op: BinaryOp::LogicalAnd,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_bit_or(&mut self) -> Result<Expr, EvalError> {
        let mut lhs = self.parse_bit_xor()?;
        while matches!(self.current().kind, TokenKind::Pipe) {
            self.bump();
            let rhs = self.parse_bit_xor()?;
            lhs = Expr::Binary {
                op: BinaryOp::BitOr,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_bit_xor(&mut self) -> Result<Expr, EvalError> {
        let mut lhs = self.parse_bit_and()?;
        while matches!(self.current().kind, TokenKind::Caret) {
            self.bump();
            let rhs = self.parse_bit_and()?;
            lhs = Expr::Binary {
                op: BinaryOp::BitXor,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_bit_and(&mut self) -> Result<Expr, EvalError> {
        let mut lhs = self.parse_equality()?;
        while matches!(self.current().kind, TokenKind::Amp) {
            self.bump();
            let rhs = self.parse_equality()?;
            lhs = Expr::Binary {
                op: BinaryOp::BitAnd,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_equality(&mut self) -> Result<Expr, EvalError> {
        let mut lhs = self.parse_comparison()?;
        loop {
            let op = match self.current().kind {
                TokenKind::EqEq => BinaryOp::Eq,
                TokenKind::BangEq => BinaryOp::Ne,
                _ => break,
            };
            self.bump();
            let rhs = self.parse_comparison()?;
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_comparison(&mut self) -> Result<Expr, EvalError> {
        let mut lhs = self.parse_shift()?;
        loop {
            let op = match self.current().kind {
                TokenKind::Lt => BinaryOp::Lt,
                TokenKind::LtEq => BinaryOp::Le,
                TokenKind::Gt => BinaryOp::Gt,
                TokenKind::GtEq => BinaryOp::Ge,
                TokenKind::QGreater => BinaryOp::MaxSelect,
                TokenKind::QLess => BinaryOp::MinSelect,
                _ => break,
            };
            self.bump();
            let rhs = self.parse_shift()?;
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_shift(&mut self) -> Result<Expr, EvalError> {
        let mut lhs = self.parse_add()?;
        loop {
            let op = match self.current().kind {
                TokenKind::Shl => BinaryOp::Shl,
                TokenKind::Shr => BinaryOp::Shr,
                _ => break,
            };
            self.bump();
            let rhs = self.parse_add()?;
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_add(&mut self) -> Result<Expr, EvalError> {
        let mut lhs = self.parse_mul()?;
        loop {
            let op = match self.current().kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                _ => break,
            };
            self.bump();
            let rhs = self.parse_mul()?;
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_mul(&mut self) -> Result<Expr, EvalError> {
        let mut lhs = self.parse_unary()?;
        loop {
            let op = match self.current().kind {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                TokenKind::Percent => BinaryOp::Rem,
                _ => break,
            };
            self.bump();
            let rhs = self.parse_unary()?;
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_unary(&mut self) -> Result<Expr, EvalError> {
        match self.current().kind {
            TokenKind::PlusPlus => {
                self.bump();
                let target = self.parse_unary()?;
                let Expr::Ident {
                    name: target,
                    start: target_start,
                    end: target_end,
                } = target
                else {
                    return Err(EvalError::InvalidAssignmentTarget);
                };
                Ok(Expr::Update {
                    target,
                    target_start,
                    target_end,
                    delta: 1,
                    prefix: true,
                })
            }
            TokenKind::MinusMinus => {
                self.bump();
                let target = self.parse_unary()?;
                let Expr::Ident {
                    name: target,
                    start: target_start,
                    end: target_end,
                } = target
                else {
                    return Err(EvalError::InvalidAssignmentTarget);
                };
                Ok(Expr::Update {
                    target,
                    target_start,
                    target_end,
                    delta: -1,
                    prefix: true,
                })
            }
            TokenKind::Plus => {
                self.bump();
                Ok(Expr::Unary {
                    op: UnaryOp::Plus,
                    expr: Box::new(self.parse_unary()?),
                })
            }
            TokenKind::Minus => {
                self.bump();
                Ok(Expr::Unary {
                    op: UnaryOp::Minus,
                    expr: Box::new(self.parse_unary()?),
                })
            }
            TokenKind::Bang => {
                self.bump();
                Ok(Expr::Unary {
                    op: UnaryOp::LogicalNot,
                    expr: Box::new(self.parse_unary()?),
                })
            }
            TokenKind::Tilde => {
                self.bump();
                Ok(Expr::Unary {
                    op: UnaryOp::BitNot,
                    expr: Box::new(self.parse_unary()?),
                })
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr, EvalError> {
        let mut expr = self.parse_primary()?;
        loop {
            match self.current().kind.clone() {
                TokenKind::LParen => {
                    self.bump();
                    let args = self.parse_arguments(TokenKind::RParen)?;
                    let Expr::Ident { name, .. } = expr else {
                        return Err(self.unexpected_token());
                    };
                    expr = Expr::Call { name, args };
                }
                TokenKind::LBracket => {
                    self.bump();
                    let mut args = Vec::new();
                    args.push(expr);
                    args.extend(self.parse_arguments(TokenKind::RBracket)?);
                    expr = Expr::Call {
                        name: "index".to_string(),
                        args,
                    };
                }
                TokenKind::Dot => {
                    self.bump();
                    let name = self.consume_ident()?;
                    let mut args = Vec::new();
                    args.push(expr);
                    if matches!(self.current().kind, TokenKind::LParen) {
                        self.bump();
                        args.extend(self.parse_arguments(TokenKind::RParen)?);
                    }
                    expr = Expr::Call { name, args };
                }
                TokenKind::PlusPlus => {
                    self.bump();
                    let Expr::Ident {
                        name: target,
                        start: target_start,
                        end: target_end,
                    } = expr
                    else {
                        return Err(EvalError::InvalidAssignmentTarget);
                    };
                    expr = Expr::Update {
                        target,
                        target_start,
                        target_end,
                        delta: 1,
                        prefix: false,
                    };
                }
                TokenKind::MinusMinus => {
                    self.bump();
                    let Expr::Ident {
                        name: target,
                        start: target_start,
                        end: target_end,
                    } = expr
                    else {
                        return Err(EvalError::InvalidAssignmentTarget);
                    };
                    expr = Expr::Update {
                        target,
                        target_start,
                        target_end,
                        delta: -1,
                        prefix: false,
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, EvalError> {
        match self.current().kind.clone() {
            TokenKind::Number(value) => {
                self.bump();
                Ok(Expr::Number(value))
            }
            TokenKind::Ident(name) => {
                let start = self.current().start;
                let end = self.current().end;
                self.bump();
                Ok(Expr::Ident { name, start, end })
            }
            TokenKind::LParen => {
                self.bump();
                let value = self.parse_comma()?;
                if !matches!(self.current().kind, TokenKind::RParen) {
                    return Err(self.unexpected_token());
                }
                self.bump();
                Ok(value)
            }
            TokenKind::LBracket => {
                self.bump();
                let values = self.parse_arguments(TokenKind::RBracket)?;
                Ok(Expr::ArrayLiteral(values))
            }
            TokenKind::End => Err(EvalError::UnexpectedEof),
            _ => Err(self.unexpected_token()),
        }
    }

    fn parse_arguments(&mut self, end: TokenKind) -> Result<Vec<Expr>, EvalError> {
        let mut args = Vec::new();

        if self.same_kind(&self.current().kind, &end) {
            self.bump();
            return Ok(args);
        }

        loop {
            args.push(self.parse_assignment()?);
            if matches!(self.current().kind, TokenKind::Comma) {
                self.bump();
                continue;
            }
            if self.same_kind(&self.current().kind, &end) {
                self.bump();
                break;
            }
            return Err(self.unexpected_token());
        }

        Ok(args)
    }

    fn consume_assign_op(&mut self) -> Option<AssignOp> {
        let op = match self.current().kind {
            TokenKind::Eq => AssignOp::Assign,
            TokenKind::PlusEq => AssignOp::Add,
            TokenKind::MinusEq => AssignOp::Sub,
            TokenKind::StarEq => AssignOp::Mul,
            TokenKind::SlashEq => AssignOp::Div,
            TokenKind::PercentEq => AssignOp::Rem,
            TokenKind::ShlEq => AssignOp::Shl,
            TokenKind::ShrEq => AssignOp::Shr,
            TokenKind::AmpEq => AssignOp::BitAnd,
            TokenKind::CaretEq => AssignOp::BitXor,
            TokenKind::PipeEq => AssignOp::BitOr,
            _ => return None,
        };
        self.bump();
        Some(op)
    }

    fn consume_ident(&mut self) -> Result<String, EvalError> {
        match self.current().kind.clone() {
            TokenKind::Ident(value) => {
                self.bump();
                Ok(value)
            }
            _ => Err(self.unexpected_token()),
        }
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or_else(|| {
            self.tokens
                .last()
                .expect("token stream always has an explicit end token")
        })
    }

    fn bump(&mut self) {
        self.pos += 1;
    }

    fn same_kind(&self, lhs: &TokenKind, rhs: &TokenKind) -> bool {
        std::mem::discriminant(lhs) == std::mem::discriminant(rhs)
    }

    fn unexpected_token(&self) -> EvalError {
        EvalError::UnexpectedToken {
            column: self.current().start + 1,
            token: token_kind_message(&self.current().kind),
        }
    }
}

fn format_token_for_message(token: &str) -> String {
    let escaped: String = token.chars().flat_map(char::escape_default).collect();
    format!("'{escaped}'")
}

fn token_kind_message(kind: &TokenKind) -> String {
    match kind {
        TokenKind::Number(_) => "number literal".to_string(),
        TokenKind::Ident(value) => format_token_for_message(value),
        TokenKind::Plus => "'+'".to_string(),
        TokenKind::Minus => "'-'".to_string(),
        TokenKind::Star => "'*'".to_string(),
        TokenKind::Slash => "'/'".to_string(),
        TokenKind::Percent => "'%'".to_string(),
        TokenKind::PlusPlus => "'++'".to_string(),
        TokenKind::MinusMinus => "'--'".to_string(),
        TokenKind::PlusEq => "'+='".to_string(),
        TokenKind::MinusEq => "'-='".to_string(),
        TokenKind::StarEq => "'*='".to_string(),
        TokenKind::SlashEq => "'/='".to_string(),
        TokenKind::PercentEq => "'%='".to_string(),
        TokenKind::Shl => "'<<'".to_string(),
        TokenKind::Shr => "'>>'".to_string(),
        TokenKind::ShlEq => "'<<='".to_string(),
        TokenKind::ShrEq => "'>>='".to_string(),
        TokenKind::Amp => "'&'".to_string(),
        TokenKind::AmpEq => "'&='".to_string(),
        TokenKind::Pipe => "'|'".to_string(),
        TokenKind::PipeEq => "'|='".to_string(),
        TokenKind::Caret => "'^'".to_string(),
        TokenKind::CaretEq => "'^='".to_string(),
        TokenKind::AndAnd => "'&&'".to_string(),
        TokenKind::OrOr => "'||'".to_string(),
        TokenKind::Bang => "'!'".to_string(),
        TokenKind::Tilde => "'~'".to_string(),
        TokenKind::Eq => "'='".to_string(),
        TokenKind::EqEq => "'=='".to_string(),
        TokenKind::BangEq => "'!='".to_string(),
        TokenKind::Lt => "'<'".to_string(),
        TokenKind::LtEq => "'<='".to_string(),
        TokenKind::Gt => "'>'".to_string(),
        TokenKind::GtEq => "'>='".to_string(),
        TokenKind::QLess => "'?<'".to_string(),
        TokenKind::QGreater => "'?>'".to_string(),
        TokenKind::Question => "'?'".to_string(),
        TokenKind::Colon => "':'".to_string(),
        TokenKind::Comma => "','".to_string(),
        TokenKind::Dot => "'.'".to_string(),
        TokenKind::LParen => "'('".to_string(),
        TokenKind::RParen => "')'".to_string(),
        TokenKind::LBracket => "'['".to_string(),
        TokenKind::RBracket => "']'".to_string(),
        TokenKind::End => "end of expression".to_string(),
    }
}

fn evaluate_expr(
    expr: &Expr,
    context: &mut EvalContext,
    assigned_names: &mut Vec<String>,
) -> Result<Number, EvalError> {
    match expr {
        Expr::Number(value) => Ok(*value),
        Expr::Ident { name, start, end } => resolve_identifier(name, *start, *end, context),
        Expr::ArrayLiteral(_) => Err(EvalError::ArrayLiteralInNumericContext),
        Expr::Unary { op, expr } => {
            let value = evaluate_expr(expr, context, assigned_names)?;
            match op {
                UnaryOp::Plus => Ok(value),
                UnaryOp::Minus => value.checked_neg(),
                UnaryOp::LogicalNot => Ok(Number::Int((!value.is_truthy()) as i64)),
                UnaryOp::BitNot => value.bit_not(),
            }
        }
        Expr::Binary { op, lhs, rhs } => match op {
            BinaryOp::LogicalAnd => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                if !lhs.is_truthy() {
                    return Ok(Number::Int(0));
                }
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                Ok(Number::Int(rhs.is_truthy() as i64))
            }
            BinaryOp::LogicalOr => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                if lhs.is_truthy() {
                    return Ok(Number::Int(1));
                }
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                Ok(Number::Int(rhs.is_truthy() as i64))
            }
            BinaryOp::Mul => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                lhs.checked_mul(rhs)
            }
            BinaryOp::Div => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                lhs.checked_div(rhs)
            }
            BinaryOp::Rem => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                lhs.checked_rem(rhs)
            }
            BinaryOp::Add => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                lhs.checked_add(rhs)
            }
            BinaryOp::Sub => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                lhs.checked_sub(rhs)
            }
            BinaryOp::Shl => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                lhs.checked_shl(rhs)
            }
            BinaryOp::Shr => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                lhs.checked_shr(rhs)
            }
            BinaryOp::Lt => compare(
                evaluate_expr(lhs, context, assigned_names)?,
                evaluate_expr(rhs, context, assigned_names)?,
                |lhs, rhs| lhs < rhs,
            ),
            BinaryOp::Le => compare(
                evaluate_expr(lhs, context, assigned_names)?,
                evaluate_expr(rhs, context, assigned_names)?,
                |lhs, rhs| lhs <= rhs,
            ),
            BinaryOp::Gt => compare(
                evaluate_expr(lhs, context, assigned_names)?,
                evaluate_expr(rhs, context, assigned_names)?,
                |lhs, rhs| lhs > rhs,
            ),
            BinaryOp::Ge => compare(
                evaluate_expr(lhs, context, assigned_names)?,
                evaluate_expr(rhs, context, assigned_names)?,
                |lhs, rhs| lhs >= rhs,
            ),
            BinaryOp::Eq => compare(
                evaluate_expr(lhs, context, assigned_names)?,
                evaluate_expr(rhs, context, assigned_names)?,
                |lhs, rhs| lhs == rhs,
            ),
            BinaryOp::Ne => compare(
                evaluate_expr(lhs, context, assigned_names)?,
                evaluate_expr(rhs, context, assigned_names)?,
                |lhs, rhs| lhs != rhs,
            ),
            BinaryOp::MaxSelect => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                number_from_f64(lhs.as_f64().max(rhs.as_f64()))
            }
            BinaryOp::MinSelect => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                number_from_f64(lhs.as_f64().min(rhs.as_f64()))
            }
            BinaryOp::BitAnd => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                lhs.bit_and(rhs)
            }
            BinaryOp::BitXor => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                lhs.bit_xor(rhs)
            }
            BinaryOp::BitOr => {
                let lhs = evaluate_expr(lhs, context, assigned_names)?;
                let rhs = evaluate_expr(rhs, context, assigned_names)?;
                lhs.bit_or(rhs)
            }
        },
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            let condition = evaluate_expr(condition, context, assigned_names)?;
            if condition.is_truthy() {
                evaluate_expr(then_expr, context, assigned_names)
            } else {
                evaluate_expr(else_expr, context, assigned_names)
            }
        }
        Expr::Assign {
            target,
            target_start,
            target_end,
            op,
            value,
        } => {
            if matches!(op, AssignOp::Assign)
                && let Expr::ArrayLiteral(elements) = value.as_ref()
            {
                let array = evaluate_array_literal(elements, context, assigned_names)?;
                context.set_array(target.as_str(), array);
                return Ok(Number::Int(0));
            }
            let rhs = evaluate_expr(value, context, assigned_names)?;
            let out = match op {
                AssignOp::Assign => rhs,
                AssignOp::Add => resolve_identifier(target, *target_start, *target_end, context)?
                    .checked_add(rhs)?,
                AssignOp::Sub => resolve_identifier(target, *target_start, *target_end, context)?
                    .checked_sub(rhs)?,
                AssignOp::Mul => resolve_identifier(target, *target_start, *target_end, context)?
                    .checked_mul(rhs)?,
                AssignOp::Div => resolve_identifier(target, *target_start, *target_end, context)?
                    .checked_div(rhs)?,
                AssignOp::Rem => resolve_identifier(target, *target_start, *target_end, context)?
                    .checked_rem(rhs)?,
                AssignOp::Shl => resolve_identifier(target, *target_start, *target_end, context)?
                    .checked_shl(rhs)?,
                AssignOp::Shr => resolve_identifier(target, *target_start, *target_end, context)?
                    .checked_shr(rhs)?,
                AssignOp::BitAnd => {
                    resolve_identifier(target, *target_start, *target_end, context)?.bit_and(rhs)?
                }
                AssignOp::BitXor => {
                    resolve_identifier(target, *target_start, *target_end, context)?.bit_xor(rhs)?
                }
                AssignOp::BitOr => {
                    resolve_identifier(target, *target_start, *target_end, context)?.bit_or(rhs)?
                }
            };
            write_identifier(target, out, context, assigned_names);
            Ok(out)
        }
        Expr::Update {
            target,
            target_start,
            target_end,
            delta,
            prefix,
        } => {
            let previous = resolve_identifier(target, *target_start, *target_end, context)?;
            let next = if *delta >= 0 {
                previous.checked_add(Number::Int(i64::from(*delta)))?
            } else {
                previous.checked_sub(Number::Int(i64::from(-*delta)))?
            };
            write_identifier(target, next, context, assigned_names);
            if *prefix { Ok(next) } else { Ok(previous) }
        }
        Expr::Call { name, args } => {
            if name == "index" {
                return evaluate_index_call(args, context, assigned_names);
            }
            let mut values = Vec::with_capacity(args.len());
            for arg in args {
                values.push(evaluate_expr(arg, context, assigned_names)?);
            }
            functions::invoke(name, &values)
        }
        Expr::Sequence(values) => {
            let mut last = Number::Int(0);
            for value in values {
                last = evaluate_expr(value, context, assigned_names)?;
            }
            Ok(last)
        }
    }
}

fn resolve_identifier(
    name: &str,
    start: usize,
    end: usize,
    context: &EvalContext,
) -> Result<Number, EvalError> {
    if let Some(value) = context.get(name) {
        return Ok(value);
    }
    if name.eq_ignore_ascii_case("pi") {
        return Ok(Number::Float(std::f64::consts::PI));
    }
    if name.eq_ignore_ascii_case("e") {
        return Ok(Number::Float(std::f64::consts::E));
    }
    Err(EvalError::UnknownIdentifier {
        name: name.to_string(),
        start,
        end,
    })
}

fn evaluate_array_literal(
    elements: &[Expr],
    context: &mut EvalContext,
    assigned_names: &mut Vec<String>,
) -> Result<Vec<Number>, EvalError> {
    let mut values = Vec::with_capacity(elements.len());
    for element in elements {
        values.push(evaluate_expr(element, context, assigned_names)?);
    }
    Ok(values)
}

fn evaluate_index_call(
    args: &[Expr],
    context: &mut EvalContext,
    assigned_names: &mut Vec<String>,
) -> Result<Number, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::BadArity {
            name: "index".to_string(),
            expected: 2,
            got: args.len(),
        });
    }

    let base = &args[0];
    let index = evaluate_expr(&args[1], context, assigned_names)?
        .to_i64_exact()
        .ok_or(EvalError::IntegerRequired { op: "[]" })?;
    if index < 0 {
        return Err(EvalError::Overflow);
    }
    let index = usize::try_from(index).map_err(|_| EvalError::Overflow)?;

    let array = match base {
        Expr::Ident { name, start, end } => {
            if let Some(values) = context.get_array(name) {
                values.to_vec()
            } else {
                let _ = resolve_identifier(name, *start, *end, context)?;
                return Err(EvalError::DeferredFunction {
                    name: "index".to_string(),
                    reason: "indexing is only supported on array literals",
                });
            }
        }
        Expr::ArrayLiteral(elements) => evaluate_array_literal(elements, context, assigned_names)?,
        _ => {
            let _ = evaluate_expr(base, context, assigned_names)?;
            return Err(EvalError::DeferredFunction {
                name: "index".to_string(),
                reason: "indexing is only supported on array literals",
            });
        }
    };

    array.get(index).copied().ok_or(EvalError::Overflow)
}

fn write_identifier(
    name: &str,
    value: Number,
    context: &mut EvalContext,
    assigned_names: &mut Vec<String>,
) {
    context.set(name.to_string(), value);
    if !assigned_names.iter().any(|existing| existing == name) {
        assigned_names.push(name.to_string());
    }
}

fn compare(
    lhs: Number,
    rhs: Number,
    predicate: impl FnOnce(f64, f64) -> bool,
) -> Result<Number, EvalError> {
    let result = predicate(lhs.as_f64(), rhs.as_f64());
    Ok(Number::Int(result as i64))
}

pub(crate) fn evaluate(input: &str, context: &mut EvalContext) -> Result<EvalOutcome, EvalError> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.lex_all()?;
    let mut parser = Parser::new(tokens);
    let expression = parser.parse()?;

    let mut assigned_names = Vec::new();
    let value = evaluate_expr(&expression, context, &mut assigned_names)?;
    let assigned = assigned_names
        .into_iter()
        .filter_map(|name| context.get(&name).map(|value| (name, value)))
        .collect();

    Ok(EvalOutcome { value, assigned })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shifts_and_bitwise() {
        let mut context = EvalContext::default();
        let value = evaluate("(1 << 4) | 3", &mut context).expect("eval");
        assert_eq!(value.value, Number::Int(19));
    }

    #[test]
    fn parses_dollar_prefixed_hex_literals() {
        let mut context = EvalContext::default();
        assert_eq!(
            evaluate("$FF", &mut context).expect("eval").value,
            Number::Int(255)
        );
        assert_eq!(
            evaluate("$0d + $0A", &mut context).expect("eval").value,
            Number::Int(23)
        );
    }

    #[test]
    fn rejects_invalid_dollar_hex_literal() {
        let mut context = EvalContext::default();
        let err = evaluate("$", &mut context).expect_err("expected invalid number");
        assert_eq!(
            err,
            EvalError::InvalidNumber {
                literal: "$".to_string()
            }
        );
    }

    #[test]
    fn assignment_and_mutation_work_in_one_expression() {
        let mut context = EvalContext::default();
        let outcome = evaluate("A = 1, B = 2, C = ++A + B--, C", &mut context).expect("eval");

        assert_eq!(outcome.value, Number::Int(4));
        assert_eq!(context.get("A"), Some(Number::Int(2)));
        assert_eq!(context.get("B"), Some(Number::Int(1)));
        assert_eq!(context.get("C"), Some(Number::Int(4)));
    }

    #[test]
    fn ternary_logical_and_comma_precedence_match_c_style() {
        let mut context = EvalContext::default();
        let outcome = evaluate("A = !0 ? 11 : 22, A && 3", &mut context).expect("eval");
        assert_eq!(context.get("A"), Some(Number::Int(11)));
        assert_eq!(outcome.value, Number::Int(1));
    }

    #[test]
    fn assignment_is_right_associative() {
        let mut context = EvalContext::default();
        let outcome = evaluate("A = B = 5", &mut context).expect("eval");
        assert_eq!(outcome.value, Number::Int(5));
        assert_eq!(context.get("A"), Some(Number::Int(5)));
        assert_eq!(context.get("B"), Some(Number::Int(5)));
    }

    #[test]
    fn supports_array_literal_assignment_and_indexing() {
        let mut context = EvalContext::default();
        let outcome = evaluate("arr = [10, 20, 30], arr[1]", &mut context).expect("eval");
        assert_eq!(outcome.value, Number::Int(20));
    }

    #[test]
    fn supports_index_function_on_array_literals() {
        let mut context = EvalContext::default();
        let outcome = evaluate("arr = [4, 5, 6], index(arr, 2)", &mut context).expect("eval");
        assert_eq!(outcome.value, Number::Int(6));
    }

    #[test]
    fn reports_deferred_functions() {
        let mut context = EvalContext::default();
        let err = evaluate("rnd()", &mut context).expect_err("must fail");
        assert!(matches!(
            err,
            EvalError::DeferredFunction { name, .. } if name == "rnd"
        ));
    }

    #[test]
    fn parses_char_literals() {
        let mut context = EvalContext::default();
        assert_eq!(
            evaluate("'A'", &mut context).expect("eval").value,
            Number::Int(65)
        );
        assert_eq!(
            evaluate("'0'", &mut context).expect("eval").value,
            Number::Int(48)
        );
        assert_eq!(
            evaluate("' '", &mut context).expect("eval").value,
            Number::Int(32)
        );
    }

    #[test]
    fn parses_char_literal_escapes() {
        let mut context = EvalContext::default();
        assert_eq!(
            evaluate("'\\n'", &mut context).expect("eval").value,
            Number::Int(10)
        );
        assert_eq!(
            evaluate("'\\\\'"  , &mut context).expect("eval").value,
            Number::Int(92)
        );
        assert_eq!(
            evaluate("'\\''"  , &mut context).expect("eval").value,
            Number::Int(39)
        );
        assert_eq!(
            evaluate("'\\0'", &mut context).expect("eval").value,
            Number::Int(0)
        );
    }

    #[test]
    fn char_literal_in_expression() {
        let mut context = EvalContext::default();
        assert_eq!(
            evaluate("'A' + 1", &mut context).expect("eval").value,
            Number::Int(66)
        );
    }
}
