use crate::{EvalError, functions};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Number(i64),
    Ident(String),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Shl,
    Shr,
    Amp,
    Pipe,
    Caret,
    Tilde,
    LParen,
    RParen,
    Comma,
    End,
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

    fn next_token(&mut self) -> Result<Token, EvalError> {
        self.skip_ws();
        if self.pos >= self.bytes.len() {
            return Ok(Token::End);
        }

        let start = self.pos;
        let c = self.bytes[self.pos] as char;
        match c {
            '0'..='9' => self.lex_number(),
            '$' => self.lex_dollar_hex(),
            'a'..='z' | 'A'..='Z' | '_' => self.lex_ident(),
            '+' => {
                self.pos += 1;
                Ok(Token::Plus)
            }
            '-' => {
                self.pos += 1;
                Ok(Token::Minus)
            }
            '*' => {
                self.pos += 1;
                Ok(Token::Star)
            }
            '/' => {
                self.pos += 1;
                Ok(Token::Slash)
            }
            '%' => {
                self.pos += 1;
                Ok(Token::Percent)
            }
            '&' => {
                self.pos += 1;
                Ok(Token::Amp)
            }
            '|' => {
                self.pos += 1;
                Ok(Token::Pipe)
            }
            '^' => {
                self.pos += 1;
                Ok(Token::Caret)
            }
            '~' => {
                self.pos += 1;
                Ok(Token::Tilde)
            }
            '(' => {
                self.pos += 1;
                Ok(Token::LParen)
            }
            ')' => {
                self.pos += 1;
                Ok(Token::RParen)
            }
            ',' => {
                self.pos += 1;
                Ok(Token::Comma)
            }
            '<' if self.peek_char(1) == Some('<') => {
                self.pos += 2;
                Ok(Token::Shl)
            }
            '>' if self.peek_char(1) == Some('>') => {
                self.pos += 2;
                Ok(Token::Shr)
            }
            _ => Err(EvalError::UnexpectedToken { column: start + 1 }),
        }
    }

    fn lex_number(&mut self) -> Result<Token, EvalError> {
        let start = self.pos;
        if self.starts_with("0x") || self.starts_with("0X") {
            return self.lex_prefixed_hex(start, 2);
        }

        while self.pos < self.bytes.len() {
            let c = self.bytes[self.pos] as char;
            if c.is_ascii_digit() {
                self.pos += 1;
            } else {
                break;
            }
        }

        let literal = &self.input[start..self.pos];
        let value = literal
            .parse::<i64>()
            .map_err(|_| EvalError::InvalidNumber {
                literal: literal.to_string(),
            })?;
        Ok(Token::Number(value))
    }

    fn lex_dollar_hex(&mut self) -> Result<Token, EvalError> {
        let start = self.pos;
        self.lex_prefixed_hex(start, 1)
    }

    fn lex_prefixed_hex(&mut self, start: usize, prefix_len: usize) -> Result<Token, EvalError> {
        self.pos += prefix_len;
        let hex_start = self.pos;
        while self.pos < self.bytes.len() {
            let c = self.bytes[self.pos] as char;
            if c.is_ascii_hexdigit() {
                self.pos += 1;
            } else {
                break;
            }
        }
        if self.pos == hex_start {
            return Err(EvalError::InvalidNumber {
                literal: self.input[start..self.pos].to_string(),
            });
        }
        let literal = &self.input[hex_start..self.pos];
        let value = i64::from_str_radix(literal, 16).map_err(|_| EvalError::InvalidNumber {
            literal: self.input[start..self.pos].to_string(),
        })?;
        Ok(Token::Number(value))
    }

    fn lex_ident(&mut self) -> Result<Token, EvalError> {
        let start = self.pos;
        while self.pos < self.bytes.len() {
            let c = self.bytes[self.pos] as char;
            if c.is_ascii_alphanumeric() || c == '_' {
                self.pos += 1;
            } else {
                break;
            }
        }
        Ok(Token::Ident(self.input[start..self.pos].to_string()))
    }

    fn skip_ws(&mut self) {
        while self.pos < self.bytes.len() {
            let c = self.bytes[self.pos] as char;
            if c.is_ascii_whitespace() {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn starts_with(&self, needle: &str) -> bool {
        self.input[self.pos..].starts_with(needle)
    }

    fn peek_char(&self, offset: usize) -> Option<char> {
        self.bytes.get(self.pos + offset).map(|b| *b as char)
    }
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

    fn parse(&mut self) -> Result<i64, EvalError> {
        let value = self.parse_bit_or()?;
        if self.current() != &Token::End {
            return Err(EvalError::UnexpectedToken {
                column: self.pos + 1,
            });
        }
        Ok(value)
    }

    fn parse_bit_or(&mut self) -> Result<i64, EvalError> {
        let mut lhs = self.parse_bit_xor()?;
        while matches!(self.current(), Token::Pipe) {
            self.bump();
            let rhs = self.parse_bit_xor()?;
            lhs |= rhs;
        }
        Ok(lhs)
    }

    fn parse_bit_xor(&mut self) -> Result<i64, EvalError> {
        let mut lhs = self.parse_bit_and()?;
        while matches!(self.current(), Token::Caret) {
            self.bump();
            let rhs = self.parse_bit_and()?;
            lhs ^= rhs;
        }
        Ok(lhs)
    }

    fn parse_bit_and(&mut self) -> Result<i64, EvalError> {
        let mut lhs = self.parse_shift()?;
        while matches!(self.current(), Token::Amp) {
            self.bump();
            let rhs = self.parse_shift()?;
            lhs &= rhs;
        }
        Ok(lhs)
    }

    fn parse_shift(&mut self) -> Result<i64, EvalError> {
        let mut lhs = self.parse_add()?;
        loop {
            match self.current() {
                Token::Shl => {
                    self.bump();
                    let rhs = self.parse_add()?;
                    lhs = lhs.checked_shl(rhs as u32).ok_or(EvalError::Overflow)?;
                }
                Token::Shr => {
                    self.bump();
                    let rhs = self.parse_add()?;
                    lhs = lhs.checked_shr(rhs as u32).ok_or(EvalError::Overflow)?;
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_add(&mut self) -> Result<i64, EvalError> {
        let mut lhs = self.parse_mul()?;
        loop {
            match self.current() {
                Token::Plus => {
                    self.bump();
                    let rhs = self.parse_mul()?;
                    lhs = lhs.checked_add(rhs).ok_or(EvalError::Overflow)?;
                }
                Token::Minus => {
                    self.bump();
                    let rhs = self.parse_mul()?;
                    lhs = lhs.checked_sub(rhs).ok_or(EvalError::Overflow)?;
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_mul(&mut self) -> Result<i64, EvalError> {
        let mut lhs = self.parse_unary()?;
        loop {
            match self.current() {
                Token::Star => {
                    self.bump();
                    let rhs = self.parse_unary()?;
                    lhs = lhs.checked_mul(rhs).ok_or(EvalError::Overflow)?;
                }
                Token::Slash => {
                    self.bump();
                    let rhs = self.parse_unary()?;
                    if rhs == 0 {
                        return Err(EvalError::DivisionByZero);
                    }
                    lhs = lhs.checked_div(rhs).ok_or(EvalError::Overflow)?;
                }
                Token::Percent => {
                    self.bump();
                    let rhs = self.parse_unary()?;
                    if rhs == 0 {
                        return Err(EvalError::DivisionByZero);
                    }
                    lhs = lhs.checked_rem(rhs).ok_or(EvalError::Overflow)?;
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_unary(&mut self) -> Result<i64, EvalError> {
        match self.current() {
            Token::Plus => {
                self.bump();
                self.parse_unary()
            }
            Token::Minus => {
                self.bump();
                let value = self.parse_unary()?;
                value.checked_neg().ok_or(EvalError::Overflow)
            }
            Token::Tilde => {
                self.bump();
                let value = self.parse_unary()?;
                Ok(!value)
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<i64, EvalError> {
        match self.current() {
            Token::Number(value) => {
                let value = *value;
                self.bump();
                Ok(value)
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.bump();
                if matches!(self.current(), Token::LParen) {
                    self.bump();
                    let mut args = Vec::new();
                    if !matches!(self.current(), Token::RParen) {
                        loop {
                            args.push(self.parse_bit_or()?);
                            if matches!(self.current(), Token::Comma) {
                                self.bump();
                                continue;
                            }
                            break;
                        }
                    }
                    self.expect(&Token::RParen)?;
                    return functions::invoke(&name, &args);
                }
                Err(EvalError::UnknownIdentifier { name })
            }
            Token::LParen => {
                self.bump();
                let value = self.parse_bit_or()?;
                self.expect(&Token::RParen)?;
                Ok(value)
            }
            Token::End => Err(EvalError::UnexpectedEof),
            _ => Err(EvalError::UnexpectedToken {
                column: self.pos + 1,
            }),
        }
    }

    fn expect(&mut self, token: &Token) -> Result<(), EvalError> {
        if self.current() == token {
            self.bump();
            Ok(())
        } else {
            Err(EvalError::UnexpectedToken {
                column: self.pos + 1,
            })
        }
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::End)
    }

    fn bump(&mut self) {
        self.pos += 1;
    }
}

pub(crate) fn evaluate(input: &str) -> Result<i64, EvalError> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token()?;
        let is_end = token == Token::End;
        tokens.push(token);
        if is_end {
            break;
        }
    }

    let mut parser = Parser::new(tokens);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shifts_and_bitwise() {
        assert_eq!(evaluate("(1 << 4) | 3").expect("eval"), 19);
    }

    #[test]
    fn parses_dollar_prefixed_hex_literals() {
        assert_eq!(evaluate("$FF").expect("eval"), 255);
        assert_eq!(evaluate("$0d + $0A").expect("eval"), 23);
    }

    #[test]
    fn rejects_invalid_dollar_hex_literal() {
        let err = evaluate("$").expect_err("expected invalid number");
        assert_eq!(
            err,
            EvalError::InvalidNumber {
                literal: "$".to_string()
            }
        );
    }
}
