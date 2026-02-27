use crate::ast::{HlaCpuRegister, HlaStackTarget, IndexRegister};
use crate::lexer::TokenKind;
use chumsky::{error::Rich, prelude::SimpleSpan};

pub(super) fn parse_cpu_register(value: &str) -> Option<HlaCpuRegister> {
    match value {
        "a" => Some(HlaCpuRegister::A),
        "b" => Some(HlaCpuRegister::B),
        "c" => Some(HlaCpuRegister::C),
        "d" => Some(HlaCpuRegister::D),
        "s" => Some(HlaCpuRegister::S),
        "x" => Some(HlaCpuRegister::X),
        "y" => Some(HlaCpuRegister::Y),
        _ => None,
    }
}

pub(super) fn parse_stack_target(value: &str) -> Option<HlaStackTarget> {
    if value.eq_ignore_ascii_case("a") {
        return Some(HlaStackTarget::A);
    }
    if value.eq_ignore_ascii_case("p")
        || value.eq_ignore_ascii_case("flag")
        || value.eq_ignore_ascii_case("n")
        || value.eq_ignore_ascii_case("v")
        || value.eq_ignore_ascii_case("o")
        || value.eq_ignore_ascii_case("m")
        || value.eq_ignore_ascii_case("x")
        || value.eq_ignore_ascii_case("b")
        || value.eq_ignore_ascii_case("d")
        || value.eq_ignore_ascii_case("i")
        || value.eq_ignore_ascii_case("z")
        || value.eq_ignore_ascii_case("c")
    {
        return Some(HlaStackTarget::P);
    }
    None
}

pub(super) fn parse_index_register<'src>(
    index: Option<String>,
    span: SimpleSpan,
) -> Result<Option<IndexRegister>, Rich<'src, TokenKind>> {
    match index {
        None => Ok(None),
        Some(value) if value.eq_ignore_ascii_case("x") => Ok(Some(IndexRegister::X)),
        Some(value) if value.eq_ignore_ascii_case("y") => Ok(Some(IndexRegister::Y)),
        Some(value) => Err(Rich::custom(
            span,
            format!("unsupported index register '{value}', expected 'x' or 'y'"),
        )),
    }
}

pub(super) fn is_register_name(value: &str) -> bool {
    parse_cpu_register(&value.to_ascii_lowercase()).is_some()
}

pub(super) fn resolve_transfer(dest: &str, src: &str) -> Option<&'static str> {
    match (dest, src) {
        ("x", "a") => Some("tax"),
        ("y", "a") => Some("tay"),
        ("a", "x") => Some("txa"),
        ("a", "y") => Some("tya"),
        ("x", "s") => Some("tsx"),
        ("s", "x") => Some("txs"),
        ("y", "x") => Some("txy"),
        ("x", "y") => Some("tyx"),
        ("d", "c") => Some("tcd"),
        ("s", "c") => Some("tcs"),
        ("c", "d") => Some("tdc"),
        ("c", "s") => Some("tsc"),
        _ => None,
    }
}

pub(super) fn invalid_transfer_hint(dest: &str, src: &str) -> Option<&'static str> {
    match (dest, src) {
        ("d", "a") => Some("use d=c"),
        ("s", "a") => Some("use s=c"),
        ("a", "d") => Some("use c=d"),
        ("a", "s") => Some("use c=s"),
        ("d", "s") => Some("use d=c=s"),
        ("s", "d") => Some("use s=c=d"),
        ("d", "x") => Some("use a=x then d=c"),
        ("d", "y") => Some("use a=y then d=c"),
        ("s", "y") => Some("use s=x=y"),
        ("y", "s") => Some("use y=x=s"),
        ("y", "d") => Some("use c=d then y=a"),
        ("x", "d") => Some("use c=d then x=a"),
        ("b", _) | (_, "b") => Some("use b><a to swap A and B"),
        ("c", _) | (_, "c") => {
            Some("C is the 16-bit accumulator; only d=c, s=c, c=d, c=s are valid")
        }
        _ => None,
    }
}
