/*****************************************************************************/
/*                                                                           */
/*                                  items.rs                                 */
/*                                                                           */
/*             A ca65 file as items, with the widths ca65 assumed            */
/*                                                                           */
/*                                                                           */
/* Copyright (c) 2026 Tomasz Sterna (smokku)                                 */
/*                                                                           */
/* Permission to use, copy, modify, and/or distribute this software for any  */
/* purpose with or without fee is hereby granted.                            */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  */
/* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   */
/* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            */
/*                                                                           */
/*****************************************************************************/

//! The structure of a ca65 file, and the register widths ca65 assembled each
//! instruction with.
//!
//! ca65's `.a8`/`.a16`/`.i8`/`.i16` are assertions it sizes immediates by and
//! checks against nothing, and they follow the *text*: the state at a line is
//! whatever the last directive above it said, whatever the control flow. That
//! is the state this module records on every instruction, because it is the
//! one the original was assembled with. What the program does at run time is
//! the compiler's to work out from the converted source, not this module's.

use super::expr::{self, Expr};
use super::lex::{Tok, Token};
use super::source::{self, Body, Label, Line};

/// A register width.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Width {
    W8,
    W16,
}

impl Width {
    pub fn bits(self) -> u8 {
        match self {
            Self::W8 => 8,
            Self::W16 => 16,
        }
    }
}

/// What ca65 believes about the two widths at a line.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Widths {
    pub a: Width,
    pub i: Width,
    /// Whether a directive ever stated the axis. ca65 starts a 65816 file at
    /// eight bits on both, and a width nobody wrote is not one a converted
    /// section should claim.
    pub a_stated: bool,
    pub i_stated: bool,
}

impl Default for Widths {
    fn default() -> Self {
        Self {
            a: Width::W8,
            i: Width::W8,
            a_stated: false,
            i_stated: false,
        }
    }
}

/// A change of width on either axis or both.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Axes {
    pub a: Option<Width>,
    pub i: Option<Width>,
}

impl Axes {
    pub fn apply(self, widths: &mut Widths) {
        if let Some(a) = self.a {
            widths.a = a;
            widths.a_stated = true;
        }
        if let Some(i) = self.i {
            widths.i = i;
            widths.i_stated = true;
        }
    }

    pub fn is_empty(self) -> bool {
        self.a.is_none() && self.i.is_none()
    }

    /// The K816 annotations, `@a8 @i16`.
    pub fn annotations(self) -> String {
        let mut out = Vec::new();
        if let Some(a) = self.a {
            out.push(format!("@a{}", a.bits()));
        }
        if let Some(i) = self.i {
            out.push(format!("@i{}", i.bits()));
        }
        out.join(" ")
    }
}

/// The width a `sep`/`rep` mask establishes.
pub fn mask_axes(mnemonic: &str, mask: u32) -> Axes {
    let width = if mnemonic == "sep" {
        Width::W8
    } else {
        Width::W16
    };
    Axes {
        a: (mask & 0x20 != 0).then_some(width),
        i: (mask & 0x10 != 0).then_some(width),
    }
}

/// A macro: its parameters and its body, kept as lines.
#[derive(Clone, Debug)]
pub struct Macro {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Line>,
    pub line: Line,
    pub end: Line,
}

/// One member of a `.struct`.
#[derive(Clone, Debug)]
pub struct Member {
    /// `None` for an anonymous member, which reserves bytes and names none.
    pub name: Option<String>,
    pub kind: MemberKind,
    pub line: Line,
}

/// What a member holds.
#[derive(Clone, Debug)]
pub enum MemberKind {
    /// `.byte`, `.word`/`.addr`, `.faraddr`, `.dword`, `.res`: an element of
    /// `size` bytes, `count` of them.
    Data { size: u32, count: Option<Expr> },
    /// `.tag NAME`.
    Tag(String),
    /// `.org ADDR`, which makes the struct a layout at an address.
    Org(Expr),
    /// A comment line or a blank one, kept for the layout's text.
    Trivia,
}

/// A `.struct`.
#[derive(Clone, Debug)]
pub struct Struct {
    pub name: String,
    pub members: Vec<Member>,
    pub line: Line,
    pub end: Line,
}

/// One `.enum` member.
#[derive(Clone, Debug)]
pub struct EnumMember {
    /// `None` for a comment or blank line.
    pub name: Option<String>,
    pub value: Option<Expr>,
    pub line: Line,
}

/// A `.enum`.
#[derive(Clone, Debug)]
pub struct Enum {
    pub name: Option<String>,
    pub members: Vec<EnumMember>,
    pub line: Line,
    pub end: Line,
}

/// One branch of a `.if` chain.
#[derive(Clone, Debug)]
pub struct Branch {
    /// The directive that opened it — `if`, `ifdef`, `elseif`, `else` — and
    /// its condition.
    pub directive: String,
    pub condition: Vec<Token>,
    pub line: Line,
    pub items: Vec<Item>,
}

/// What one line, or one construct, contributes.
#[derive(Clone, Debug)]
pub enum Item {
    /// A blank line, or one with only a comment.
    Trivia(Line),
    /// A label definition. A line with two labels yields two.
    Label(Label, Line),
    /// An instruction, with the widths ca65 assembled it with.
    Instruction {
        mnemonic: String,
        operand: Vec<Token>,
        widths: Widths,
        line: Line,
    },
    /// A width the source sets: a `sep`/`rep` and the directives after it, or
    /// a macro that is only that.
    WidthSet {
        /// What the directives say, which is what ca65 believes.
        axes: Axes,
        /// The `sep`/`rep` that does it.
        mnemonic: String,
        mask: u32,
        /// The directive lines gathered after a `sep`/`rep`; empty for a
        /// macro, whose one line is `line`.
        more: Vec<Line>,
        line: Line,
    },
    /// A width directive with nothing setting it: an assertion.
    WidthAssert {
        axes: Axes,
        line: Line,
    },
    /// A macro invocation, with its arguments split at commas.
    MacroUse {
        name: String,
        args: Vec<Vec<Token>>,
        widths: Widths,
        line: Line,
    },
    /// A data or storage directive, `.byte`, `.res`, `.tag`, `.align`, ….
    Data {
        directive: String,
        args: Vec<Token>,
        line: Line,
    },
    /// `.segment "NAME"` and the shorthands, as the segment's name.
    Segment(String, Line),
    /// `name = value` / `name := value`.
    Assign {
        name: String,
        value: Expr,
        label: bool,
        line: Line,
    },
    /// `.define NAME value`.
    Define {
        name: String,
        value: Vec<Token>,
        line: Line,
    },
    Struct(Struct),
    Enum(Enum),
    Macro(Macro),
    /// `.proc NAME` … `.endproc`.
    Proc {
        name: String,
        items: Vec<Item>,
        line: Line,
        end: Line,
    },
    /// `.repeat COUNT[, VAR]` … `.endrep`.
    Repeat {
        count: Expr,
        var: Option<String>,
        items: Vec<Item>,
        line: Line,
        end: Line,
    },
    /// `.if` … `.endif`.
    If {
        branches: Vec<Branch>,
        end: Line,
    },
    /// `.include "FILE"`.
    Include(String, Line),
    /// `.export`, `.import`, `.global` and their zero-page forms.
    Linkage {
        directive: String,
        names: Vec<String>,
        line: Line,
    },
    /// `.org ADDR`, or `.reloc` as `None`.
    Org(Option<Expr>, Line),
    /// `.macpack NAME`.
    Macpack(String, Line),
    /// A directive that changes nothing K816 writes: `.setcpu`, `.p816`,
    /// `.feature`, `.debuginfo`, `.smart`.
    Ignored(Line),
    /// A line this converter cannot convert, and why.
    Unsupported(Line, String),
}

impl Item {
    /// The line the item starts on.
    pub fn line(&self) -> &Line {
        match self {
            Self::Trivia(line)
            | Self::Label(_, line)
            | Self::Instruction { line, .. }
            | Self::WidthSet { line, .. }
            | Self::WidthAssert { line, .. }
            | Self::MacroUse { line, .. }
            | Self::Data { line, .. }
            | Self::Segment(_, line)
            | Self::Assign { line, .. }
            | Self::Define { line, .. }
            | Self::Proc { line, .. }
            | Self::Repeat { line, .. }
            | Self::Include(_, line)
            | Self::Linkage { line, .. }
            | Self::Org(_, line)
            | Self::Macpack(_, line)
            | Self::Ignored(line)
            | Self::Unsupported(line, _) => line,
            Self::Struct(definition) => &definition.line,
            Self::Enum(definition) => &definition.line,
            Self::Macro(definition) => &definition.line,
            Self::If { branches, .. } => &branches[0].line,
        }
    }
}

/// Data directives: their names, and the bytes one value occupies (`0` for
/// the ones whose size is not per value).
pub const DATA_DIRECTIVES: [(&str, u32); 14] = [
    ("byte", 1),
    ("byt", 1),
    ("db", 1),
    ("asciiz", 0),
    ("word", 2),
    ("addr", 2),
    ("dw", 2),
    ("dbyt", 2),
    ("faraddr", 3),
    ("dword", 4),
    ("res", 0),
    ("tag", 0),
    ("align", 0),
    ("incbin", 0),
];

/// A width macro: one whose body is a `sep`/`rep` and the directives that
/// state what it set, and nothing else. `_a16` in most ca65 sources.
pub fn width_macro(definition: &Macro) -> Option<(String, u32, Axes)> {
    let mut set: Option<(String, u32)> = None;
    let mut axes = Axes::default();
    for line in &definition.body {
        if !line.labels.is_empty() {
            return None;
        }
        match &line.body {
            None => {}
            Some(Body::Instruction { mnemonic, operand })
                if (mnemonic == "sep" || mnemonic == "rep") && set.is_none() =>
            {
                set = Some((mnemonic.clone(), immediate(operand)?));
            }
            Some(Body::Directive { name, args }) if args.is_empty() => {
                apply_directive(&mut axes, name)?;
            }
            _ => return None,
        }
    }
    let (mnemonic, mask) = set?;
    (!axes.is_empty()).then_some((mnemonic, mask, axes))
}

/// `#value` as a number, for a `sep`/`rep` mask.
pub fn immediate(operand: &[Token]) -> Option<u32> {
    match operand {
        [
            Token {
                tok: Tok::Punct("#"),
                ..
            },
            Token {
                tok: Tok::Number { value, .. },
                ..
            },
        ] => Some(*value),
        _ => None,
    }
}

/// Records a width directive's axis, or `None` when `name` is not one.
fn apply_directive(axes: &mut Axes, name: &str) -> Option<()> {
    match name {
        "a8" => axes.a = Some(Width::W8),
        "a16" => axes.a = Some(Width::W16),
        "i8" => axes.i = Some(Width::W8),
        "i16" => axes.i = Some(Width::W16),
        _ => return None,
    }
    Some(())
}

/// The state an item list builds with: the widths, and the macros it has
/// seen, which is what decides how much an invocation changes them.
pub struct Builder<'m> {
    pub widths: Widths,
    pub macros: &'m dyn Fn(&str) -> Option<Macro>,
}

impl Builder<'_> {
    /// Builds the items of `lines`, consuming every line up to the end or to
    /// the directive in `until`, which is returned with what it closed.
    pub fn items(
        &mut self,
        lines: &[Line],
        at: &mut usize,
        until: &[&str],
    ) -> (Vec<Item>, Option<Line>) {
        let mut items: Vec<Item> = Vec::new();
        while *at < lines.len() {
            let line = lines[*at].clone();
            *at += 1;
            if let Some((name, _)) = line.directive()
                && until.contains(&name)
            {
                // A label on the closing line still belongs to the body.
                for label in &line.labels {
                    items.push(Item::Label(label.clone(), line.clone()));
                }
                return (items, Some(line));
            }
            for label in &line.labels {
                items.push(Item::Label(label.clone(), line.clone()));
            }
            let Some(body) = line.body.clone() else {
                if line.labels.is_empty() {
                    items.push(Item::Trivia(line));
                }
                continue;
            };
            match body {
                Body::Instruction { mnemonic, operand } => {
                    items.push(Item::Instruction {
                        mnemonic,
                        operand,
                        widths: self.widths,
                        line,
                    });
                }
                Body::Assign { name, value, label } => match expr::parse_all(&value) {
                    Ok(value) => items.push(Item::Assign {
                        name,
                        value,
                        label,
                        line,
                    }),
                    Err(why) => items.push(Item::Unsupported(line, why)),
                },
                Body::Name { name, rest } => {
                    let widths = self.widths;
                    if let Some(definition) = (self.macros)(&name) {
                        if let Some((mnemonic, mask, axes)) = width_macro(&definition) {
                            axes.apply(&mut self.widths);
                            items.push(Item::WidthSet {
                                axes,
                                mnemonic,
                                mask,
                                more: Vec::new(),
                                line,
                            });
                            continue;
                        }
                        self.through_macro(&definition);
                    }
                    let args = source::split_commas(&rest)
                        .into_iter()
                        .map(<[Token]>::to_vec)
                        .collect();
                    items.push(Item::MacroUse {
                        name,
                        args,
                        widths,
                        line,
                    });
                }
                Body::Directive { name, args } => {
                    self.directive(&name, args, line, lines, at, &mut items);
                }
            }
        }
        (items, None)
    }

    /// The widths after a macro whose body states some.
    fn through_macro(&mut self, definition: &Macro) {
        for line in &definition.body {
            if let Some((name, args)) = line.directive()
                && args.is_empty()
            {
                let mut axes = Axes::default();
                if apply_directive(&mut axes, name).is_some() {
                    axes.apply(&mut self.widths);
                }
            }
        }
    }

    fn directive(
        &mut self,
        name: &str,
        args: Vec<Token>,
        line: Line,
        lines: &[Line],
        at: &mut usize,
        items: &mut Vec<Item>,
    ) {
        let mut axes = Axes::default();
        if args.is_empty() && apply_directive(&mut axes, name).is_some() {
            axes.apply(&mut self.widths);
            join_width(items, axes, line);
            return;
        }
        let item = match name {
            "segment" => match args.as_slice() {
                [
                    Token {
                        tok: Tok::Str { text, .. },
                        ..
                    },
                    ..,
                ] => Item::Segment(text.clone(), line),
                _ => Item::Unsupported(line, "a `.segment` names no segment".to_owned()),
            },
            "code" | "rodata" | "data" | "bss" | "zeropage" => {
                Item::Segment(name.to_ascii_uppercase(), line)
            }
            "include" => match args.as_slice() {
                [
                    Token {
                        tok: Tok::Str { text, .. },
                        ..
                    },
                ] => Item::Include(text.clone(), line),
                _ => Item::Unsupported(line, "an `.include` names no file".to_owned()),
            },
            "export" | "exportzp" | "import" | "importzp" | "global" | "globalzp" => {
                Item::Linkage {
                    directive: name.to_owned(),
                    names: linkage_names(&args),
                    line,
                }
            }
            "define" => match args.split_first() {
                Some((
                    Token {
                        tok: Tok::Ident(defined),
                        ..
                    },
                    value,
                )) => Item::Define {
                    name: defined.clone(),
                    value: value.to_vec(),
                    line,
                },
                _ => Item::Unsupported(line, "a `.define` names nothing".to_owned()),
            },
            "org" => match expr::parse_all(&args) {
                Ok(address) => Item::Org(Some(address), line),
                Err(why) => Item::Unsupported(line, why),
            },
            "reloc" => Item::Org(None, line),
            "macpack" => match args.as_slice() {
                [
                    Token {
                        tok: Tok::Ident(package),
                        ..
                    },
                ] => Item::Macpack(package.to_ascii_lowercase(), line),
                _ => Item::Unsupported(line, "a `.macpack` names no package".to_owned()),
            },
            "setcpu" | "p816" | "p02" | "pc02" | "feature" | "debuginfo" | "smart" | "list"
            | "listbytes" | "pagelength" | "case" | "autoimport" | "fileopt" | "fopt" => {
                Item::Ignored(line)
            }
            "macro" | "mac" => match self.read_macro(args, line, lines, at) {
                Ok(definition) => Item::Macro(definition),
                Err(refused) => Item::Unsupported(refused.0, refused.1),
            },
            "struct" => self.read_struct(args, line, lines, at),
            "enum" => self.read_enum(args, line, lines, at),
            "proc" => {
                let Some(Token {
                    tok: Tok::Ident(proc),
                    ..
                }) = args.first()
                else {
                    items.push(Item::Unsupported(
                        line,
                        "a `.proc` names nothing".to_owned(),
                    ));
                    return;
                };
                let proc = proc.clone();
                let (body, end) = self.items(lines, at, &["endproc"]);
                match end {
                    Some(end) => Item::Proc {
                        name: proc,
                        items: body,
                        line,
                        end,
                    },
                    None => Item::Unsupported(line, "a `.proc` with no `.endproc`".to_owned()),
                }
            }
            "repeat" | "rep" => {
                let parts = source::split_commas(&args);
                let count = parts.first().map(|count| expr::parse_all(count));
                let var = match parts.get(1) {
                    Some(
                        [
                            Token {
                                tok: Tok::Ident(var),
                                ..
                            },
                        ],
                    ) => Some(var.clone()),
                    _ => None,
                };
                let (body, end) = self.items(lines, at, &["endrep", "endrepeat"]);
                match (count, end) {
                    (Some(Ok(count)), Some(end)) => Item::Repeat {
                        count,
                        var,
                        items: body,
                        line,
                        end,
                    },
                    (Some(Err(why)), _) => Item::Unsupported(line, why),
                    _ => Item::Unsupported(line, "a `.repeat` with no count or no end".to_owned()),
                }
            }
            "if" | "ifdef" | "ifndef" | "ifconst" | "ifblank" | "ifnblank" => {
                self.read_if(name, args, line, lines, at)
            }
            _ if DATA_DIRECTIVES.iter().any(|(data, _)| *data == name) => Item::Data {
                directive: name.to_owned(),
                args,
                line,
            },
            _ => Item::Unsupported(
                line,
                format!("`.{name}` is a directive this converter does not convert"),
            ),
        };
        items.push(item);
    }

    fn read_macro(
        &mut self,
        args: Vec<Token>,
        line: Line,
        lines: &[Line],
        at: &mut usize,
    ) -> Result<Macro, Box<(Line, String)>> {
        let mut names = args.iter().filter_map(|token| match &token.tok {
            Tok::Ident(name) => Some(name.clone()),
            _ => None,
        });
        let Some(name) = names.next() else {
            return Err(Box::new((line, "a `.macro` names nothing".to_owned())));
        };
        let params = names.collect();
        let mut body = Vec::new();
        while *at < lines.len() {
            let candidate = lines[*at].clone();
            *at += 1;
            if let Some((directive, _)) = candidate.directive()
                && (directive == "endmacro" || directive == "endmac")
            {
                return Ok(Macro {
                    name,
                    params,
                    body,
                    line,
                    end: candidate,
                });
            }
            body.push(candidate);
        }
        Err(Box::new((
            line,
            "a `.macro` with no `.endmacro`".to_owned(),
        )))
    }

    fn read_struct(
        &mut self,
        args: Vec<Token>,
        line: Line,
        lines: &[Line],
        at: &mut usize,
    ) -> Item {
        let Some(Token {
            tok: Tok::Ident(name),
            ..
        }) = args.first()
        else {
            return Item::Unsupported(line, "an anonymous `.struct` is not converted".to_owned());
        };
        let name = name.clone();
        let mut members = Vec::new();
        while *at < lines.len() {
            let member = lines[*at].clone();
            *at += 1;
            let (field, directive, rest) = match &member.body {
                None => {
                    members.push(Member {
                        name: None,
                        kind: MemberKind::Trivia,
                        line: member,
                    });
                    continue;
                }
                Some(Body::Directive {
                    name: directive,
                    args,
                }) if directive == "endstruct" => {
                    let _ = args;
                    return Item::Struct(Struct {
                        name,
                        members,
                        line,
                        end: member,
                    });
                }
                Some(Body::Directive {
                    name: directive,
                    args,
                }) => (None, directive.clone(), args.clone()),
                Some(Body::Name { name: field, rest }) => match rest.split_first() {
                    Some((
                        Token {
                            tok: Tok::Directive(directive),
                            ..
                        },
                        rest,
                    )) => (Some(field.clone()), directive.clone(), rest.to_vec()),
                    _ => {
                        let why = format!("`.struct {name}` member `{field}` has no type");
                        return Item::Unsupported(member.clone(), why);
                    }
                },
                _ => {
                    return Item::Unsupported(
                        member,
                        format!("a line in `.struct {name}` this converter does not read"),
                    );
                }
            };
            let count = || -> Result<Option<Expr>, String> {
                if rest.is_empty() {
                    Ok(None)
                } else {
                    expr::parse_all(&rest).map(Some)
                }
            };
            let kind = match directive.as_str() {
                "byte" | "byt" | "db" => count().map(|count| MemberKind::Data { size: 1, count }),
                "word" | "addr" | "dw" => count().map(|count| MemberKind::Data { size: 2, count }),
                "faraddr" => count().map(|count| MemberKind::Data { size: 3, count }),
                "dword" => count().map(|count| MemberKind::Data { size: 4, count }),
                "res" => expr::parse_all(&rest).map(|count| MemberKind::Data {
                    size: 1,
                    count: Some(count),
                }),
                "tag" => match rest.as_slice() {
                    [
                        Token {
                            tok: Tok::Ident(tag),
                            ..
                        },
                    ] => Ok(MemberKind::Tag(tag.clone())),
                    _ => Err("a `.tag` names no struct".to_owned()),
                },
                "org" => expr::parse_all(&rest).map(MemberKind::Org),
                other => Err(format!(
                    "`.{other}` is not a struct member this converter reads"
                )),
            };
            match kind {
                Ok(kind) => members.push(Member {
                    name: field,
                    kind,
                    line: member,
                }),
                Err(why) => return Item::Unsupported(member, why),
            }
        }
        Item::Unsupported(line, format!("`.struct {name}` has no `.endstruct`"))
    }

    fn read_enum(&mut self, args: Vec<Token>, line: Line, lines: &[Line], at: &mut usize) -> Item {
        let name = match args.first() {
            Some(Token {
                tok: Tok::Ident(name),
                ..
            }) => Some(name.clone()),
            _ => None,
        };
        let mut members = Vec::new();
        while *at < lines.len() {
            let member = lines[*at].clone();
            *at += 1;
            match member.body.clone() {
                None => members.push(EnumMember {
                    name: None,
                    value: None,
                    line: member,
                }),
                Some(Body::Directive {
                    name: directive, ..
                }) if directive == "endenum" => {
                    return Item::Enum(Enum {
                        name,
                        members,
                        line,
                        end: member,
                    });
                }
                Some(Body::Name { name: field, rest }) if rest.is_empty() => {
                    members.push(EnumMember {
                        name: Some(field),
                        value: None,
                        line: member,
                    });
                }
                Some(Body::Assign {
                    name: field, value, ..
                }) => match expr::parse_all(&value) {
                    Ok(value) => members.push(EnumMember {
                        name: Some(field),
                        value: Some(value),
                        line: member,
                    }),
                    Err(why) => return Item::Unsupported(member, why),
                },
                _ => {
                    return Item::Unsupported(
                        member,
                        "a line in `.enum` this converter does not read".to_owned(),
                    );
                }
            }
        }
        Item::Unsupported(line, "a `.enum` has no `.endenum`".to_owned())
    }

    fn read_if(
        &mut self,
        name: &str,
        args: Vec<Token>,
        line: Line,
        lines: &[Line],
        at: &mut usize,
    ) -> Item {
        let mut branches = Vec::new();
        let mut directive = name.to_owned();
        let mut condition = args;
        let mut opener = line;
        loop {
            let entry = self.widths;
            let (items, end) = self.items(lines, at, &["elseif", "else", "endif"]);
            branches.push(Branch {
                directive: directive.clone(),
                condition: condition.clone(),
                line: opener.clone(),
                items,
            });
            let Some(end) = end else {
                return Item::Unsupported(opener, "a `.if` with no `.endif`".to_owned());
            };
            let (closing, args) = end
                .directive()
                .map(|(name, args)| (name.to_owned(), args.to_vec()))
                .unwrap_or_default();
            if closing == "endif" {
                return Item::If { branches, end };
            }
            // ca65 does not join the branches' widths either: the state after
            // the chain is what the last branch's text left.
            let _ = entry;
            directive = closing;
            condition = args;
            opener = end;
        }
    }
}

/// A width directive, joined to the `sep`/`rep` above it when that is what
/// set the width.
///
/// The directive joins when the instruction is the item immediately before
/// it, unlabeled, and its mask covers the directive's axis in the direction
/// the directive says; a second directive joins the set the first made.
/// Anything else leaves the directive an assertion about a width nothing on
/// the line before set.
fn join_width(items: &mut Vec<Item>, axes: Axes, line: Line) {
    let joined = match items.last_mut() {
        Some(Item::Instruction {
            mnemonic,
            operand,
            line: set,
            ..
        }) if (mnemonic == "sep" || mnemonic == "rep") && set.labels.is_empty() => {
            match immediate(operand) {
                Some(mask) if covers(mnemonic, mask, axes) => Some(Item::WidthSet {
                    axes,
                    mnemonic: mnemonic.clone(),
                    mask,
                    more: vec![line.clone()],
                    line: set.clone(),
                }),
                _ => None,
            }
        }
        Some(Item::WidthSet {
            axes: set,
            mnemonic,
            mask,
            more,
            ..
        }) if !more.is_empty()
            && covers(mnemonic, *mask, axes)
            && (set.a.is_none() || axes.a.is_none())
            && (set.i.is_none() || axes.i.is_none()) =>
        {
            set.a = set.a.or(axes.a);
            set.i = set.i.or(axes.i);
            more.push(line);
            return;
        }
        _ => None,
    };
    match joined {
        Some(set) => {
            items.pop();
            items.push(set);
        }
        None => items.push(Item::WidthAssert { axes, line }),
    }
}

/// Whether a `sep`/`rep` of `mask` sets every axis `axes` names, to the
/// width it names.
fn covers(mnemonic: &str, mask: u32, axes: Axes) -> bool {
    let set = mask_axes(mnemonic, mask);
    axes.a.is_none_or(|a| set.a == Some(a)) && axes.i.is_none_or(|i| set.i == Some(i))
}

/// The names a linkage directive lists, without `: zp`-style qualifiers.
fn linkage_names(args: &[Token]) -> Vec<String> {
    source::split_commas(args)
        .into_iter()
        .filter_map(|part| match part.first() {
            Some(Token {
                tok: Tok::Ident(name),
                ..
            }) => Some(name.clone()),
            _ => None,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lines(text: &str) -> Vec<Line> {
        text.lines()
            .enumerate()
            .map(|(index, line)| source::read(index + 1, line, false).expect("reads"))
            .collect()
    }

    fn build(text: &str) -> Vec<Item> {
        let lines = lines(text);
        let mut builder = Builder {
            widths: Widths::default(),
            macros: &|_| None,
        };
        builder.items(&lines, &mut 0, &[]).0
    }

    #[test]
    fn a_rep_and_its_directives_are_one_width_set() {
        let items = build("        rep #%00110000\n        .a16\n        .i16\n        lda #1\n");
        assert!(matches!(
            &items[0],
            Item::WidthSet { axes, mask: 0x30, .. }
                if *axes == Axes { a: Some(Width::W16), i: Some(Width::W16) }
        ));
        let Item::Instruction { widths, .. } = &items[1] else {
            panic!("an instruction: {items:?}");
        };
        assert_eq!((widths.a, widths.i), (Width::W16, Width::W16));
    }

    #[test]
    fn a_rep_that_also_clears_carry_keeps_its_mask() {
        let items = build("        rep #%00110001\n        .a16\n        .i16\n");
        assert!(matches!(
            &items[0],
            Item::WidthSet { mnemonic, mask: 0x31, more, .. } if mnemonic == "rep" && more.len() == 2
        ));
    }

    #[test]
    fn a_lone_directive_is_an_assertion() {
        let items = build("        rts\n        .a8\nnext:   inx\n");
        assert!(matches!(&items[1], Item::WidthAssert { .. }));
        let Item::Instruction { widths, .. } = &items[3] else {
            panic!("an instruction: {items:?}");
        };
        assert_eq!(widths.a, Width::W8);
        assert!(widths.a_stated && !widths.i_stated);
    }

    #[test]
    fn a_width_macro_is_recognized_by_its_body() {
        let all = lines(".macro _a8\n        sep #%00100000\n        .a8\n.endmacro\n");
        let mut builder = Builder {
            widths: Widths::default(),
            macros: &|_| None,
        };
        let items = builder.items(&all, &mut 0, &[]).0;
        let Item::Macro(definition) = &items[0] else {
            panic!("a macro: {items:?}");
        };
        assert_eq!(
            width_macro(definition),
            Some((
                "sep".to_owned(),
                0x20,
                Axes {
                    a: Some(Width::W8),
                    i: None
                }
            ))
        );
    }

    #[test]
    fn structs_enums_and_repeats_nest() {
        let items = build(
            ".struct R\n    .org $FF00\n    a .byte\n        .byte 2\n    w .word\n.endstruct\n\
             .enum\n    ONE = 1\n    TWO ; two\n.endenum\n\
             .repeat 3, I\n        nop\n.endrep\n",
        );
        let Item::Struct(layout) = &items[0] else {
            panic!("a struct: {items:?}");
        };
        assert_eq!(layout.members.len(), 4);
        assert!(matches!(layout.members[0].kind, MemberKind::Org(_)));
        assert!(layout.members[2].name.is_none());
        let Item::Enum(enumeration) = &items[1] else {
            panic!("an enum: {items:?}");
        };
        assert_eq!(enumeration.members.len(), 2);
        assert!(
            matches!(&items[2], Item::Repeat { var: Some(var), items, .. } if var == "I" && items.len() == 1)
        );
    }
}
