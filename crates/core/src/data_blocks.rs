use std::collections::HashMap;

use k816_assets::{Arg, AssetFS, ConvertRequest};

use crate::ast::{DataArg, DataBlock, DataCommand};
use crate::diag::Diagnostic;
use crate::hir::Op;
use crate::span::Spanned;

pub fn lower_data_block(
    block: &DataBlock,
    fs: &dyn AssetFS,
) -> Result<Vec<Spanned<Op>>, Vec<Diagnostic>> {
    let converters = k816_assets::builtin_registry();
    let mut by_kind = HashMap::new();
    for converter in converters {
        by_kind.insert(converter.kind(), converter);
    }

    let mut diagnostics = Vec::new();
    let mut ops = Vec::new();

    for command in &block.commands {
        match &command.node {
            DataCommand::Align(value) => {
                ops.push(Spanned::new(Op::Align(*value), command.span));
            }
            DataCommand::Address(value) => {
                ops.push(Spanned::new(Op::Address(*value), command.span));
            }
            DataCommand::Nocross(value) => {
                ops.push(Spanned::new(Op::Nocross(*value), command.span));
            }
            DataCommand::Convert { kind, args } if kind == "bytes" => {
                match collect_byte_args(args, command.span) {
                    Ok(bytes) => ops.push(Spanned::new(Op::EmitBytes(bytes), command.span)),
                    Err(diag) => diagnostics.push(*diag),
                }
            }
            DataCommand::Convert { kind, args } => {
                let Some(converter) = by_kind.get(kind.as_str()) else {
                    diagnostics.push(
                        Diagnostic::error(command.span, format!("unknown data converter '{kind}'"))
                            .with_help("expected one of: binary, charset, image, bytes"),
                    );
                    continue;
                };

                let converted_args: Vec<Arg> = args.iter().map(convert_arg).collect();
                let span = k816_assets::Span {
                    source_id: command.span.source_id.0,
                    start: command.span.start,
                    end: command.span.end,
                };

                let request = ConvertRequest {
                    kind,
                    args: &converted_args,
                    span,
                    fs,
                };

                match converter.convert(request) {
                    Ok(bytes) => ops.push(Spanned::new(Op::EmitBytes(bytes), command.span)),
                    Err(err) => diagnostics.push(Diagnostic::error(
                        command.span,
                        format!("converter '{kind}' failed: {err}"),
                    )),
                }
            }
            DataCommand::Ignored => {}
        }
    }

    if diagnostics.is_empty() {
        Ok(ops)
    } else {
        Err(diagnostics)
    }
}

fn collect_byte_args(
    args: &[DataArg],
    span: crate::span::Span,
) -> Result<Vec<u8>, Box<Diagnostic>> {
    let mut out = Vec::with_capacity(args.len());
    for (idx, arg) in args.iter().enumerate() {
        match arg {
            DataArg::Int(value) => {
                let byte = u8::try_from(*value).map_err(|_| {
                    Box::new(Diagnostic::error(
                        span,
                        format!("bytes() argument at index {idx} must fit in u8"),
                    ))
                })?;
                out.push(byte);
            }
            DataArg::Str(_) => {
                return Err(Box::new(Diagnostic::error(
                    span,
                    format!("bytes() argument at index {idx} must be integer"),
                )));
            }
        }
    }
    Ok(out)
}

fn convert_arg(arg: &DataArg) -> Arg {
    match arg {
        DataArg::Int(value) => Arg::Int(*value),
        DataArg::Str(value) => Arg::Str(value.clone()),
    }
}
