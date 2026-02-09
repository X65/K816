use crate::{Arg, ConvertError, ConvertRequest, DataConverter};

#[derive(Debug, Default)]
pub struct CharsetConverter;

impl DataConverter for CharsetConverter {
    fn kind(&self) -> &'static str {
        "charset"
    }

    fn convert(&self, req: ConvertRequest<'_>) -> Result<Vec<u8>, ConvertError> {
        let text = match req.args.first() {
            Some(Arg::Str(value)) => value,
            Some(_) => {
                return Err(ConvertError::InvalidArg {
                    index: 0,
                    message: "expected string text".to_string(),
                });
            }
            None => return Err(ConvertError::MissingArg { index: 0 }),
        };

        Ok(text.as_bytes().to_vec())
    }
}
