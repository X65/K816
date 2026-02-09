use std::path::Path;

use crate::{Arg, ConvertError, ConvertRequest, DataConverter};

#[derive(Debug, Default)]
pub struct BinaryConverter;

impl DataConverter for BinaryConverter {
    fn kind(&self) -> &'static str {
        "binary"
    }

    fn convert(&self, req: ConvertRequest<'_>) -> Result<Vec<u8>, ConvertError> {
        let path = match req.args.first() {
            Some(Arg::Str(path)) => path,
            Some(_) => {
                return Err(ConvertError::InvalidArg {
                    index: 0,
                    message: "expected string path".to_string(),
                });
            }
            None => return Err(ConvertError::MissingArg { index: 0 }),
        };

        req.fs.read(Path::new(path))
    }
}
