use std::path::Path;

use crate::{Arg, ConvertError, ConvertRequest, DataConverter};

#[derive(Debug, Default)]
pub struct ImageConverter;

impl DataConverter for ImageConverter {
    fn kind(&self) -> &'static str {
        "image"
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

        let bytes = req.fs.read(Path::new(path))?;
        let dyn_image =
            image::load_from_memory(&bytes).map_err(|source| ConvertError::ImageDecode {
                message: source.to_string(),
            })?;
        Ok(dyn_image.to_luma8().into_raw())
    }
}
