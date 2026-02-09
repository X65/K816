mod binary;
mod builtin;
mod charset;
mod image;
mod traits;

pub use crate::traits::{
    Arg, AssetFS, ConvertError, ConvertRequest, DataConverter, Span, StdAssetFS,
};

pub fn builtin_registry() -> Vec<Box<dyn DataConverter>> {
    vec![
        builtin::binary_converter(),
        builtin::charset_converter(),
        builtin::image_converter(),
    ]
}
