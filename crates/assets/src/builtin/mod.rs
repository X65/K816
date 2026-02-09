use crate::DataConverter;
use crate::binary::BinaryConverter;
use crate::charset::CharsetConverter;
use crate::image::ImageConverter;

pub fn binary_converter() -> Box<dyn DataConverter> {
    Box::new(BinaryConverter)
}

pub fn charset_converter() -> Box<dyn DataConverter> {
    Box::new(CharsetConverter)
}

pub fn image_converter() -> Box<dyn DataConverter> {
    Box::new(ImageConverter)
}
