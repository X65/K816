use super::*;

pub fn write_object(path: &std::path::Path, object: &O65Object) -> Result<()> {
    let bytes = encode_object(object)?;
    std::fs::write(path, bytes).with_context(|| format!("failed to write '{}'", path.display()))
}

pub fn read_object(path: &std::path::Path) -> Result<O65Object> {
    let bytes =
        std::fs::read(path).with_context(|| format!("failed to read '{}'", path.display()))?;
    decode_object(&bytes).with_context(|| format!("failed to decode '{}'", path.display()))
}
