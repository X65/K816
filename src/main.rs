use std::path::PathBuf;

fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn run() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);
    let Some(input_path) = args.next() else {
        eprintln!("Usage: k816 <input.k816>");
        std::process::exit(2);
    };

    let input_path = PathBuf::from(input_path);
    let source = std::fs::read_to_string(&input_path)?;
    let output = k816_core::compile_source(&input_path.display().to_string(), &source)
        .map_err(|error| anyhow::anyhow!(error.rendered))?;

    let stem = input_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("out");
    let parent = input_path.parent().unwrap_or(std::path::Path::new("."));

    if output.banks.len() == 1 {
        let (_, bytes) = output
            .banks
            .first()
            .expect("a single bank should be present");
        let bin_path = parent.join(format!("{stem}.bin"));
        std::fs::write(bin_path, bytes)?;
    } else {
        for (bank, bytes) in &output.banks {
            let bin_path = parent.join(format!("{stem}.{bank}.bin"));
            std::fs::write(bin_path, bytes)?;
        }
    }

    let listing_path = parent.join(format!("{stem}.lst"));
    std::fs::write(listing_path, output.listing)?;
    Ok(())
}
