const VERSION: &str = "2.19";

fn main() {
    let mut args = std::env::args();
    let prog_name = args
        .next()
        .and_then(|arg| {
            std::path::Path::new(&arg)
                .file_name()
                .map(|name| name.to_string_lossy().into_owned())
        })
        .unwrap_or_else(|| String::from("k816"));

    if args.any(|arg| arg == "-V" || arg == "--version") {
        eprintln!(
            "{} V{} - {}",
            prog_name,
            VERSION,
            option_env!("BUILD_ID").unwrap_or("N/A")
        );
        return;
    }

    println!("Hello, world!");
}
