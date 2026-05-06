use std::io::Write;
use std::sync::Once;

static INIT: Once = Once::new();

// Initialize the LSP server's logger. Idempotent so callers (including the
// integration test harness) can invoke it on every server start without
// panicking from a re-init. Output goes to stderr, which the VS Code client
// surfaces in the "k816 LSP" output channel.
//
// `RUST_LOG` is honored using env_logger's standard syntax
// (e.g. `RUST_LOG=k816_lsp=debug`); when unset we default to `info` for the
// LSP crate so the workspace progress messages remain visible.
pub(crate) fn init() {
    INIT.call_once(|| {
        let env = env_logger::Env::default().default_filter_or("k816_lsp=info");
        env_logger::Builder::from_env(env)
            .target(env_logger::Target::Stderr)
            .format(|buf, record| writeln!(buf, "k816-lsp [{}] {}", record.level(), record.args()))
            .init();
    });
}
