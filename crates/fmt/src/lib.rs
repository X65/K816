pub mod print_ast;
pub mod print_ir;

use k816_core::ast;

pub fn format_file(ast: &ast::File, source: &str) -> String {
    print_ast::format_ast(ast, source)
}

pub fn format_source(source: &str) -> String {
    print_ast::format_source(source)
}
