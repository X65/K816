pub mod doc;
pub mod print_ast;
pub mod print_ir;

use k816_core::ast;

pub fn format_file(ast: &ast::File) -> String {
    print_ast::format_ast(ast)
}
