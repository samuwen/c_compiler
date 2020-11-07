use crate::Prog;

pub fn generate(ast: Prog) -> String {
  let function = ast.get_function();
  let asm_string = function.get_function_asm();
  asm_string
}
