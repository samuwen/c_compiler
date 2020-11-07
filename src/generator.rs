use crate::Prog;

pub fn generate(ast: Prog) -> String {
  let function = ast.get_function();
  let mut asm_vec = vec![];
  function.get_function_asm(&mut asm_vec);
  asm_vec.join("\n")
}
