use crate::Prog;

pub fn generate(ast: Prog) -> String {
  let function = ast.get_function();
  let mut asm_vec = vec![];
  function.get_function_asm(&mut asm_vec);
  remove_extra_movs(&mut asm_vec);
  asm_vec.join("\n")
}

// hack, the && and || methods add an extra mov at the end of statements
fn remove_extra_movs(vec: &mut Vec<String>) {
  let last_label_opt_pos = vec.iter().rposition(|asm| asm.contains("label"));
  if let Some(pos) = last_label_opt_pos {
    let ret_opt_pos = vec
      .iter()
      .rposition(|asm| asm.contains("ret"))
      .expect("no ret statement");
    if ret_opt_pos - pos == 2 {
      vec.remove(vec.len() - 2);
    }
  }
}
