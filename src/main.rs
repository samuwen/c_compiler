mod lexer;
mod parse2;
mod token;

use flexi_logger::*;
use lexer::lex;
use log::*;
use parse2::{parse, Prog};
// use parser::{parse, Program};
use std::env;
use std::fs::{read_to_string, write};
use std::process::Command;
use token::{Token, TokenType};

const ASSEMBLY_FILE_NAME: &str = "src/out/assembly.s";

fn main() {
  Logger::with_env_or_str("debug")
    .duplicate_to_stdout(Duplicate::All)
    .format_for_stdout(colored_default_format)
    .start()
    .unwrap();
  let args: Vec<String> = env::args().collect();
  let file = read_to_string(&args[1]).expect("File not found");
  let tokens = lex(file);
  let parsed = parse(tokens);
  // let generated = generate(parsed);
  // write(ASSEMBLY_FILE_NAME, &generated).expect("Failed to write output file");
  // let output = Command::new("gcc")
  //   .args(&["-m32", ASSEMBLY_FILE_NAME, "-o", "out"])
  //   .output()
  //   .expect("GCC failed to run");
  // if output.status.success() {
  //   let delete_result = Command::new("rm")
  //     .arg(ASSEMBLY_FILE_NAME)
  //     .output()
  //     .expect("Failed to delete file");
  //   debug!("delete: {}", delete_result.status);
  // }
}

// fn generate(ast: Prog) -> String {
//   let mut strings: Vec<String> = vec![];
//   let fname = ast.get_function().get_name();
//   strings.push(format!(".globl {}", fname));
//   strings.push(format!("{}:", fname));
//   let _return_value = ast
//     .get_function()
//     .get_statement()
//     .get_return_expression()
//     .get_value();
//   // strings.push(format!("\tmovl \t${}, %eax", return_value[0]));
//   // for term in return_value {
//   //   match operator {
//   //     TokenType::Negation => strings.push(format!("\tneg \t%eax")),
//   //     TokenType::BitwiseComplement => strings.push(format!("\tnot \t%eax")),
//   //     TokenType::LogicalNegation => {
//   //       strings.push(format!("\tcmpl\t$0, %eax"));
//   //       strings.push(format!("\tmovl\t$0, %eax"));
//   //       strings.push(format!("\tsete\t%al"));
//   //     }
//   //     _ => panic!("Invalid unary operator coming back from expression"),
//   //   }
//   // }
//   strings.push(String::from("\tret"));
//   strings.join("\n")
// }
