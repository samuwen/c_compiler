mod generator;
mod lexer;
mod parser;
mod token;

use flexi_logger::*;
use generator::generate;
use lexer::lex;
use log::*;
use parser::{parse, Node, Prog};
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
  let generated = generate(parsed);
  debug!("{}", generated);
  write(ASSEMBLY_FILE_NAME, &generated).expect("Failed to write output file");
  let output = Command::new("gcc")
    .args(&["-m32", ASSEMBLY_FILE_NAME, "-o", "out"])
    .output()
    .expect("GCC failed to run");
  if output.status.success() {
    let delete_result = Command::new("rm")
      .arg(ASSEMBLY_FILE_NAME)
      .output()
      .expect("Failed to delete file");
    debug!("delete: {}", delete_result.status);
  }
}
