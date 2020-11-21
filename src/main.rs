mod generator;
mod lexer;
mod parser;
mod token;

use flexi_logger::*;
use generator::{generate, Node, NodeType};
use lexer::lex;
use log::*;
use parser::{parse, Tree};
use std::env;
use std::fs::{read_to_string, write};
use std::process::Command;
use std::time::{Duration, Instant};
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
  let lex_start = Instant::now();
  let tokens = lex(file);
  let lex_end = Instant::now();
  let lex_dur = lex_end.duration_since(lex_start);
  let parse_start = Instant::now();
  let parsed = parse(tokens);
  let parse_end = Instant::now();
  let parse_dur = parse_end.duration_since(parse_start);
  let generate_start = Instant::now();
  let generated = generate(parsed);
  let generate_end = Instant::now();
  let generate_dur = generate_end.duration_since(generate_start);
  debug!("{}", generated);
  write(ASSEMBLY_FILE_NAME, &generated).expect("Failed to write output file");
  let output = Command::new("gcc")
    .args(&["-m32", ASSEMBLY_FILE_NAME, "-o", "out"])
    .output()
    .expect("GCC failed to run");
  if output.status.success() {
    debug!("File successfully linked");
    let delete_result = Command::new("rm")
      .arg(ASSEMBLY_FILE_NAME)
      .output()
      .expect("Failed to delete file");
    trace!("delete: {}", delete_result.status);
  } else {
    panic!("GCC failed to compile");
  }
  let run_output = Command::new("./out")
    .output()
    .expect("Failed to run program");
  debug!("program run: {},", run_output.status);
  let print_durs = |dur: Duration, name| {
    warn!("{} took {} microseconds", name, dur.as_micros());
  };
  print_durs(lex_dur, "Lexing");
  print_durs(parse_dur, "Parsing");
  print_durs(generate_dur, "Generation");
}
