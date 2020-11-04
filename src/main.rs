use flexi_logger::*;
use log::*;
use regex::{Matches, Regex};
use std::cmp::Ordering;
use std::env;
use std::fmt;
use std::fs::{read_to_string, write};
use std::process::Command;

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
  debug!("\n{}", parsed);
  let generated = generate(parsed);
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

fn parse(tokens: Vec<Token>) -> Program {
  parse_program(tokens)
}

fn generate(ast: Program) -> String {
  let mut strings: Vec<String> = vec![];
  let fname = ast.get_function().get_name();
  strings.push(format!(".globl {}", fname));
  strings.push(format!("{}:", fname));
  let return_value = ast
    .get_function()
    .get_statement()
    .get_return_expression()
    .get_value();
  strings.push(format!("\tmovl \t${}, %eax", return_value.0));
  for operator in return_value.1 {
    match operator {
      TokenType::Negation => strings.push(format!("\tneg \t%eax")),
      TokenType::BitwiseComplement => strings.push(format!("\tnot \t%eax")),
      TokenType::LogicalNegation => {
        strings.push(format!("\tcmpl\t$0, %eax"));
        strings.push(format!("\tmovl\t$0, %eax"));
        strings.push(format!("\tsete\t%al"));
      }
      _ => panic!("Invalid unary operator coming back from expression"),
    }
  }
  strings.push(String::from("\tret"));
  strings.join("\n")
}

fn parse_program(mut tokens: Vec<Token>) -> Program {
  let function = parse_function(&mut tokens);
  Program { function: function }
}

fn parse_function(tokens: &mut Vec<Token>) -> Function {
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::IntKeyword {
    panic!("Int not at start of function declaration");
  }
  let data_type = token.get_value();
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::Identifier {
    panic!("Function declaration does not have a name");
  }
  let name = token.get_value();
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::OParen {
    panic!("Function declaration does not have parens");
  }
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::CParen {
    panic!("Function declaration does not have parens");
  }
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::OBrace {
    panic!("Function declaration not followed by brace");
  }
  let statement = parse_statement(tokens);
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::CBrace {
    panic!("Function declaration not closed by brace");
  }
  Function {
    name: name,
    statement: statement,
    data_type: data_type,
  }
}

fn parse_statement(tokens: &mut Vec<Token>) -> Statement {
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::ReturnKeyword {
    panic!("Statement does not have return keyword");
  }
  let expression = parse_expression(tokens);
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::Semicolon {
    panic!("Statement is not closed by a semicolon");
  }
  Statement { ret: expression }
}

fn parse_expression(tokens: &mut Vec<Token>) -> Expression {
  let token = tokens.remove(0);
  let expression = match token.get_type() {
    TokenType::Integer => Expression::Const(
      isize::from_str_radix(&token.get_value(), 10)
        .expect("Integer token does not have a proper base 10 value"),
    ),
    TokenType::BitwiseComplement | TokenType::Negation | TokenType::LogicalNegation => {
      Expression::UnOp(
        token.get_type().clone(),
        Some(Box::new(parse_expression(tokens))),
      )
    }
    _ => panic!("Unexpected expression type: {}", token.get_type()),
  };
  expression
}

fn lex(f: String) -> Vec<Token> {
  let mut total = Vec::with_capacity(f.len());
  total.append(&mut find_tokens(&f, "\\{", TokenType::OBrace));
  total.append(&mut find_tokens(&f, "\\}", TokenType::CBrace));
  total.append(&mut find_tokens(&f, "\\(", TokenType::OParen));
  total.append(&mut find_tokens(&f, "\\)", TokenType::CParen));
  total.append(&mut find_tokens(&f, ";", TokenType::Semicolon));
  total.append(&mut find_tokens(&f, "int", TokenType::IntKeyword));
  total.append(&mut find_tokens(&f, "return", TokenType::ReturnKeyword));
  total.append(&mut find_tokens(&f, "[a-zA-Z]\\w*", TokenType::Identifier));
  total.append(&mut find_tokens(&f, "[0-9]+", TokenType::Integer));
  total.append(&mut find_tokens(&f, "-", TokenType::Negation));
  total.append(&mut find_tokens(&f, "~", TokenType::BitwiseComplement));
  total.append(&mut find_tokens(&f, "!", TokenType::LogicalNegation));
  total.sort();
  total.dedup();
  debug!("{:?}", total);
  total
}

fn find_tokens(f: &String, value: &str, token_type: TokenType) -> Vec<Token> {
  let re = Regex::new(value).unwrap();
  gen_tokens(re.find_iter(&f), token_type)
}

fn gen_tokens(matches: Matches, token_type: TokenType) -> Vec<Token> {
  matches
    .map(|m| {
      Token::new(
        m.as_str().to_owned(),
        token_type.clone(),
        m.start() as isize,
      )
    })
    .collect()
}

#[derive(Debug)]
struct Token {
  value: String,
  token_type: TokenType,
  column: isize,
}

impl Token {
  fn new(value: String, typ: TokenType, column: isize) -> Token {
    Token {
      value: value,
      token_type: typ,
      column: column,
    }
  }

  fn get_type(&self) -> &TokenType {
    &self.token_type
  }

  fn get_value(&self) -> String {
    self.value.to_owned()
  }
}

impl PartialOrd for Token {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for Token {
  fn cmp(&self, other: &Self) -> Ordering {
    self.column.cmp(&other.column)
  }
}

impl Eq for Token {}

impl PartialEq for Token {
  fn eq(&self, other: &Self) -> bool {
    self.column == other.column && self.value == other.value
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum TokenType {
  OBrace,
  CBrace,
  OParen,
  CParen,
  Semicolon,
  IntKeyword,
  ReturnKeyword,
  Identifier,
  Integer,
  Negation,
  BitwiseComplement,
  LogicalNegation,
}

impl fmt::Display for TokenType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let t = match self {
      TokenType::OBrace => "OBrace",
      TokenType::CBrace => "CBrace",
      TokenType::OParen => "OParen",
      TokenType::CParen => "CParen",
      TokenType::Semicolon => "Semicolon",
      TokenType::IntKeyword => "IntKeyword",
      TokenType::ReturnKeyword => "ReturnKeyword",
      TokenType::Identifier => "Identifier",
      TokenType::Integer => "Integer",
      TokenType::Negation => "Negation",
      TokenType::BitwiseComplement => "BitwiseComplement",
      TokenType::LogicalNegation => "LogicalNegation",
    };
    write!(f, "{}", t)
  }
}

#[derive(Debug)]
struct Program {
  function: Function,
}

impl Program {
  fn get_function(&self) -> &Function {
    &self.function
  }
}

impl fmt::Display for Program {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let data_type = self.get_function().get_data_type().to_ascii_uppercase();
    let f_name = format!(
      "FUN {} {}:",
      &data_type,
      self.get_function().get_name().to_ascii_uppercase()
    );
    let params = String::from("params: ()");
    let value = self
      .get_function()
      .get_statement()
      .get_return_expression()
      .get_value();
    write!(
      f,
      "{}\n\t{}\n\tBODY:\n\t\tRETURN {}<{}>",
      f_name, params, &data_type, value.0
    )
  }
}

#[derive(Debug)]
struct Function {
  name: String,
  statement: Statement,
  data_type: String,
}

impl Function {
  fn get_name(&self) -> &String {
    &self.name
  }

  fn get_statement(&self) -> &Statement {
    &self.statement
  }

  fn get_data_type(&self) -> &String {
    &self.data_type
  }
}

#[derive(Debug)]
struct Statement {
  ret: Expression,
}

impl Statement {
  fn get_return_expression(&self) -> &Expression {
    &self.ret
  }
}

#[derive(Debug)]
enum Expression {
  Const(isize),
  UnOp(TokenType, Option<Box<Expression>>),
}

impl Expression {
  fn get_value(&self) -> (isize, Vec<TokenType>) {
    match self {
      Expression::Const(val) => (*val, vec![]),
      Expression::UnOp(typ, next) => {
        let value = match next {
          Some(unop) => {
            let (val, mut vec) = unop.get_value();
            vec.push(typ.clone());
            (val, vec)
          }
          None => panic!("UnOp with no child nodes"),
        };
        value
      }
    }
  }
}
