use crate::{Token, TokenType};
use log::*;
use serde::Serialize;
use serde_json::to_string;
use std::fmt;
use std::fmt::{Display, Formatter};

// <program> ::= <function>
#[derive(Debug)]
pub struct Prog {
  function: Node<String>,
}

impl Prog {
  fn new(fun: Node<String>) -> Prog {
    Prog { function: fun }
  }

  fn _get_function(&self) -> &Node<String> {
    &self.function
  }
}

impl Display for Prog {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.function)
  }
}

pub fn parse(mut tokens: Vec<Token>) -> Prog {
  let function = parse_function(&mut tokens);
  let json = to_string(&function._get_children()[0]._get_children()[0]).unwrap();
  std::fs::write("out.json", &json).expect("Failed to write");
  debug!("{}", function);
  Prog::new(function)
}

// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
fn parse_function(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new("Function");
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::IntKeyword {
    panic!("Int not at start of function declaration");
  }
  let data_type = token.get_value();
  n.add_data(data_type);
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::Identifier {
    panic!("Function declaration does not have a name");
  }
  let id = token.get_value();
  n.add_data(id);
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::OParen {
    panic!("Function declaration does not have parens");
  }
  n.add_data(String::from("("));
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::CParen {
    panic!("Function declaration does not have parens");
  }
  n.add_data(String::from(")"));
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::OBrace {
    panic!("Function declaration not followed by brace");
  }
  let statement = parse_statement(tokens);
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::CBrace {
    panic!("Function declaration not closed by brace");
  }
  n.add_child(statement);
  n
}

// <statement> ::= "return" <exp> ";"
fn parse_statement(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new("Statement");
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::ReturnKeyword {
    panic!("Statement does not have return keyword");
  }
  n.add_data(String::from("return "));
  let expression = parse_expression(tokens);
  n.add_child(expression);
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::Semicolon {
    panic!("Statement is not closed by a semicolon");
  }
  n.add_data(String::from(";"));
  n
}

// <exp> ::= <term> { ("+" | "-") <term> }
fn parse_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new("Expression");
  let error_message = "Unexpected termination of tokens";
  let mut term = parse_term(tokens);
  let next = tokens.get(0).expect(error_message);
  let mut next_type = next.get_type().clone();
  while next_type == TokenType::Addition || next_type == TokenType::Negation {
    let op = tokens.remove(0);
    let next_term = parse_term(tokens);
    let mut bin_op = Node::new("Binary Op");
    bin_op.add_data(op.get_value());
    bin_op.add_child(term);
    bin_op.add_child(next_term);
    term = bin_op;
    next_type = tokens.get(0).expect(error_message).get_type().clone();
  }
  n.add_child(term);
  n
}

// <term> ::= <factor> { ("*" | "/") <factor> }
fn parse_term(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new("Term");
  let error_message = "Unexpected termination of tokens";
  let mut factor = parse_factor(tokens);
  let next = tokens.get(0).expect(error_message);
  let mut next_type = next.get_type().clone();
  while next_type == TokenType::Multiplication || next_type == TokenType::Division {
    let op = tokens.remove(0);
    let next_factor = parse_factor(tokens);
    let mut bin_op = Node::new("Binary Op");
    bin_op.add_data(op.get_value());
    bin_op.add_child(factor);
    bin_op.add_child(next_factor);
    factor = bin_op;
    next_type = tokens.get(0).expect(error_message).get_type().clone();
  }
  n.add_child(factor);
  n
}

// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
fn parse_factor(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new("Factor");
  let token = tokens.remove(0);
  match token.get_type() {
    TokenType::OParen => {
      let mut exp = parse_expression(tokens);
      exp.add_data(token.get_type().to_string());
      let token = tokens.remove(0);
      if token.get_type() != &TokenType::CParen {
        panic!("Open paren does not have matching close paren");
      }
      exp.add_data(token.get_type().to_string());
      n.add_child(exp);
    }
    TokenType::Integer => {
      let mut int_node = Node::new("Integer");
      int_node.add_data(token.get_value());
      n.add_child(int_node);
    }
    TokenType::BitwiseComplement | TokenType::Negation | TokenType::LogicalNegation => {
      let mut op_node = Node::new("Unary Op");
      op_node.add_data(token.get_type().to_string());
      let factor = parse_factor(tokens);
      op_node.add_child(factor);
      n.add_child(op_node);
    }
    _ => panic!("the disco"),
  }
  n
}

#[derive(Debug, Serialize)]
pub struct Node<T> {
  _type: String,
  child_count: usize,
  data: Vec<T>,
  children: Vec<Node<T>>,
}

impl<T> Node<T> {
  fn new(node_type: &str) -> Node<T> {
    Node {
      _type: String::from(node_type),
      data: vec![],
      children: vec![],
      child_count: 0,
    }
  }

  fn add_child(&mut self, child: Node<T>) {
    self.child_count += 1;
    self.children.push(child);
  }

  fn add_data(&mut self, data: T) {
    self.data.push(data);
  }

  fn get_data(&self) -> &Vec<T> {
    &self.data
  }

  fn _get_child_count(&self) -> usize {
    self.child_count
  }

  fn _get_children(&self) -> &Vec<Node<T>> {
    &self.children
  }

  fn get_type(&self) -> &String {
    &self._type
  }
}

impl Node<String> {
  fn add_function_log(&self) -> String {
    format!(
      "FUN {} {}:\n\tparams: {}{}\n\tbody:\n",
      self.data[0].to_ascii_uppercase(),
      self.data[1],
      self.data[2],
      self.data[3]
    )
  }

  fn add_statement_log(&self) -> String {
    let mut out_string = self.data[0].to_ascii_uppercase().to_owned();
    for child in self.children.iter() {
      out_string.push_str(&child.add_expression_log());
    }
    format!("\t\t{} {}\n", out_string, self.data[1])
  }

  fn add_expression_log(&self) -> String {
    let mut out_string = String::new();
    let mut result = vec![];
    let mut has_parens = false;
    if self.data.len() > 0 {
      has_parens = true;
    }
    for child in self.children.iter() {
      let f = match child.get_type().as_str() {
        "Binary Op" => Node::add_binary_op_log,
        "Unary Op" => Node::add_unary_op_log,
        "Term" => Node::add_term_log,
        _ => panic!("Invalid child type in expression: {}", child.get_type()),
      };
      result.push(f(child));
    }
    if has_parens {
      result.insert(0, self.data.get(0).unwrap().to_owned());
      result.push(self.data.last().unwrap().to_owned());
    }
    out_string.push_str(&result.join(""));
    out_string
  }

  fn add_term_log(&self) -> String {
    let mut out_string = String::new();
    for child in self.children.iter() {
      let f = match child.get_type().as_str() {
        "Binary Op" => Node::add_binary_op_log,
        "Factor" => Node::add_factor_log,
        _ => panic!("Invalid child type in Term: {}", child.get_type()),
      };
      out_string.push_str(&f(child));
    }
    out_string
  }

  fn add_binary_op_log(&self) -> String {
    let mut out_string = String::new();
    let mut result: Vec<String> = vec![];
    for child in self.children.iter() {
      let f = match child.get_type().as_str() {
        "Term" => Node::add_term_log,
        "Factor" => Node::add_factor_log,
        "Binary Op" => Node::add_binary_op_log,
        _ => panic!("Invalid child type in binary op: {}", child.get_type()),
      };
      result.push(f(child));
    }
    result.insert(1, self.data.get(0).expect("Binary op has no op").to_owned());
    out_string.push_str(&result.join(" "));
    out_string
  }

  fn add_factor_log(&self) -> String {
    let mut out_string = String::new();
    for child in self.children.iter() {
      let f = match child.get_type().as_str() {
        "Expression" => Node::add_expression_log,
        "Unary Op" => Node::add_unary_op_log,
        "Integer" => Node::add_integer_log,
        _ => panic!("Invalid child type in factor: {}", child.get_type()),
      };
      out_string.push_str(&f(child));
    }
    out_string
  }

  fn add_unary_op_log(&self) -> String {
    let mut out_string = String::new();
    let mut result: Vec<String> = vec![
      String::from(" "),
      self.data.get(0).expect("Unary op has no op").to_owned(),
    ];
    for child in self.children.iter() {
      result.push(child.add_factor_log());
    }
    out_string.push_str(&result.join(""));
    out_string
  }

  fn add_integer_log(&self) -> String {
    String::from(&self.data.join(" "))
  }
}

impl Display for Node<String> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let mut out_string = String::new();
    out_string.push_str(&self.add_function_log());
    for child in self.children.iter() {
      out_string.push_str(&child.add_statement_log());
    }
    write!(f, "\n{}", out_string)
  }
}
