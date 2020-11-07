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

  pub fn get_function(&self) -> &Node<String> {
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
  let json = to_string(&function.get_children()[0].get_children()[0]).unwrap();
  std::fs::write("out.json", &json).expect("Failed to write");
  debug!("{}", function);
  Prog::new(function)
}

// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
fn parse_function(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::Function);
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
  let mut n = Node::new(NodeType::Statement);
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
  let mut n = Node::new(NodeType::Expression);
  let error_message = "Unexpected termination of tokens";
  let mut term = parse_term(tokens);
  let next = tokens.get(0).expect(error_message);
  let mut next_type = next.get_type().clone();
  while next_type == TokenType::Addition || next_type == TokenType::Negation {
    let op = tokens.remove(0);
    let next_term = parse_term(tokens);
    let mut bin_op = Node::new(NodeType::BinaryOp);
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
  let mut n = Node::new(NodeType::Term);
  let error_message = "Unexpected termination of tokens";
  let mut factor = parse_factor(tokens);
  let next = tokens.get(0).expect(error_message);
  let mut next_type = next.get_type().clone();
  while next_type == TokenType::Multiplication || next_type == TokenType::Division {
    let op = tokens.remove(0);
    let next_factor = parse_factor(tokens);
    let mut bin_op = Node::new(NodeType::BinaryOp);
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
  let mut n = Node::new(NodeType::Factor);
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
      let mut int_node = Node::new(NodeType::Integer);
      int_node.add_data(token.get_value());
      n.add_child(int_node);
    }
    TokenType::BitwiseComplement | TokenType::Negation | TokenType::LogicalNegation => {
      let mut op_node = Node::new(NodeType::UnaryOp);
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
  _type: NodeType,
  child_count: usize,
  data: Vec<T>,
  children: Vec<Node<T>>,
}

impl<T> Node<T> {
  fn new(node_type: NodeType) -> Node<T> {
    Node {
      _type: node_type,
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

  pub fn get_children(&self) -> &Vec<Node<T>> {
    &self.children
  }

  pub fn get_type(&self) -> &NodeType {
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

  pub fn get_function_asm(&self) -> String {
    let mut out_string = String::new();
    let name = self.data.get(1).unwrap();
    out_string.push_str(&format!(".globl {}\n", name));
    out_string.push_str(&format!("{}:\n", name));
    for statement in self.children.iter() {
      out_string.push_str(&statement.get_statement_asm());
    }
    out_string
  }

  fn add_statement_log(&self) -> String {
    let mut out_string = self.data[0].to_ascii_uppercase().to_owned();
    for child in self.children.iter() {
      out_string.push_str(&child.add_expression_log());
    }
    format!("\t\t{} {}\n", out_string, self.data[1])
  }

  fn get_statement_asm(&self) -> String {
    let mut out_string = String::new();
    for child in self.children.iter() {
      out_string.push_str(&child.get_expression_asm());
    }
    out_string.push_str("\tret\n");
    out_string
  }

  fn add_expression_log(&self) -> String {
    let mut out_string = String::new();
    let mut result = vec![];
    let mut has_parens = false;
    if self.data.len() > 0 {
      has_parens = true;
    }
    for child in self.children.iter() {
      let f = match child.get_type() {
        NodeType::BinaryOp => Node::add_binary_op_log,
        NodeType::UnaryOp => Node::add_unary_op_log,
        NodeType::Term => Node::add_term_log,
        _ => panic!(
          "Invalid child type in expression: {}",
          child.get_type().as_str()
        ),
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

  fn get_expression_asm(&self) -> String {
    let mut out_string = String::new();
    for child in self.children.iter() {
      let f = match child.get_type() {
        NodeType::BinaryOp => Node::get_binary_op_asm,
        NodeType::UnaryOp => Node::get_unary_op_asm,
        NodeType::Term => Node::get_term_asm,
        _ => panic!(
          "Invalid child type in expression: {}",
          child.get_type().as_str()
        ),
      };
      out_string.push_str(&f(child));
    }
    out_string
  }

  fn add_term_log(&self) -> String {
    let mut out_string = String::new();
    for child in self.children.iter() {
      let f = match child.get_type() {
        NodeType::BinaryOp => Node::add_binary_op_log,
        NodeType::Factor => Node::add_factor_log,
        _ => panic!("Invalid child type in Term: {}", child.get_type().as_str()),
      };
      out_string.push_str(&f(child));
    }
    out_string
  }

  fn get_term_asm(&self) -> String {
    let mut out_string = String::new();
    for child in self.children.iter() {
      let f = match child.get_type() {
        NodeType::BinaryOp => Node::get_binary_op_asm,
        NodeType::Factor => Node::get_factor_asm,
        _ => panic!("Invalid child type in Term: {}", child.get_type().as_str()),
      };
      out_string.push_str(&f(child));
    }
    out_string
  }

  fn add_binary_op_log(&self) -> String {
    let mut out_string = String::new();
    let mut result: Vec<String> = vec![];
    for child in self.children.iter() {
      let f = match child.get_type() {
        NodeType::Term => Node::add_term_log,
        NodeType::Factor => Node::add_factor_log,
        NodeType::BinaryOp => Node::add_binary_op_log,
        _ => panic!(
          "Invalid child type in binary op: {}",
          child.get_type().as_str()
        ),
      };
      result.push(f(child));
    }
    result.insert(1, self.data.get(0).expect("Binary op has no op").to_owned());
    out_string.push_str(&result.join(" "));
    out_string
  }

  fn get_binary_op_asm(&self) -> String {
    let mut out_string = String::new();
    let mut result: Vec<String> = vec![];
    for child in self.children.iter() {
      let f = match child.get_type() {
        NodeType::Term => Node::get_term_asm,
        NodeType::Factor => Node::get_factor_asm,
        NodeType::BinaryOp => Node::get_binary_op_asm,
        _ => panic!(
          "Invalid child type in binary op: {}",
          child.get_type().as_str()
        ),
      };
      result.push(f(child));
    }
    match self.data.get(0).unwrap().as_str() {
      "+" => {
        out_string.push_str(&self.create_arith_string(result));
        out_string.push_str("\taddl\t%ecx, %eax\n");
      }
      "*" => {
        out_string.push_str(&self.create_arith_string(result));
        out_string.push_str("\timul\t%ecx, %eax\n");
      }
      "-" => {
        out_string.push_str(&self.create_arith_string(result));
        out_string.push_str("\tsubl\t%eax, %ecx\n");
        out_string.push_str("\tmovl\t%ecx, %eax\n");
      }
      "/" => {
        out_string.push_str(result.get(0).unwrap());
        let strang = result.get(1).unwrap();
        let strang = &strang.replace("eax", "ecx");
        out_string.push_str(&strang);
        out_string.push_str("\tcdq\n");
        out_string.push_str("\tidivl\t%ecx\n");
      }
      _ => panic!("hurp"),
    }
    out_string
  }

  fn create_arith_string(&self, result: Vec<String>) -> String {
    let mut out_string = String::new();
    out_string.push_str(result.get(0).unwrap());
    out_string.push_str("\tpush\t%eax\n");
    out_string.push_str(result.get(1).unwrap());
    out_string.push_str("\tpop\t%ecx\n");
    out_string
  }

  fn add_factor_log(&self) -> String {
    let mut out_string = String::new();
    for child in self.children.iter() {
      let f = match child.get_type() {
        NodeType::Expression => Node::add_expression_log,
        NodeType::UnaryOp => Node::add_unary_op_log,
        NodeType::Integer => Node::add_integer_log,
        _ => panic!(
          "Invalid child type in factor: {}",
          child.get_type().as_str()
        ),
      };
      out_string.push_str(&f(child));
    }
    out_string
  }

  fn get_factor_asm(&self) -> String {
    let mut out_string = String::new();
    for child in self.children.iter() {
      let f = match child.get_type() {
        NodeType::Expression => Node::get_expression_asm,
        NodeType::UnaryOp => Node::get_unary_op_asm,
        NodeType::Integer => Node::get_integer_asm,
        _ => panic!(
          "Invalid child type in factor: {}",
          child.get_type().as_str()
        ),
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

  fn get_unary_op_asm(&self) -> String {
    let mut out_string = String::new();
    for child in self.children.iter() {
      out_string.push_str(&child.get_factor_asm());
    }
    match self.data.get(0).expect("Unary op has no op").as_str() {
      "~" => out_string.push_str("\tnot\t%eax\n"),
      "-" => out_string.push_str("\tneg\t%eax\n"),
      "!" => {
        out_string.push_str("\tcmpl\t$0, %eax\n");
        out_string.push_str("\tmovl\t$0, %eax\n");
        out_string.push_str("\tsete\t%al\n");
      }
      _ => panic!("hard corps"),
    }
    out_string
  }

  fn add_integer_log(&self) -> String {
    String::from(&self.data.join(" "))
  }

  fn get_integer_asm(&self) -> String {
    format!("\tmovl\t${}, %eax\n", self.data.join(""))
  }
}

#[derive(Debug, Serialize)]
pub enum NodeType {
  Program,
  Function,
  Statement,
  Expression,
  Term,
  Factor,
  Integer,
  UnaryOp,
  BinaryOp,
}

impl NodeType {
  fn as_str(&self) -> &str {
    match self {
      NodeType::Program => "Program",
      NodeType::Function => "Function",
      NodeType::Statement => "Statement",
      NodeType::Expression => "Expression",
      NodeType::Term => "Term",
      NodeType::Factor => "Factor",
      NodeType::Integer => "Integer",
      NodeType::UnaryOp => "UnaryOp",
      NodeType::BinaryOp => "BinaryOp",
    }
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
