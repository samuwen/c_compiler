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
  std::fs::write("out_json.json", &json).expect("Failed to write");
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
  n.add_data(String::from("return"));
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
  let term = parse_term(tokens);
  n.add_child(term);
  let next = tokens.get(0).expect(error_message);
  let mut next_type = next.get_type().clone();
  while next_type == TokenType::Addition || next_type == TokenType::Negation {
    let op = tokens.remove(0);
    let mut op_node = Node::new("Operator");
    op_node.add_data(op.get_value());
    n.add_child(op_node);
    let next_term = parse_term(tokens);
    n.add_child(next_term);
    next_type = tokens.get(0).expect(error_message).get_type().clone();
  }
  n
}

// <term> ::= <factor> { ("*" | "/") <factor> }
fn parse_term(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new("Term");
  let error_message = "Unexpected termination of tokens";
  let factor = parse_factor(tokens);
  n.add_child(factor);
  let next = tokens.get(0).expect(error_message);
  let mut next_type = next.get_type().clone();
  while next_type == TokenType::Multiplication || next_type == TokenType::Division {
    let op = tokens.remove(0);
    let mut op_node = Node::new("Operator");
    op_node.add_data(op.get_value());
    n.add_child(op_node);
    let next_factor = parse_factor(tokens);
    n.add_child(next_factor);
    next_type = tokens.get(0).expect(error_message).get_type().clone();
  }
  n
}

// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
fn parse_factor(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new("Factor");
  let token = tokens.remove(0);
  match token.get_type() {
    TokenType::OParen => {
      let exp = parse_expression(tokens);
      n.add_child(exp);
      let token = tokens.remove(0);
      if token.get_type() != &TokenType::CParen {
        panic!("Open paren does not have matching close paren");
      }
    }
    TokenType::Integer => {
      let mut int_node = Node::new("Integer");
      int_node.add_data(token.get_value());
      n.add_child(int_node);
    }
    TokenType::BitwiseComplement | TokenType::Negation | TokenType::LogicalNegation => {
      let mut op_node = Node::new("Unary Op");
      op_node.add_data(token.get_type().to_string());
      n.add_child(op_node);
      let factor = parse_factor(tokens);
      n.add_child(factor);
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
    let mut add_str = |s| {
      out_string.push_str(" ");
      out_string.push_str(s);
    };
    if self.data.len() > 0 {
      for d in self.data.iter() {
        add_str(&d);
      }
    }
    let list = self.get_data_nodes(0, vec![]);
    debug!("{:?}", list);
    let mut int_iter = list.iter().filter(|t| t.get_type() != "Operator");
    let mut op_iter = list.iter().filter(|t| t.get_type() != "Integer");
    for i in 0..list.len() {
      let cd = match i % 2 == 0 {
        true => int_iter.next().unwrap(),
        false => op_iter.next().unwrap(),
      };
      cd.get_data().iter().for_each(|d| add_str(d));
    }
    out_string
  }

  fn get_data_nodes(&self, level: usize, mut list: Vec<ChildData>) -> Vec<ChildData> {
    for child in self.children.iter() {
      match child.child_count > 0 {
        true => {
          list = child.get_data_nodes(level + 1, list);
        }
        false => {
          let child_data = ChildData::new(level, child.get_data(), child.get_type());
          let first_ele_pos = list
            .iter()
            .position(|item| item.get_level() < level && item.get_type() == child_data.get_type());
          match first_ele_pos {
            Some(val) => {
              let ele = list.get(val).unwrap();
              match ele.get_level() < level {
                true => list.insert(val, child_data),
                false => list.push(child_data),
              }
            }
            None => list.push(child_data),
          }
        }
      }
    }
    list
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

#[derive(Clone, Debug, Eq, PartialEq)]
struct ChildData {
  data: Vec<String>,
  level_count: usize,
  _type: String,
}

impl ChildData {
  fn new(count: usize, data: &Vec<String>, _type: &String) -> ChildData {
    ChildData {
      level_count: count,
      data: data.to_owned(),
      _type: _type.to_owned(),
    }
  }

  fn get_data(&self) -> &Vec<String> {
    &self.data
  }

  fn get_level(&self) -> usize {
    self.level_count
  }

  fn get_type(&self) -> &String {
    &self._type
  }
}

impl Ord for ChildData {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.level_count.cmp(&other.level_count)
  }
}

impl PartialOrd for ChildData {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}
