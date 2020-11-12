use crate::{Node, NodeType, Token, TokenType};
use log::*;
use std::fmt;

const UNEXPECTED_ERROR: &str = "Unexpected token type";

pub fn parse(mut tokens: Vec<Token>) -> Tree {
  let mut tree = Tree::new();
  tree.add_node(parse_program(&mut tokens));
  debug!("\n{}", tree);
  tree
}

fn parse_program(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::Program);
  n.add_child(parse_function(tokens));
  n
}

// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
fn parse_function(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::Function);
  let token = get_next_token(tokens);
  check_type(&TokenType::IntKeyword, &token);
  let data_type = token.get_value();
  n.add_data(data_type);
  let token = get_next_token(tokens);
  check_type(&TokenType::Identifier, &token);
  let id = token.get_value();
  n.add_data(id);
  let token = get_next_token(tokens);
  check_type(&TokenType::OParen, &token);
  let token = get_next_token(tokens);
  check_type(&TokenType::CParen, &token);
  let token = get_next_token(tokens);
  check_type(&TokenType::OBrace, &token);
  let mut next = peek_next_token(tokens);
  while next.get_type() != &TokenType::CBrace {
    let statement = parse_statement(tokens);
    n.add_child(statement);
    next = peek_next_token(tokens);
  }
  let token = get_next_token(tokens);
  check_type(&TokenType::CBrace, &token);
  n
}

// <statement> ::= "return" <exp> ";"
fn parse_statement(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::Statement);
  let token = get_next_token(tokens);
  check_type(&TokenType::ReturnKeyword, &token);
  n.add_child(parse_expression(tokens));
  let token = get_next_token(tokens);
  check_type(&TokenType::Semicolon, &token);
  n
}

// <exp> ::= <int>
fn parse_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::Expression);
  n.add_child(parse_integer(tokens));
  n
}

// <exp> ::= <int>
fn parse_integer(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::Integer);
  let token = get_next_token(tokens);
  check_type(&TokenType::Integer, &token);
  n.add_data(token.get_value());
  n
}

fn check_type(expected: &TokenType, actual: &Token) {
  if expected != actual.get_type() {
    panic!("Expected {:?} token but got {:?} token");
  }
}

fn get_next_token(tokens: &mut Vec<Token>) -> Token {
  tokens.remove(0)
}

fn peek_next_token(tokens: &mut Vec<Token>) -> &Token {
  tokens.get(0).expect("Unexpected end of input")
}

pub struct Tree {
  nodes: Vec<Node<String>>,
}

impl Tree {
  fn new() -> Tree {
    Tree { nodes: vec![] }
  }

  fn add_node(&mut self, node: Node<String>) {
    self.nodes.push(node);
  }

  pub fn generate_asm(&self, out_vec: &mut Vec<String>) {
    for node in self.nodes.iter() {
      node.generate_asm(out_vec);
    }
  }
}

impl fmt::Display for Tree {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "nodes: {}", self.nodes[0])
  }
}
