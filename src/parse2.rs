use crate::{Node, NodeType, Token, TokenType};
use log::*;
use std::fmt;

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
  n.add_child(parse_logical_or_expression(tokens));
  let token = get_next_token(tokens);
  check_type(&TokenType::Semicolon, &token);
  n
}

// <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
fn parse_logical_or_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut and_exp = parse_logical_and_expression(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_logical_or() {
    let op_token = get_next_token(tokens);
    let next_term = parse_logical_and_expression(tokens);
    let mut binary_op = Node::new(NodeType::BinaryOp);
    binary_op.add_data(op_token.get_value());
    binary_op.add_children(vec![and_exp, next_term]);
    and_exp = binary_op;
    next = peek_next_token(tokens);
  }
  and_exp
}

// <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
fn parse_logical_and_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut equality_exp = parse_equality_expression(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_logical_and() {
    let op_token = get_next_token(tokens);
    let next_term = parse_equality_expression(tokens);
    let mut binary_op = Node::new(NodeType::BinaryOp);
    binary_op.add_data(op_token.get_value());
    binary_op.add_children(vec![equality_exp, next_term]);
    equality_exp = binary_op;
    next = peek_next_token(tokens);
  }
  equality_exp
}

// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
fn parse_equality_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut term = parse_relational_expression(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_equality() {
    let op_token = get_next_token(tokens);
    let next_term = parse_relational_expression(tokens);
    let mut binary_op = Node::new(NodeType::BinaryOp);
    binary_op.add_data(op_token.get_value());
    binary_op.add_children(vec![term, next_term]);
    term = binary_op;
    next = peek_next_token(tokens);
  }
  term
}

// <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
fn parse_relational_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut term = parse_additive_expression(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_relational() {
    let op_token = get_next_token(tokens);
    let next_term = parse_additive_expression(tokens);
    let mut binary_op = Node::new(NodeType::BinaryOp);
    binary_op.add_data(op_token.get_value());
    binary_op.add_children(vec![term, next_term]);
    term = binary_op;
    next = peek_next_token(tokens);
  }
  term
}

// <additive-exp> ::= <term> { ("+" | "-") <term> }
fn parse_additive_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut term = parse_term(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_add_or_sub() {
    let op_token = get_next_token(tokens);
    let next_term = parse_term(tokens);
    let mut binary_op = Node::new(NodeType::BinaryOp);
    binary_op.add_data(op_token.get_value());
    binary_op.add_children(vec![term, next_term]);
    term = binary_op;
    next = peek_next_token(tokens);
  }
  term
}

// <term> ::= <factor> { ("*" | "/") <factor> }
fn parse_term(tokens: &mut Vec<Token>) -> Node<String> {
  let mut factor = parse_factor(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_mul_or_div() {
    let op_token = get_next_token(tokens);
    let next_factor = parse_factor(tokens);
    let mut binary_op = Node::new(NodeType::BinaryOp);
    binary_op.add_data(op_token.get_value());
    binary_op.add_children(vec![factor, next_factor]);
    factor = binary_op;
    next = peek_next_token(tokens);
  }
  factor
}

// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
fn parse_factor(tokens: &mut Vec<Token>) -> Node<String> {
  let next = peek_next_token(tokens);
  let expression = match next.get_type() {
    TokenType::OParen => {
      get_next_token(tokens);
      let expression = parse_logical_or_expression(tokens);
      let token = get_next_token(tokens);
      check_type(&TokenType::CParen, &token);
      expression
    }
    TokenType::Integer => parse_integer(tokens),
    TokenType::BitwiseComplement | TokenType::LogicalNegation | TokenType::Negation => {
      parse_unary_op(tokens)
    }
    _ => panic!("Unexpected token: {:?}", next.get_type()),
  };
  expression
}

// <unary_op> ::= "!" | "~" | "-"
fn parse_unary_op(tokens: &mut Vec<Token>) -> Node<String> {
  let mut node = Node::new(NodeType::UnaryOp);
  let operator_token = get_next_token(tokens);
  node.add_data(operator_token.get_value());
  let expression = parse_logical_or_expression(tokens);
  node.add_child(expression);
  node
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
    panic!(
      "Expected {:?} token but got {:?} token",
      expected,
      actual.get_type()
    );
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

  pub fn generate_asm(&mut self, out_vec: &mut Vec<String>) {
    for node in self.nodes.iter_mut() {
      node.generate_asm(out_vec);
    }
  }
}

impl fmt::Display for Tree {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "nodes: {}", self.nodes[0])
  }
}
