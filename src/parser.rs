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

// <function> ::= "int" <id> "(" ")" "{" { <statement> } "}"
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
    // <statement> ::= "return" <exp> ";" | <exp> ";" | "int" <id> [ = <exp>] ";"
    let statement = match next.get_type() {
      TokenType::ReturnKeyword => parse_return_statement(tokens),
      TokenType::IntKeyword => parse_declare_statement(tokens),
      _ => parse_expression_statement(tokens),
    };
    n.add_child(statement);
    next = peek_next_token(tokens);
  }
  let token = get_next_token(tokens);
  check_type(&TokenType::CBrace, &token);
  n
}

// <statement> ::= "return" <exp> ";"
fn parse_return_statement(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::ReturnStatement);
  let token = get_next_token(tokens);
  check_type(&TokenType::ReturnKeyword, &token);
  n.add_child(parse_logical_or_expression(tokens));
  let token = get_next_token(tokens);
  check_type(&TokenType::Semicolon, &token);
  n
}

// <statement> ::= "int" <id> [ = <exp>] ";"
fn parse_declare_statement(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::DeclareStatement);
  let token = get_next_token(tokens);
  check_type(&TokenType::IntKeyword, &token);
  let token = get_next_token(tokens);
  check_type(&TokenType::Identifier, &token);
  n.add_data(token.get_value());
  let token = get_next_token(tokens);
  match token.get_type() {
    TokenType::Semicolon => (),
    TokenType::Assignment => {
      n.add_child(parse_assignment_expression(tokens));
      let token = get_next_token(tokens);
      check_type(&TokenType::Semicolon, &token);
    }
    _ => panic!(
      "Expected Semicolon or Assignment, got {:?}",
      token.get_type()
    ),
  }
  n
}

// <statement> ::= <exp> ";"
fn parse_expression_statement(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::ExpressionStatement);
  n.add_child(parse_assignment_expression(tokens));
  let token = get_next_token(tokens);
  check_type(&TokenType::Semicolon, &token);
  n
}

// <exp> ::= <logical-or-exp> { "=" <logical-or-exp> }
fn parse_assignment_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut or_exp = parse_logical_or_expression(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_assignment() || next.is_combo_assignment() {
    let op_token = get_next_token(tokens);
    match op_token.is_postfix() {
      true => {
        let mut assignment = Node::new(NodeType::Assignment);
        assignment.add_data(or_exp.get_data().get(0).expect("No data found").to_owned());
        let mut unary_op = Node::new(NodeType::UnaryOp);
        unary_op.add_data(op_token.get_type().to_string());
        assignment.add_child(unary_op);
        or_exp = assignment;
        next = peek_next_token(tokens);
      }
      false => {
        let next_exp = parse_assignment_expression(tokens);
        let mut assignment = Node::new(NodeType::Assignment);
        assignment.add_data(or_exp.get_data().get(0).expect("No data found").to_owned());
        if op_token.is_combo_assignment() {
          let op = op_token.get_combo_assignment_op();
          let mut binary_op = Node::new(NodeType::BinaryOp);
          binary_op.add_data(op.to_string());
          binary_op.add_children(vec![or_exp, next_exp]);
          assignment.add_child(binary_op);
        } else {
          assignment.add_children(vec![or_exp, next_exp]);
        }
        or_exp = assignment;
        next = peek_next_token(tokens);
      }
    }
  }
  or_exp
}

// <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
fn parse_logical_or_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut and_exp = parse_logical_and_expression(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_logical_or() {
    let op_token = get_next_token(tokens);
    let next_and_exp = parse_logical_and_expression(tokens);
    let mut binary_op = Node::new(NodeType::BinaryOp);
    binary_op.add_data(op_token.get_value());
    binary_op.add_children(vec![and_exp, next_and_exp]);
    and_exp = binary_op;
    next = peek_next_token(tokens);
  }
  and_exp
}

// <logical-and-exp> ::= <bitwise-or-exp> { "&&" <bitwise-or-exp> }
fn parse_logical_and_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut bitwise_or = parse_bitwise_or_expression(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_logical_and() {
    let op_token = get_next_token(tokens);
    let next_term = parse_bitwise_or_expression(tokens);
    let mut binary_op = Node::new(NodeType::BinaryOp);
    binary_op.add_data(op_token.get_value());
    binary_op.add_children(vec![bitwise_or, next_term]);
    bitwise_or = binary_op;
    next = peek_next_token(tokens);
  }
  bitwise_or
}

// <bitwise-or-exp> ::= <bitwise-xor-exp> { "&&" <bitwise-xor-exp> }
fn parse_bitwise_or_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut bitwise_xor_exp = parse_bitwise_xor_expression(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_bitwise_or() {
    let op_token = get_next_token(tokens);
    let next_term = parse_bitwise_xor_expression(tokens);
    let mut binary_op = Node::new(NodeType::BinaryOp);
    binary_op.add_data(op_token.get_value());
    binary_op.add_children(vec![bitwise_xor_exp, next_term]);
    bitwise_xor_exp = binary_op;
    next = peek_next_token(tokens);
  }
  bitwise_xor_exp
}

// <bitwise-xor-exp> ::= <bitwise-and-exp> { "&&" <bitwise-and-exp> }
fn parse_bitwise_xor_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut bitwise_and_exp = parse_bitwise_and_expression(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_bitwise_xor() {
    let op_token = get_next_token(tokens);
    let next_term = parse_bitwise_and_expression(tokens);
    let mut binary_op = Node::new(NodeType::BinaryOp);
    binary_op.add_data(op_token.get_value());
    binary_op.add_children(vec![bitwise_and_exp, next_term]);
    bitwise_and_exp = binary_op;
    next = peek_next_token(tokens);
  }
  bitwise_and_exp
}

// <bitwise-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
fn parse_bitwise_and_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut equality_exp = parse_equality_expression(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_bitwise_and() {
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

// <relational-exp> ::= <shift-exp> { ("<" | ">" | "<=" | ">=") <shift-exp> }
fn parse_relational_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut term = parse_shift_expression(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_relational() {
    let op_token = get_next_token(tokens);
    let next_term = parse_shift_expression(tokens);
    let mut binary_op = Node::new(NodeType::BinaryOp);
    binary_op.add_data(op_token.get_value());
    binary_op.add_children(vec![term, next_term]);
    term = binary_op;
    next = peek_next_token(tokens);
  }
  term
}

// <shift-exp> ::= <additive-exp> { ("<<" | ">>") <additive-exp> }
fn parse_shift_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut additive_exp = parse_additive_expression(tokens);
  let mut next = peek_next_token(tokens);
  while next.is_shift() {
    let op_token = get_next_token(tokens);
    let next_term = parse_additive_expression(tokens);
    let mut binary_op = Node::new(NodeType::BinaryOp);
    binary_op.add_data(op_token.get_value());
    binary_op.add_children(vec![additive_exp, next_term]);
    additive_exp = binary_op;
    next = peek_next_token(tokens);
  }
  additive_exp
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

// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
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
    TokenType::BitwiseComplement
    | TokenType::LogicalNegation
    | TokenType::Negation
    | TokenType::PreDecrement
    | TokenType::PreIncrement => parse_unary_op(tokens),
    TokenType::Integer => parse_integer(tokens),
    TokenType::Identifier => parse_variable(tokens),
    _ => panic!("Unexpected token: {:?}", next.get_type()),
  };
  expression
}

// <unary_op> ::= "!" | "~" | "-" | "++" | "--"
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

fn parse_variable(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::Variable);
  let token = get_next_token(tokens);
  check_type(&TokenType::Identifier, &token);
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

fn peek_next_token(tokens: &mut Vec<Token>) -> Token {
  tokens.get(0).expect("Unexpected end of input").clone()
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
