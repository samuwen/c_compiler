use crate::{Node, NodeType};
use crate::{Token, TokenType};
use log::*;
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
  let json = to_string(&function).unwrap();
  std::fs::write("out.json", &json).expect("Failed to write");
  debug!("{}", function);
  Prog::new(function)
}

// <function> ::= "int" <id> "(" ")" "{" { <statement> } "}"
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
  let mut next = tokens.get(0).expect("Unexpected statement truncation");
  while next.get_type() != &TokenType::CBrace {
    let statement = parse_statement(tokens);
    n.add_child(statement);
    next = tokens.get(0).expect("Unexpected statement truncation");
  }
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::CBrace {
    panic!("Function declaration not closed by brace");
  }
  n
}

// <statement> ::= <return-statement> | <expression-statement> | <assignment-statement>
fn parse_statement(tokens: &mut Vec<Token>) -> Node<String> {
  let token = tokens.get(0).unwrap();
  match token.get_type() {
    TokenType::ReturnKeyword => parse_return_statement(tokens),
    TokenType::Integer => parse_expression_statement(tokens),
    TokenType::IntKeyword => parse_assignment_statement(tokens),
    _ => panic!("Unknown statement found: {:?}", token.get_type()),
  }
}

// <return-statement> ::= "return" <exp> ";"
fn parse_return_statement(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::ReturnStatement);
  tokens.remove(0);
  let expression = parse_expression(tokens);
  n.add_child(expression);
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::Semicolon {
    panic!(
      "Last value I know in a return statement is a {:?}, not a semicolon",
      token.get_type()
    );
  }
  n
}

// <exp> ";"
fn parse_expression_statement(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::ExpressionStatement);
  let expression = parse_expression(tokens);
  n.add_child(expression);
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::Semicolon {
    panic!(
      "Last value I know in an expression statement is a {:?}, not a semicolon",
      token.get_type()
    );
  }
  n
}

// "int" <id> [= <exp>] ";"
fn parse_assignment_statement(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::AssignmentStatement);
  let token = tokens.remove(0);
  n.add_data(token.get_value());
  let id_token = tokens.remove(0);
  n.add_data(id_token.get_value());
  let token = tokens
    .get(0)
    .expect("assignment ID is last token in statement");
  match token.get_type() {
    TokenType::Assignment => {
      let expression = parse_assignment_expression(tokens);
      n.add_child(expression);
    }
    TokenType::Semicolon => {
      n.add_child(Node::new(NodeType::AssignmentExpression));
    }
    _ => panic!(
      "Expected Assignment or Semicolon. Found: {:?}",
      token.get_type()
    ),
  }
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::Semicolon {
    panic!(
      "Last value I know in an assignment statement is a {:?}, not a semicolon",
      token.get_type()
    );
  }
  n
}

// "int" <id> [= <exp>] ";"
fn parse_assignment_expression(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::AssignmentExpression);
  let token = tokens.remove(0);
  n.add_data(token.get_value());
  let exp = parse_expression(tokens);
  n.add_child(exp);
  n
}

// <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
fn parse_expression(tokens: &mut Vec<Token>) -> Node<String> {
  parse_exp(
    tokens,
    NodeType::OrExpression,
    vec![TokenType::Or],
    parse_logical_and_exp,
  )
}

// <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
fn parse_logical_and_exp(tokens: &mut Vec<Token>) -> Node<String> {
  parse_exp(
    tokens,
    NodeType::AndExpression,
    vec![TokenType::And],
    parse_bitwise_or_exp,
  )
}

fn parse_bitwise_or_exp(tokens: &mut Vec<Token>) -> Node<String> {
  parse_exp(
    tokens,
    NodeType::BitwiseOrExpression,
    vec![TokenType::BitwiseOr],
    parse_bitwise_xor_exp,
  )
}

fn parse_bitwise_xor_exp(tokens: &mut Vec<Token>) -> Node<String> {
  parse_exp(
    tokens,
    NodeType::BitwiseXorExpression,
    vec![TokenType::BitwiseXor],
    parse_bitwise_and_exp,
  )
}

fn parse_bitwise_and_exp(tokens: &mut Vec<Token>) -> Node<String> {
  parse_exp(
    tokens,
    NodeType::BitwiseAndExpression,
    vec![TokenType::BitwiseAnd],
    parse_equality_exp,
  )
}

// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
fn parse_equality_exp(tokens: &mut Vec<Token>) -> Node<String> {
  parse_exp(
    tokens,
    NodeType::EqualityExpression,
    vec![TokenType::Equal, TokenType::NotEqual],
    parse_relational_exp,
  )
}

// <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
fn parse_relational_exp(tokens: &mut Vec<Token>) -> Node<String> {
  parse_exp(
    tokens,
    NodeType::RelationalExpression,
    vec![
      TokenType::GreaterThan,
      TokenType::GreaterThanOrEqual,
      TokenType::LessThan,
      TokenType::LessThanOrEqual,
    ],
    parse_shift_exp,
  )
}

fn parse_shift_exp(tokens: &mut Vec<Token>) -> Node<String> {
  parse_exp(
    tokens,
    NodeType::ShiftExpression,
    vec![TokenType::BitwiseShl, TokenType::BitwiseShr],
    parse_additive_exp,
  )
}

// <additive-exp> ::= <term> { ("+" | "-") <term> }
fn parse_additive_exp(tokens: &mut Vec<Token>) -> Node<String> {
  parse_exp(
    tokens,
    NodeType::AdditiveExpression,
    vec![TokenType::Addition, TokenType::Negation],
    parse_term,
  )
}

// <term> ::= <factor> { ("*" | "/") <factor> }
fn parse_term(tokens: &mut Vec<Token>) -> Node<String> {
  parse_exp(
    tokens,
    NodeType::Term,
    vec![
      TokenType::Multiplication,
      TokenType::Division,
      TokenType::Modulo,
    ],
    parse_factor,
  )
}

fn parse_exp<F: Fn(&mut Vec<Token>) -> Node<String>>(
  tokens: &mut Vec<Token>,
  n_type: NodeType,
  t_types: Vec<TokenType>,
  f: F,
) -> Node<String> {
  let mut n = Node::new(n_type);
  let error_message = "Unexpected termination of tokens";
  let mut exp = f(tokens);
  let next = tokens.get(0).expect(error_message);
  let mut next_type = next.get_type().clone();
  while t_types.contains(&next_type) {
    let op = tokens.remove(0);
    let next_exp = f(tokens);
    let mut bin_op = Node::new(NodeType::BinaryOp);
    bin_op.add_data(op.get_value());
    bin_op.add_child(exp);
    bin_op.add_child(next_exp);
    exp = bin_op;
    next_type = tokens.get(0).expect(error_message).get_type().clone();
  }
  n.add_child(exp);
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
    _ => panic!("Unexpected child in factor: {:?}", token.get_type()),
  }
  n
}
