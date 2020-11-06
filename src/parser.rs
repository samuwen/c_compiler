use crate::{Token, TokenType};
use log::*;
use std::fmt;
use std::fmt::{Display, Formatter};

pub fn parse(mut tokens: Vec<Token>) -> Program {
  let function = parse_function(&mut tokens);
  Program { function: function }
}

// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
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
  let id = token.get_value();
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
    name: id,
    statement: statement,
    data_type: data_type,
  }
}

// <statement> ::= "return" <exp> ";"
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

// <exp> ::= <term> { ("+" | "-") <term> }
fn parse_expression(tokens: &mut Vec<Token>) -> Exp {
  let error_message = "Unexpected termination of tokens";
  let mut term = parse_term(tokens);
  let next = tokens.get(0).expect(error_message);
  let mut next_type = next.get_type().clone();
  while next_type == TokenType::Addition || next_type == TokenType::Negation {
    let op = tokens.remove(0);
    let next_term = parse_term(tokens);
    term = Expression::BinOp(op.get_type().clone(), Box::new(term), Box::new(next_term));
    next_type = tokens.get(0).expect(error_message).get_type().clone();
  }
  term
}

// <term> ::= <factor> { ("*" | "/") <factor> }
fn parse_term(tokens: &mut Vec<Token>) -> Term {
  let error_message = "Unexpected termination of tokens";
  let mut factor = parse_factor(tokens);
  let next = tokens.get(0).expect(error_message);
  let mut next_type = next.get_type().clone();
  while next_type == TokenType::Multiplication || next_type == TokenType::Division {
    let op = tokens.remove(0);
    let next_factor = parse_factor(tokens);
    factor = Expression::BinOp(
      op.get_type().clone(),
      Box::new(factor),
      Box::new(next_factor),
    );
    next_type = tokens.get(0).expect(error_message).get_type().clone();
  }
  factor
}

// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
fn parse_factor(tokens: &mut Vec<Token>) -> Factor {
  let token = tokens.remove(0);
  match token.get_type() {
    TokenType::OParen => {
      let exp = parse_expression(tokens);
      let token = tokens.remove(0);
      if token.get_type() != &TokenType::CParen {
        panic!("Open paren does not have matching close paren");
      }
      Factor::Expression(Box::new(exp))
    }
    TokenType::Integer => Factor::Const(
      isize::from_str_radix(&token.get_value(), 10)
        .expect("Integer token does not have a proper base 10 value"),
    ),
    TokenType::BitwiseComplement | TokenType::Negation | TokenType::LogicalNegation => {
      Factor::UnaryOp(token.get_type().clone(), Box::new(parse_factor(tokens)))
    }
    _ => panic!("Unexpected expression type: {}", token.get_type()),
  }
}

pub struct Node<T> {
  _type: String,
  data: T,
  children: Vec<Node<T>>,
}

impl<T> Node<T> {
  fn new(node_type: String, data: T) -> Node<T> {
    Node {
      _type: node_type,
      data: data,
      children: vec![],
    }
  }

  fn add_child(&mut self, child: Node<T>) {
    self.children.push(child);
  }
}

#[derive(Debug)]
pub struct Program {
  function: Function,
}

impl Program {
  pub fn get_function(&self) -> &Function {
    &self.function
  }
}

impl Display for Program {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let data_type = self.get_function().get_data_type().to_ascii_uppercase();
    let f_name = format!(
      "FUN {} {}:",
      &data_type,
      self.get_function().get_name().to_ascii_uppercase()
    );
    let params = String::from("params: ()");
    let exp = self.get_function().get_statement().get_return_expression();
    debug!("{:?}", exp);
    let value = self
      .get_function()
      .get_statement()
      .get_return_expression()
      .get_value();
    let value = value
      .iter()
      .map(|t| t.get_value().to_owned())
      .collect::<Vec<_>>()
      .join(" ");
    write!(
      f,
      "{}\n\t{}\n\tBODY:\n\t\tRETURN {}<{}>",
      f_name, params, &data_type, value
    )
  }
}

#[derive(Debug)]
pub struct Function {
  name: String,
  statement: Statement,
  data_type: String,
}

impl Function {
  pub fn get_name(&self) -> &String {
    &self.name
  }

  pub fn get_statement(&self) -> &Statement {
    &self.statement
  }

  pub fn get_data_type(&self) -> &String {
    &self.data_type
  }
}

#[derive(Debug)]
pub struct Statement {
  ret: Expression,
}

impl Statement {
  pub fn get_return_expression(&self) -> &Expression {
    &self.ret
  }
}

// <exp> ::= <term> { ("+" | "-") <term> }
#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
  Const(isize),
  UnOp(TokenType, Box<Expression>),
  BinOp(TokenType, Box<Expression>, Box<Expression>),
}

// <exp> ::= <term> { ("+" | "-") <term> }
pub struct Exp {
  terms: Vec<Term>,
}

// <term> ::= <factor> { ("*" | "/") <factor> }
pub struct Term {
  factors: Vec<Factor>,
}

// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
pub enum Factor {
  Const(isize),
  Expression(Box<Exp>),
  UnaryOp(TokenType, Box<Factor>),
}

impl Expression {
  pub fn get_value(&self) -> Vec<Term2> {
    match self {
      Expression::Const(val) => vec![Term2::from_int(val)],
      Expression::UnOp(typ, unop) => {
        let mut terms = unop.get_value();
        terms.insert(0, Term2::from_token_type(typ));
        terms
      }
      Expression::BinOp(typ, op1, op2) => {
        let mut terms = op1.get_value();
        terms.push(Term2::from_token_type(typ));
        let mut terms2 = op2.get_value();
        match op2.get_type().as_ref() {
          "Const" => {
            // stub
          }
          "UnOp" => {
            // stub
          }
          "BinOp" => {
            // stub
          }
          _ => panic!("at the disco"),
        }
        terms.append(&mut terms2);
        terms
      }
    }
  }

  pub fn get_type(&self) -> String {
    String::from(match self {
      Expression::Const(_) => "Const",
      Expression::UnOp(_, _) => "UnOp",
      Expression::BinOp(_, _, _) => "BinOp",
    })
  }
}

#[derive(Debug)]
pub struct Term2 {
  term_type: String,
  value: String,
}

impl Term2 {
  fn from_int(int: &isize) -> Term2 {
    Term2 {
      term_type: String::from("Integer"),
      value: int.to_string(),
    }
  }

  fn from_token_type(token_type: &TokenType) -> Term2 {
    let text = String::from(match token_type {
      TokenType::Addition => "+",
      TokenType::Negation => "-",
      TokenType::Multiplication => "*",
      TokenType::Division => "/",
      TokenType::LogicalNegation => "!",
      TokenType::BitwiseComplement => "~",
      _ => panic!("Invalid token type: {:?}", token_type),
    });
    Term2 {
      term_type: String::from("Operator"),
      value: text,
    }
  }

  fn get_value(&self) -> &String {
    &self.value
  }
}

impl Display for Term2 {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}
