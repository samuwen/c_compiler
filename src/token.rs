use std::cmp::Ordering;
use std::fmt;

#[derive(Debug)]
pub struct Token {
  value: String,
  token_type: TokenType,
  column: isize,
}

impl Token {
  pub fn new(value: &str, typ: &TokenType, column: isize) -> Token {
    Token {
      value: value.to_owned(),
      token_type: typ.clone(),
      column: column,
    }
  }

  pub fn get_type(&self) -> &TokenType {
    &self.token_type
  }

  pub fn get_value(&self) -> String {
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
pub enum TokenType {
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
  Addition,
  Multiplication,
  Division,
  And,
  Or,
  Equal,
  NotEqual,
  LessThan,
  GreaterThan,
  LessThanOrEqual,
  GreaterThanOrEqual,
}

impl TokenType {
  fn to_string(&self) -> String {
    String::from(match self {
      TokenType::OBrace => "{",
      TokenType::CBrace => "}",
      TokenType::OParen => "(",
      TokenType::CParen => ")",
      TokenType::Semicolon => ";",
      TokenType::IntKeyword => "int",
      TokenType::ReturnKeyword => "return",
      TokenType::Identifier => "Identifier",
      TokenType::Integer => "Integer",
      TokenType::Negation => "-",
      TokenType::BitwiseComplement => "~",
      TokenType::LogicalNegation => "!",
      TokenType::Addition => "+",
      TokenType::Multiplication => "*",
      TokenType::Division => "/",
      TokenType::And => "&&",
      TokenType::Or => "||",
      TokenType::Equal => "==",
      TokenType::NotEqual => "!=",
      TokenType::LessThan => "<",
      TokenType::LessThanOrEqual => "<=",
      TokenType::GreaterThan => ">",
      TokenType::GreaterThanOrEqual => ">=",
    })
  }
}

impl fmt::Display for TokenType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.to_string())
  }
}
