use std::cmp::Ordering;
use std::fmt;

#[derive(Clone, Debug)]
pub struct Token {
  value: String,
  token_type: TokenType,
  column: usize,
}

impl Token {
  pub fn new(value: &str, typ: &TokenType, column: usize) -> Token {
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

  pub fn _is_unary_op(&self) -> bool {
    match self.get_type() {
      TokenType::Negation | TokenType::BitwiseComplement | TokenType::LogicalNegation => true,
      _ => false,
    }
  }

  pub fn is_add_or_sub(&self) -> bool {
    match self.get_type() {
      TokenType::Addition | TokenType::Negation => true,
      _ => false,
    }
  }

  pub fn is_mul_or_div(&self) -> bool {
    match self.get_type() {
      TokenType::Multiplication | TokenType::Division | TokenType::Modulo => true,
      _ => false,
    }
  }

  pub fn is_logical_or(&self) -> bool {
    match self.get_type() {
      TokenType::Or => true,
      _ => false,
    }
  }

  pub fn is_logical_and(&self) -> bool {
    match self.get_type() {
      TokenType::And => true,
      _ => false,
    }
  }

  pub fn is_equality(&self) -> bool {
    match self.get_type() {
      TokenType::Equal | TokenType::NotEqual => true,
      _ => false,
    }
  }

  pub fn is_relational(&self) -> bool {
    match self.get_type() {
      TokenType::LessThan
      | TokenType::LessThanOrEqual
      | TokenType::GreaterThan
      | TokenType::GreaterThanOrEqual => true,
      _ => false,
    }
  }

  pub fn is_bitwise_or(&self) -> bool {
    match self.get_type() {
      TokenType::BitwiseOr => true,
      _ => false,
    }
  }

  pub fn is_bitwise_xor(&self) -> bool {
    match self.get_type() {
      TokenType::BitwiseXor => true,
      _ => false,
    }
  }

  pub fn is_bitwise_and(&self) -> bool {
    match self.get_type() {
      TokenType::BitwiseAnd => true,
      _ => false,
    }
  }

  pub fn is_shift(&self) -> bool {
    match self.get_type() {
      TokenType::BitwiseShl | TokenType::BitwiseShr => true,
      _ => false,
    }
  }

  pub fn is_assignment(&self) -> bool {
    match self.get_type() {
      TokenType::Assignment => true,
      _ => false,
    }
  }

  pub fn is_combo_assignment(&self) -> bool {
    match self.get_type() {
      TokenType::AddAssign
      | TokenType::MulAssign
      | TokenType::DivAssign
      | TokenType::SubAssign
      | TokenType::ModAssign
      | TokenType::ShlAssign
      | TokenType::ShrAssign
      | TokenType::AndAssign
      | TokenType::OrAssign
      | TokenType::XorAssign
      | TokenType::PreIncrement
      | TokenType::PostIncrement
      | TokenType::PreDecrement
      | TokenType::PostDecrement => true,
      _ => false,
    }
  }

  pub fn get_combo_assignment_op(&self) -> TokenType {
    match self.get_type() {
      TokenType::AddAssign => TokenType::Addition,
      TokenType::MulAssign => TokenType::Multiplication,
      TokenType::SubAssign => TokenType::Negation,
      TokenType::DivAssign => TokenType::Division,
      TokenType::ModAssign => TokenType::Modulo,
      TokenType::ShlAssign => TokenType::BitwiseShl,
      TokenType::ShrAssign => TokenType::BitwiseShr,
      TokenType::AndAssign => TokenType::BitwiseAnd,
      TokenType::OrAssign => TokenType::BitwiseOr,
      TokenType::XorAssign => TokenType::BitwiseXor,
      TokenType::PreIncrement => TokenType::AddAssign,
      TokenType::PostIncrement => TokenType::Addition,
      TokenType::PreDecrement => TokenType::SubAssign,
      TokenType::PostDecrement => TokenType::Negation,
      _ => panic!(
        "Expected combo assignment operator. Got {}",
        self.get_type()
      ),
    }
  }

  pub fn is_postfix(&self) -> bool {
    match self.get_type() {
      TokenType::PostIncrement | TokenType::PostDecrement => true,
      _ => false,
    }
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
  Modulo,
  BitwiseAnd,
  BitwiseOr,
  BitwiseXor,
  BitwiseShl,
  BitwiseShr,
  Assignment,
  AddAssign,
  MulAssign,
  SubAssign,
  DivAssign,
  ModAssign,
  ShlAssign,
  ShrAssign,
  AndAssign,
  OrAssign,
  XorAssign,
  PreIncrement,
  PreDecrement,
  PostIncrement,
  PostDecrement,
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
      TokenType::Modulo => "%",
      TokenType::BitwiseAnd => "&",
      TokenType::BitwiseOr => "|",
      TokenType::BitwiseXor => "^",
      TokenType::BitwiseShl => "<<",
      TokenType::BitwiseShr => ">>",
      TokenType::Assignment => "=",
      TokenType::AddAssign => "+=",
      TokenType::MulAssign => "*=",
      TokenType::SubAssign => "-=",
      TokenType::DivAssign => "/=",
      TokenType::ModAssign => "%=",
      TokenType::ShlAssign => "<<=",
      TokenType::ShrAssign => ">>=",
      TokenType::AndAssign => "&=",
      TokenType::OrAssign => "|=",
      TokenType::XorAssign => "^=",
      TokenType::PreIncrement => "++",
      TokenType::PreDecrement => "--",
      TokenType::PostIncrement => "++",
      TokenType::PostDecrement => "--",
    })
  }
}

impl fmt::Display for TokenType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.to_string())
  }
}
