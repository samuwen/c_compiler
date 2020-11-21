use crate::{Token, TokenType};
use log::*;
use onig::{FindMatches, Regex};

pub fn lex(f: String) -> Vec<Token> {
  let mut total = Vec::with_capacity(f.len());
  total.append(&mut get_tokens_unique_string(&f, TokenType::OBrace));
  total.append(&mut get_tokens_unique_string(&f, TokenType::CBrace));
  total.append(&mut get_tokens_unique_string(&f, TokenType::OParen));
  total.append(&mut get_tokens_unique_string(&f, TokenType::CParen));
  total.append(&mut get_tokens_unique_string(&f, TokenType::Semicolon));
  total.append(&mut get_tokens_unique_string(&f, TokenType::IntKeyword));
  total.append(&mut get_tokens_unique_string(&f, TokenType::ReturnKeyword));
  total.append(&mut get_tokens_unique_string(&f, TokenType::Identifier));
  total.append(&mut get_tokens_unique_string(&f, TokenType::Integer));
  total.append(&mut get_tokens_unique_string(&f, TokenType::Negation));
  total.append(&mut get_tokens_unique_string(
    &f,
    TokenType::BitwiseComplement,
  ));
  total.append(&mut get_tokens_unique_string(
    &f,
    TokenType::LogicalNegation,
  ));
  total.append(&mut get_tokens_default_string(&f, TokenType::Addition));
  total.append(&mut get_tokens_default_string(
    &f,
    TokenType::Multiplication,
  ));
  total.append(&mut get_tokens_default_string(&f, TokenType::Division));
  total.append(&mut get_tokens_default_string(&f, TokenType::And));
  total.append(&mut get_tokens_unique_string(&f, TokenType::Or));
  total.append(&mut get_tokens_default_string(&f, TokenType::Equal));
  total.append(&mut get_tokens_default_string(&f, TokenType::NotEqual));
  total.append(&mut get_tokens_default_string(&f, TokenType::LessThan));
  total.append(&mut get_tokens_default_string(
    &f,
    TokenType::LessThanOrEqual,
  ));
  total.append(&mut get_tokens_default_string(&f, TokenType::GreaterThan));
  total.append(&mut get_tokens_default_string(
    &f,
    TokenType::GreaterThanOrEqual,
  ));
  total.append(&mut get_tokens_default_string(&f, TokenType::Comma));
  total.append(&mut get_tokens_default_string(&f, TokenType::Modulo));
  total.append(&mut get_tokens_default_string(&f, TokenType::BitwiseAnd));
  total.append(&mut get_tokens_default_string(&f, TokenType::BitwiseOr));
  total.append(&mut get_tokens_default_string(&f, TokenType::BitwiseXor));
  total.append(&mut get_tokens_default_string(&f, TokenType::BitwiseShl));
  total.append(&mut get_tokens_default_string(&f, TokenType::BitwiseShr));
  total.append(&mut get_tokens_default_string(&f, TokenType::Assignment));
  total.append(&mut get_tokens_default_string(&f, TokenType::AddAssign));
  total.append(&mut get_tokens_default_string(&f, TokenType::SubAssign));
  total.append(&mut get_tokens_default_string(&f, TokenType::MulAssign));
  total.append(&mut get_tokens_default_string(&f, TokenType::DivAssign));
  total.append(&mut get_tokens_default_string(&f, TokenType::ModAssign));
  total.append(&mut get_tokens_default_string(&f, TokenType::ShlAssign));
  total.append(&mut get_tokens_default_string(&f, TokenType::ShrAssign));
  total.append(&mut get_tokens_default_string(&f, TokenType::AndAssign));
  total.append(&mut get_tokens_default_string(&f, TokenType::OrAssign));
  total.append(&mut get_tokens_default_string(&f, TokenType::XorAssign));
  total.append(&mut get_tokens_unique_string(&f, TokenType::PreIncrement));
  total.append(&mut get_tokens_unique_string(&f, TokenType::PostIncrement));
  total.append(&mut get_tokens_unique_string(&f, TokenType::PreDecrement));
  total.append(&mut get_tokens_unique_string(&f, TokenType::PostDecrement));
  total.append(&mut get_tokens_unique_string(&f, TokenType::IfKeyword));
  total.append(&mut get_tokens_unique_string(&f, TokenType::ElseKeyword));
  total.append(&mut get_tokens_unique_string(&f, TokenType::Colon));
  total.append(&mut get_tokens_unique_string(&f, TokenType::QuestionMark));
  total.append(&mut get_tokens_unique_string(&f, TokenType::ForKeyword));
  total.append(&mut get_tokens_unique_string(&f, TokenType::DoKeyword));
  total.append(&mut get_tokens_unique_string(&f, TokenType::WhileKeyword));
  total.append(&mut get_tokens_unique_string(&f, TokenType::BreakKeyword));
  total.append(&mut get_tokens_unique_string(
    &f,
    TokenType::ContinueKeyword,
  ));
  total.sort();
  total
}

fn get_tokens_default_string(f: &String, operator: TokenType) -> Vec<Token> {
  let string = format!(
    "(?<=[a-zA-Z|0-9\\s\\(\\)]){}(?=[a-zA-Z|0-9/\\s\\(\\)])",
    operator.to_regex()
  );
  find_tokens(f, &string, operator)
}

fn get_tokens_unique_string(f: &String, operator: TokenType) -> Vec<Token> {
  find_tokens(f, &operator.to_regex(), operator)
}

fn find_tokens(f: &String, value: &str, token_type: TokenType) -> Vec<Token> {
  let re = Regex::new(value).unwrap();
  let matches = re.find_iter(&f);
  match token_type {
    TokenType::Identifier | TokenType::Integer => gen_tokens(f, matches, &token_type),
    _ => gen_tokens_no_val(matches, &token_type),
  }
}

fn gen_tokens(f: &String, matches: FindMatches, token_type: &TokenType) -> Vec<Token> {
  matches
    .map(|m| {
      let text: String = f.get(m.0..m.1).unwrap().chars().collect();
      Token::new(&text, token_type, m.0)
    })
    .collect()
}

fn gen_tokens_no_val(matches: FindMatches, token_type: &TokenType) -> Vec<Token> {
  matches
    .map(|m| Token::new(&token_type.to_string(), token_type, m.0))
    .collect()
}
