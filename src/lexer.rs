use crate::{Token, TokenType};
use log::*;
use onig::{FindMatches, Regex};

pub fn lex(f: String) -> Vec<Token> {
  let mut total = Vec::with_capacity(f.len());
  total.append(&mut find_tokens(&f, "{", TokenType::OBrace));
  total.append(&mut find_tokens(&f, "}", TokenType::CBrace));
  total.append(&mut find_tokens(&f, "\\(", TokenType::OParen));
  total.append(&mut find_tokens(&f, "\\)", TokenType::CParen));
  total.append(&mut find_tokens(&f, ";", TokenType::Semicolon));
  total.append(&mut find_tokens(&f, "int", TokenType::IntKeyword));
  total.append(&mut find_tokens(&f, "return", TokenType::ReturnKeyword));
  total.append(&mut find_tokens(&f, "[a-zA-Z]\\w*", TokenType::Identifier));
  total.append(&mut find_tokens(&f, "[0-9]+", TokenType::Integer));
  total.append(&mut find_tokens(
    &f,
    "(?<=\\s)-(?=[0-9|a-zA-Z]|\\s)",
    TokenType::Negation,
  ));
  total.append(&mut find_tokens(&f, "~", TokenType::BitwiseComplement));
  total.append(&mut find_tokens(
    &f,
    "!(?=[0-9|a-zA-Z])",
    TokenType::LogicalNegation,
  ));
  total.append(&mut find_tokens(&f, "\\s\\+\\s", TokenType::Addition));
  total.append(&mut find_tokens(&f, "\\s\\*\\s", TokenType::Multiplication));
  total.append(&mut find_tokens(&f, "\\s/\\s", TokenType::Division));
  total.append(&mut find_tokens(&f, "\\s&&\\s", TokenType::And));
  total.append(&mut find_tokens(&f, "\\|\\|", TokenType::Or));
  total.append(&mut find_tokens(&f, "\\s==\\s", TokenType::Equal));
  total.append(&mut find_tokens(&f, "\\s!=\\s", TokenType::NotEqual));
  total.append(&mut find_tokens(&f, "\\s<\\s", TokenType::LessThan));
  total.append(&mut find_tokens(&f, "\\s<=\\s", TokenType::LessThanOrEqual));
  total.append(&mut find_tokens(&f, "\\s>\\s", TokenType::GreaterThan));
  total.append(&mut find_tokens(
    &f,
    "\\s>=\\s",
    TokenType::GreaterThanOrEqual,
  ));
  total.append(&mut find_tokens(&f, "\\s%\\s", TokenType::Modulo));
  total.append(&mut find_tokens(&f, "\\s&\\s", TokenType::BitwiseAnd));
  total.append(&mut find_tokens(&f, "\\s\\|\\s", TokenType::BitwiseOr));
  total.append(&mut find_tokens(&f, "\\^\\s", TokenType::BitwiseXor));
  total.append(&mut find_tokens(&f, "\\s<<\\s", TokenType::BitwiseShl));
  total.append(&mut find_tokens(&f, "\\s>>\\s", TokenType::BitwiseShr));
  total.append(&mut find_tokens(&f, "\\s=\\s", TokenType::Assignment));
  total.append(&mut find_tokens(&f, "\\s\\+=\\s", TokenType::AddAssign));
  total.append(&mut find_tokens(&f, "\\s-=\\s", TokenType::SubAssign));
  total.append(&mut find_tokens(&f, "\\s\\*=\\s", TokenType::MulAssign));
  total.append(&mut find_tokens(&f, "\\s/=\\s", TokenType::DivAssign));
  total.append(&mut find_tokens(&f, "\\s%=\\s", TokenType::ModAssign));
  total.append(&mut find_tokens(&f, "\\s<<=\\s", TokenType::ShlAssign));
  total.append(&mut find_tokens(&f, "\\s>>=\\s", TokenType::ShrAssign));
  total.append(&mut find_tokens(&f, "\\s&=\\s", TokenType::AndAssign));
  total.append(&mut find_tokens(&f, "\\s\\|=\\s", TokenType::OrAssign));
  total.append(&mut find_tokens(&f, "\\s\\^=\\s", TokenType::XorAssign));
  total.append(&mut find_tokens(
    &f,
    "\\+\\+(?=[0-9|a-zA-Z])",
    TokenType::PreIncrement,
  ));
  total.append(&mut find_tokens(
    &f,
    "(?<=[0-9|a-zA-Z])\\+\\+",
    TokenType::PostIncrement,
  ));
  total.append(&mut find_tokens(
    &f,
    "--(?=[0-9|a-zA-Z])",
    TokenType::PreDecrement,
  ));
  total.append(&mut find_tokens(
    &f,
    "(?<=[0-9|a-zA-Z])--",
    TokenType::PostDecrement,
  ));
  total.sort();
  total.dedup();
  debug!("{:?}", total);
  total
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
