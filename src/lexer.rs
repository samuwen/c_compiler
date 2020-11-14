use crate::{Token, TokenType};
use log::*;
use regex::{Matches, Regex};

pub fn lex(f: String) -> Vec<Token> {
  let mut total = Vec::with_capacity(f.len());
  total.append(&mut find_tokens(&f, "\\{", TokenType::OBrace));
  total.append(&mut find_tokens(&f, "\\}", TokenType::CBrace));
  total.append(&mut find_tokens(&f, "\\(", TokenType::OParen));
  total.append(&mut find_tokens(&f, "\\)", TokenType::CParen));
  total.append(&mut find_tokens(&f, ";", TokenType::Semicolon));
  total.append(&mut find_tokens(&f, "int", TokenType::IntKeyword));
  total.append(&mut find_tokens(&f, "return", TokenType::ReturnKeyword));
  total.append(&mut find_tokens(&f, "[a-zA-Z]\\w*", TokenType::Identifier));
  total.append(&mut find_tokens(&f, "[0-9]+", TokenType::Integer));
  total.append(&mut find_tokens(&f, "-", TokenType::Negation));
  total.append(&mut find_tokens(&f, "~", TokenType::BitwiseComplement));
  total.append(&mut find_tokens(&f, "!", TokenType::LogicalNegation));
  total.append(&mut find_tokens(&f, "\\s\\+\\s", TokenType::Addition));
  total.append(&mut find_tokens(&f, "\\s\\*\\s", TokenType::Multiplication));
  total.append(&mut find_tokens(&f, "\\s/\\s", TokenType::Division));
  total.append(&mut find_tokens(&f, "&&", TokenType::And));
  total.append(&mut find_tokens(&f, "\\|\\|", TokenType::Or));
  total.append(&mut find_tokens(&f, "==", TokenType::Equal));
  total.append(&mut find_tokens(&f, "\\s!=\\s", TokenType::NotEqual));
  total.append(&mut find_tokens(&f, "\\s<\\s", TokenType::LessThan));
  total.append(&mut find_tokens(&f, "<=", TokenType::LessThanOrEqual));
  total.append(&mut find_tokens(&f, "\\s>\\s", TokenType::GreaterThan));
  total.append(&mut find_tokens(&f, ">=", TokenType::GreaterThanOrEqual));
  total.append(&mut find_tokens(&f, "%", TokenType::Modulo));
  total.append(&mut find_tokens(&f, "\\s&\\s", TokenType::BitwiseAnd));
  total.append(&mut find_tokens(&f, "\\s\\|\\s", TokenType::BitwiseOr));
  total.append(&mut find_tokens(&f, "\\^", TokenType::BitwiseXor));
  total.append(&mut find_tokens(&f, "<<", TokenType::BitwiseShl));
  total.append(&mut find_tokens(&f, ">>", TokenType::BitwiseShr));
  total.append(&mut find_tokens(&f, "\\s=\\s", TokenType::Assignment));
  total.append(&mut find_tokens(&f, "\\s\\+=\\s", TokenType::AddAssign));
  total.append(&mut find_tokens(&f, "\\s-=\\s", TokenType::SubAssign));
  total.append(&mut find_tokens(&f, "\\s\\*=\\s", TokenType::MulAssign));
  total.append(&mut find_tokens(&f, "\\s/=\\s", TokenType::DivAssign));
  total.sort();
  total.dedup();
  total = remove_extra_logical_negation_tokens(total);
  total = remove_extra_negation_tokens(total);
  debug!("{:?}", total);
  total
}

fn find_tokens(f: &String, value: &str, token_type: TokenType) -> Vec<Token> {
  let re = Regex::new(value).unwrap();
  gen_tokens(re.find_iter(&f), &token_type)
}

fn gen_tokens(matches: Matches, token_type: &TokenType) -> Vec<Token> {
  matches
    .map(|m| Token::new(m.as_str().trim(), token_type, m.start() as isize))
    .collect()
}

// gross hack because regex library does not support lookahead
fn remove_extra_logical_negation_tokens(mut total: Vec<Token>) -> Vec<Token> {
  let ne_count = total
    .iter()
    .filter(|tok| tok.get_type() == &TokenType::NotEqual)
    .count();
  for _ in 0..ne_count {
    let pos = total
      .iter()
      .position(|tok| tok.get_type() == &TokenType::NotEqual)
      .unwrap();
    let removed = total.remove(pos + 1);
    if removed.get_type() != &TokenType::LogicalNegation {
      panic!(
        "Removed incorrect token after NotEqual expression: {}",
        removed.get_type()
      );
    }
  }
  total
}

// gross hack because regex library does not support lookahead
fn remove_extra_negation_tokens(mut total: Vec<Token>) -> Vec<Token> {
  let ne_count = total
    .iter()
    .filter(|tok| tok.get_type() == &TokenType::SubAssign)
    .count();
  for _ in 0..ne_count {
    let pos = total
      .iter()
      .position(|tok| tok.get_type() == &TokenType::SubAssign)
      .unwrap();
    let removed = total.remove(pos + 1);
    if removed.get_type() != &TokenType::Negation {
      panic!(
        "Removed incorrect token after SubAssign expression: {}",
        removed.get_type()
      );
    }
  }
  total
}
