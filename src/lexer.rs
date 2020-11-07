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
  total.append(&mut find_tokens(&f, "\\+", TokenType::Addition));
  total.append(&mut find_tokens(&f, "\\*", TokenType::Multiplication));
  total.append(&mut find_tokens(&f, "/", TokenType::Division));
  total.sort();
  total.dedup();
  trace!("{:?}", total);
  total
}

fn find_tokens(f: &String, value: &str, token_type: TokenType) -> Vec<Token> {
  let re = Regex::new(value).unwrap();
  gen_tokens(re.find_iter(&f), &token_type)
}

fn gen_tokens(matches: Matches, token_type: &TokenType) -> Vec<Token> {
  matches
    .map(|m| Token::new(m.as_str(), token_type, m.start() as isize))
    .collect()
}