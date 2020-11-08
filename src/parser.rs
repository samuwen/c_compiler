use crate::{Token, TokenType};
use log::*;
use serde::Serialize;
use serde_json::to_string;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::sync::atomic::{AtomicUsize, Ordering};

const DATA_TYPE_INDEX: usize = 0;
const FUNCTION_NAME_INDEX: usize = 1;
const OPAREN_INDEX: usize = 2;
const CPAREN_INDEX: usize = 3;

static COUNT: AtomicUsize = AtomicUsize::new(0);

fn get_next_label() -> String {
  format!("label{}", COUNT.fetch_add(1, Ordering::Relaxed))
}

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
  let json = to_string(&function.get_children()[0].get_children()[0]).unwrap();
  std::fs::write("out.json", &json).expect("Failed to write");
  debug!("{}", function);
  Prog::new(function)
}

// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
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
  let statement = parse_statement(tokens);
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::CBrace {
    panic!("Function declaration not closed by brace");
  }
  n.add_child(statement);
  n
}

// <statement> ::= "return" <exp> ";"
fn parse_statement(tokens: &mut Vec<Token>) -> Node<String> {
  let mut n = Node::new(NodeType::Statement);
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::ReturnKeyword {
    panic!("Statement does not have return keyword");
  }
  n.add_data(String::from("return "));
  let expression = parse_expression(tokens);
  n.add_child(expression);
  let token = tokens.remove(0);
  if token.get_type() != &TokenType::Semicolon {
    panic!(
      "Last value I know in a statement is a {:?}, not a semicolon",
      token.get_type()
    );
  }
  n.add_data(String::from(";"));
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
    _ => panic!("the disco"),
  }
  n
}

#[derive(Debug, Serialize)]
pub struct Node<T> {
  _type: NodeType,
  child_count: usize,
  data: Vec<T>,
  children: Vec<Node<T>>,
}

impl<T> Node<T> {
  fn new(node_type: NodeType) -> Node<T> {
    Node {
      _type: node_type,
      data: vec![],
      children: vec![],
      child_count: 0,
    }
  }

  fn add_child(&mut self, child: Node<T>) {
    self.child_count += 1;
    self.children.push(child);
  }

  fn add_data(&mut self, data: T) {
    self.data.push(data);
  }

  pub fn get_children(&self) -> &Vec<Node<T>> {
    &self.children
  }

  pub fn get_type(&self) -> &NodeType {
    &self._type
  }
}

impl Node<String> {
  fn add_function_log(&self) -> String {
    format!(
      "FUN {} {}:\n\tparams: {}{}\n\tbody:\n",
      self.data[DATA_TYPE_INDEX].to_ascii_uppercase(),
      self.data[FUNCTION_NAME_INDEX],
      self.data[OPAREN_INDEX],
      self.data[CPAREN_INDEX]
    )
  }

  fn add_statement_log(&self) -> String {
    let mut out_vec = vec![self.data[0].to_ascii_uppercase().to_owned()];
    for child in self.children.iter() {
      &child.add_logical_or_log(&mut out_vec);
    }
    format!("\t\t{} {}\n", out_vec.join(""), self.data[1])
  }

  fn add_generic_log(&self, out_vec: &mut Vec<String>) {
    let mut original_len = out_vec.len();
    for child in self.children.iter() {
      original_len = out_vec.len();
      let f = self.get_function_for_node_type(child.get_type(), false);
      f(child, out_vec);
    }
    if self.get_type() == &NodeType::BinaryOp && self.data.len() > 0 {
      out_vec.insert(
        original_len,
        self.data.get(0).expect("Binary op has no op").to_owned(),
      );
    }
  }

  fn add_logical_or_log(&self, out_vec: &mut Vec<String>) {
    if self.data.len() > 0 {
      out_vec.push(self.data.get(0).unwrap().to_owned());
    }
    self.add_generic_log(out_vec);
    if self.data.len() > 0 {
      out_vec.push(self.data.last().unwrap().to_owned());
    }
  }

  fn add_logical_and_log(&self, out_vec: &mut Vec<String>) {
    self.add_generic_log(out_vec);
  }

  fn add_bitwise_or_log(&self, out_vec: &mut Vec<String>) {
    self.add_generic_log(out_vec);
  }

  fn add_bitwise_xor_log(&self, out_vec: &mut Vec<String>) {
    self.add_generic_log(out_vec);
  }

  fn add_bitwise_and_log(&self, out_vec: &mut Vec<String>) {
    self.add_generic_log(out_vec);
  }

  fn add_equality_log(&self, out_vec: &mut Vec<String>) {
    self.add_generic_log(out_vec);
  }

  fn add_relational_log(&self, out_vec: &mut Vec<String>) {
    self.add_generic_log(out_vec);
  }

  fn add_shift_log(&self, out_vec: &mut Vec<String>) {
    self.add_generic_log(out_vec);
  }

  fn add_additive_log(&self, out_vec: &mut Vec<String>) {
    self.add_generic_log(out_vec);
  }

  fn add_term_log(&self, out_vec: &mut Vec<String>) {
    self.add_generic_log(out_vec);
  }

  fn add_factor_log(&self, out_vec: &mut Vec<String>) {
    self.add_generic_log(out_vec);
  }

  fn add_binary_op_log(&self, out_vec: &mut Vec<String>) {
    self.add_generic_log(out_vec)
  }

  fn add_unary_op_log(&self, out_vec: &mut Vec<String>) {
    out_vec.push(self.data.get(0).expect("Unary op has no op").to_owned());
    for child in self.children.iter() {
      child.add_factor_log(out_vec);
    }
  }

  fn add_integer_log(&self, out_vec: &mut Vec<String>) {
    out_vec.push(self.data.join(" "))
  }

  // ===================== Assembly builder statements ====================

  pub fn get_function_asm(&self, out_vec: &mut Vec<String>) {
    let name = self.data.get(1).unwrap();
    out_vec.push(format!(".globl {}", name));
    out_vec.push(format!("{}:", name));
    for statement in self.children.iter() {
      statement.get_statement_asm(out_vec);
    }
  }

  fn get_statement_asm(&self, out_vec: &mut Vec<String>) {
    for child in self.children.iter() {
      child.get_logical_or_asm(out_vec);
    }
    out_vec.push(format!("\tret"));
  }

  fn get_generic_asm(&self, out_vec: &mut Vec<String>) {
    for child in self.children.iter() {
      let f = self.get_function_for_node_type(child.get_type(), true);
      f(child, out_vec);
    }
  }

  fn get_logical_or_asm(&self, out_vec: &mut Vec<String>) {
    self.get_generic_asm(out_vec);
  }

  fn get_logical_and_asm(&self, out_vec: &mut Vec<String>) {
    self.get_generic_asm(out_vec);
  }

  fn get_bitwise_or_asm(&self, out_vec: &mut Vec<String>) {
    self.get_generic_asm(out_vec);
  }

  fn get_bitwise_xor_asm(&self, out_vec: &mut Vec<String>) {
    self.get_generic_asm(out_vec);
  }

  fn get_bitwise_and_asm(&self, out_vec: &mut Vec<String>) {
    self.get_generic_asm(out_vec);
  }

  fn get_equality_asm(&self, out_vec: &mut Vec<String>) {
    self.get_generic_asm(out_vec);
  }

  fn get_relational_asm(&self, out_vec: &mut Vec<String>) {
    self.get_generic_asm(out_vec);
  }

  fn get_shift_asm(&self, out_vec: &mut Vec<String>) {
    self.get_generic_asm(out_vec);
  }

  fn get_additive_asm(&self, out_vec: &mut Vec<String>) {
    self.get_generic_asm(out_vec);
  }

  fn get_term_asm(&self, out_vec: &mut Vec<String>) {
    self.get_generic_asm(out_vec);
  }

  fn get_binary_op_asm(&self, out_vec: &mut Vec<String>) {
    self.get_generic_asm(out_vec);
    let mut handle_comparison = || {
      self.add_arith_stack_asm(out_vec);
      out_vec.push(format!("\tcmpl\t%eax, %ecx"));
      out_vec.push(format!("\tmovl\t$0, %eax"));
    };
    match self.data.get(0).unwrap().as_str() {
      "+" => {
        self.add_arith_stack_asm(out_vec);
        out_vec.push(format!("\taddl\t%ecx, %eax"));
      }
      "*" => {
        self.add_arith_stack_asm(out_vec);
        out_vec.push(format!("\timul\t%ecx, %eax"));
      }
      "-" => {
        self.add_arith_stack_asm(out_vec);
        out_vec.push(format!("\tsubl\t%eax, %ecx"));
        out_vec.push(format!("\tmovl\t%ecx, %eax"));
      }
      "/" => {
        let last_arith_ele = out_vec.remove(out_vec.len() - 1);
        let modified_arith_ele = &last_arith_ele.replace("eax", "ecx");
        out_vec.push(modified_arith_ele.to_owned());
        out_vec.push(format!("\tcdq"));
        out_vec.push(format!("\tidivl\t%ecx"));
      }
      "==" => {
        handle_comparison();
        out_vec.push(format!("\tsete\t%al"));
      }
      "!=" => {
        handle_comparison();
        out_vec.push(format!("\tsetne\t%al"));
      }
      ">" => {
        handle_comparison();
        out_vec.push(format!("\tsetg\t%al"));
      }
      ">=" => {
        handle_comparison();
        out_vec.push(format!("\tsetge\t%al"));
      }
      "<" => {
        handle_comparison();
        out_vec.push(format!("\tsetl\t%al"));
      }
      "<=" => {
        handle_comparison();
        out_vec.push(format!("\tsetle\t%al"));
      }
      "||" => {
        let label1 = get_next_label();
        let label2 = get_next_label();
        let last_arith_ele = out_vec.remove(out_vec.len() - 1);
        out_vec.push(format!("\tcmpl\t$0, %eax"));
        out_vec.push(format!("\tje\t{}", label1));
        out_vec.push(format!("\tmovl\t$1, %eax"));
        out_vec.push(format!("\tjmp\t{}", label2));
        out_vec.push(format!("{}:", label1));
        out_vec.push(last_arith_ele.to_owned());
        out_vec.push(format!("\tcmpl\t$0, %eax"));
        out_vec.push(format!("\tmovl\t$0, %eax"));
        out_vec.push(format!("\tsetne\t%al"));
        out_vec.push(format!("{}:", label2));
      }
      "&&" => {
        let label1 = get_next_label();
        let label2 = get_next_label();
        let last_arith_ele = out_vec.remove(out_vec.len() - 1);
        out_vec.push(format!("\tcmpl\t$0, %eax"));
        out_vec.push(format!("\tjne\t{}", label1));
        out_vec.push(format!("\tjmp\t{}", label2));
        out_vec.push(format!("{}:", label1));
        out_vec.push(last_arith_ele.to_owned());
        out_vec.push(format!("\tcmpl\t$0, %eax"));
        out_vec.push(format!("\tmovl\t$0, %eax"));
        out_vec.push(format!("\tsetne\t%al"));
        out_vec.push(format!("{}:", label2));
      }
      _ => panic!("Couldn't find assembly for op: {}", self.data[0]),
    }
  }

  fn add_arith_stack_asm(&self, out_vec: &mut Vec<String>) {
    out_vec.insert(out_vec.len() - 1, format!("\tpush\t%eax"));
    out_vec.push(format!("\tpop\t%ecx"));
  }

  fn get_factor_asm(&self, out_vec: &mut Vec<String>) {
    self.get_generic_asm(out_vec);
  }

  fn get_unary_op_asm(&self, out_vec: &mut Vec<String>) {
    for child in self.children.iter() {
      child.get_factor_asm(out_vec);
    }
    match self.data.get(0).expect("Unary op has no op").as_str() {
      "~" => out_vec.push(format!("\tnot\t%eax")),
      "-" => out_vec.push(format!("\tneg\t%eax")),
      "!" => {
        out_vec.push(format!("\tcmpl\t$0, %eax"));
        out_vec.push(format!("\tmovl\t$0, %eax"));
        out_vec.push(format!("\tsete\t%al"));
      }
      _ => panic!(
        "Invalid char passed to unary op: {}",
        self.data.get(0).unwrap()
      ),
    }
  }

  fn get_integer_asm(&self, out_vec: &mut Vec<String>) {
    out_vec.push(format!("\tmovl\t${}, %eax", self.data.join("")));
  }

  fn get_function_for_node_type(
    &self,
    nt: &NodeType,
    is_asm: bool,
  ) -> fn(&Node<String>, &mut Vec<String>) {
    match nt {
      NodeType::OrExpression => match is_asm {
        true => Node::get_logical_or_asm,
        false => Node::add_logical_or_log,
      },
      NodeType::AndExpression => match is_asm {
        true => Node::get_logical_and_asm,
        false => Node::add_logical_and_log,
      },
      NodeType::BitwiseOrExpression => match is_asm {
        true => Node::get_bitwise_or_asm,
        false => Node::add_bitwise_or_log,
      },
      NodeType::BitwiseXorExpression => match is_asm {
        true => Node::get_bitwise_xor_asm,
        false => Node::add_bitwise_xor_log,
      },
      NodeType::BitwiseAndExpression => match is_asm {
        true => Node::get_bitwise_and_asm,
        false => Node::add_bitwise_and_log,
      },
      NodeType::EqualityExpression => match is_asm {
        true => Node::get_equality_asm,
        false => Node::add_equality_log,
      },
      NodeType::RelationalExpression => match is_asm {
        true => Node::get_relational_asm,
        false => Node::add_relational_log,
      },
      NodeType::ShiftExpression => match is_asm {
        true => Node::get_shift_asm,
        false => Node::add_shift_log,
      },
      NodeType::AdditiveExpression => match is_asm {
        true => Node::get_additive_asm,
        false => Node::add_additive_log,
      },
      NodeType::Term => match is_asm {
        true => Node::get_term_asm,
        false => Node::add_term_log,
      },
      NodeType::Factor => match is_asm {
        true => Node::get_factor_asm,
        false => Node::add_factor_log,
      },
      NodeType::Integer => match is_asm {
        true => Node::get_integer_asm,
        false => Node::add_integer_log,
      },
      NodeType::UnaryOp => match is_asm {
        true => Node::get_unary_op_asm,
        false => Node::add_unary_op_log,
      },
      NodeType::BinaryOp => match is_asm {
        true => Node::get_binary_op_asm,
        false => Node::add_binary_op_log,
      },
      _ => panic!("Type {} doesn't have two associated functions"),
    }
  }
}

#[derive(Debug, Serialize, Eq, PartialEq)]
pub enum NodeType {
  Program,
  Function,
  Statement,
  OrExpression,
  AndExpression,
  BitwiseOrExpression,
  BitwiseXorExpression,
  BitwiseAndExpression,
  EqualityExpression,
  RelationalExpression,
  ShiftExpression,
  AdditiveExpression,
  Term,
  Factor,
  Integer,
  UnaryOp,
  BinaryOp,
}

impl Display for Node<String> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let mut out_string = String::new();
    out_string.push_str(&self.add_function_log());
    for child in self.children.iter() {
      out_string.push_str(&child.add_statement_log());
    }
    write!(f, "\n{}", out_string)
  }
}
