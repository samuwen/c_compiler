use crate::{Token, TokenType};
use log::*;
use serde::Serialize;
use serde_json::to_string;
use std::fmt;
use std::fmt::{Display, Formatter};

const DATA_TYPE_INDEX: usize = 0;
const FUNCTION_NAME_INDEX: usize = 1;
const OPAREN_INDEX: usize = 2;
const CPAREN_INDEX: usize = 3;

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
    panic!("Statement is not closed by a semicolon");
  }
  n.add_data(String::from(";"));
  n
}

// <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
fn parse_expression(tokens: &mut Vec<Token>) -> Node<String> {
  parse_exp(
    tokens,
    NodeType::Expression,
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
    vec![TokenType::Multiplication, TokenType::Division],
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
    let mut out_string = self.data[0].to_ascii_uppercase().to_owned();
    for child in self.children.iter() {
      &child.add_expression_log(&mut out_string);
    }
    format!("\t\t{} {}\n", out_string, self.data[1])
  }

  fn add_generic_log<F: Fn(&Node<String>, &mut String)>(
    &self,
    out_string: &mut String,
    n_types: Vec<NodeType>,
    fs: Vec<F>,
  ) {
    for child in self.children.iter() {
      let position = n_types.iter().position(|n| n == child.get_type());
      if let Some(pos) = position {
        let f = fs
          .get(pos)
          .expect("Unequal count of node types and functions");
        f(child, out_string);
      } else {
        panic!(
          "Invalid child type in {}: {}",
          self.get_type().as_str(),
          child.get_type().as_str()
        );
      }
    }
  }

  fn add_expression_log(&self, out_string: &mut String) {
    if self.data.len() > 0 {
      out_string.push_str(self.data.get(0).unwrap());
    }
    let fns: Vec<fn(&Node<String>, &mut String)> =
      vec![Node::add_logical_and_log, Node::add_binary_op_log];
    self.add_generic_log(
      out_string,
      vec![NodeType::AndExpression, NodeType::BinaryOp],
      fns,
    );
    if self.data.len() > 0 {
      out_string.push_str(self.data.last().unwrap());
    }
  }

  fn add_logical_and_log(&self, out_string: &mut String) {
    let fns: Vec<fn(&Node<String>, &mut String)> =
      vec![Node::add_equality_log, Node::add_binary_op_log];
    self.add_generic_log(
      out_string,
      vec![NodeType::EqualityExpression, NodeType::BinaryOp],
      fns,
    );
  }

  fn add_equality_log(&self, out_string: &mut String) {
    let fns: Vec<fn(&Node<String>, &mut String)> =
      vec![Node::add_relational_log, Node::add_binary_op_log];
    self.add_generic_log(
      out_string,
      vec![NodeType::RelationalExpression, NodeType::BinaryOp],
      fns,
    );
  }

  fn add_relational_log(&self, out_string: &mut String) {
    let fns: Vec<fn(&Node<String>, &mut String)> =
      vec![Node::add_additive_log, Node::add_binary_op_log];
    self.add_generic_log(
      out_string,
      vec![NodeType::AdditiveExpression, NodeType::BinaryOp],
      fns,
    );
  }

  fn add_additive_log(&self, out_string: &mut String) {
    let fns: Vec<fn(&Node<String>, &mut String)> =
      vec![Node::add_term_log, Node::add_binary_op_log];
    self.add_generic_log(out_string, vec![NodeType::Term, NodeType::BinaryOp], fns);
  }

  fn add_term_log(&self, out_string: &mut String) {
    let fns: Vec<fn(&Node<String>, &mut String)> =
      vec![Node::add_binary_op_log, Node::add_factor_log];
    self.add_generic_log(out_string, vec![NodeType::BinaryOp, NodeType::Factor], fns);
  }

  fn add_factor_log(&self, out_string: &mut String) {
    let fns: Vec<fn(&Node<String>, &mut String)> = vec![
      Node::add_expression_log,
      Node::add_unary_op_log,
      Node::add_integer_log,
    ];
    self.add_generic_log(
      out_string,
      vec![NodeType::Expression, NodeType::UnaryOp, NodeType::Integer],
      fns,
    );
  }

  fn add_binary_op_log(&self, out_string: &mut String) {
    for (i, child) in self.children.iter().enumerate() {
      if i == 1 {
        // janky
        out_string.push_str(self.data.get(0).expect("Binary op has no op"));
      }
      let f = match child.get_type() {
        NodeType::Term => Node::add_term_log,
        NodeType::Factor => Node::add_factor_log,
        NodeType::BinaryOp => Node::add_binary_op_log,
        _ => panic!(
          "Invalid child type in binary op: {}",
          child.get_type().as_str()
        ),
      };
      f(child, out_string);
    }
  }

  fn add_unary_op_log(&self, out_string: &mut String) {
    out_string.push_str(self.data.get(0).expect("Unary op has no op"));
    for child in self.children.iter() {
      child.add_factor_log(out_string);
    }
  }

  fn add_integer_log(&self, out_string: &mut String) {
    out_string.push_str(&self.data.join(" "))
  }

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
      child.get_expression_asm(out_vec);
    }
    out_vec.push(format!("\tret"));
  }

  fn get_generic_asm<F: Fn(&Node<String>, &mut Vec<String>)>(
    &self,
    out_vec: &mut Vec<String>,
    n_types: Vec<NodeType>,
    fs: Vec<F>,
  ) {
    for child in self.children.iter() {
      let position = n_types.iter().position(|n| n == child.get_type());
      if let Some(pos) = position {
        let f = fs
          .get(pos)
          .expect("Unequal count of node types and functions");
        f(child, out_vec);
      } else {
        panic!(
          "Invalid child type in {}: {}",
          self.get_type().as_str(),
          child.get_type().as_str()
        );
      }
    }
  }

  fn get_expression_asm(&self, out_vec: &mut Vec<String>) {
    let fns: Vec<fn(&Node<String>, &mut Vec<String>)> =
      vec![Node::get_logical_and_asm, Node::get_binary_op_asm];
    self.get_generic_asm(
      out_vec,
      vec![NodeType::AndExpression, NodeType::BinaryOp],
      fns,
    );
  }

  fn get_logical_and_asm(&self, out_vec: &mut Vec<String>) {
    let fns: Vec<fn(&Node<String>, &mut Vec<String>)> =
      vec![Node::get_equality_asm, Node::get_binary_op_asm];
    self.get_generic_asm(
      out_vec,
      vec![NodeType::EqualityExpression, NodeType::BinaryOp],
      fns,
    );
  }

  fn get_equality_asm(&self, out_vec: &mut Vec<String>) {
    let fns: Vec<fn(&Node<String>, &mut Vec<String>)> =
      vec![Node::get_relational_asm, Node::get_binary_op_asm];
    self.get_generic_asm(
      out_vec,
      vec![NodeType::RelationalExpression, NodeType::BinaryOp],
      fns,
    );
  }

  fn get_relational_asm(&self, out_vec: &mut Vec<String>) {
    let fns: Vec<fn(&Node<String>, &mut Vec<String>)> =
      vec![Node::get_additive_asm, Node::get_binary_op_asm];
    self.get_generic_asm(
      out_vec,
      vec![NodeType::AdditiveExpression, NodeType::BinaryOp],
      fns,
    );
  }

  fn get_additive_asm(&self, out_vec: &mut Vec<String>) {
    let fns: Vec<fn(&Node<String>, &mut Vec<String>)> =
      vec![Node::get_term_asm, Node::get_binary_op_asm];
    self.get_generic_asm(out_vec, vec![NodeType::Term, NodeType::BinaryOp], fns);
  }

  fn get_term_asm(&self, out_vec: &mut Vec<String>) {
    for child in self.children.iter() {
      let f = match child.get_type() {
        NodeType::BinaryOp => Node::get_binary_op_asm,
        NodeType::Factor => Node::get_factor_asm,
        _ => panic!("Invalid child type in Term: {}", child.get_type().as_str()),
      };
      f(child, out_vec);
    }
  }

  fn get_binary_op_asm(&self, out_vec: &mut Vec<String>) {
    for child in self.children.iter() {
      let f = match child.get_type() {
        NodeType::Term => Node::get_term_asm,
        NodeType::Factor => Node::get_factor_asm,
        NodeType::BinaryOp => Node::get_binary_op_asm,
        _ => panic!(
          "Invalid child type in binary op: {}",
          child.get_type().as_str()
        ),
      };
      f(child, out_vec);
    }
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
      _ => panic!("hurp"),
    }
  }

  fn add_arith_stack_asm(&self, out_vec: &mut Vec<String>) {
    out_vec.insert(out_vec.len() - 1, format!("\tpush\t%eax"));
    out_vec.push(format!("\tpop\t%ecx"));
  }

  fn get_factor_asm(&self, out_vec: &mut Vec<String>) {
    for child in self.children.iter() {
      let f = match child.get_type() {
        NodeType::Expression => Node::get_expression_asm,
        NodeType::UnaryOp => Node::get_unary_op_asm,
        NodeType::Integer => Node::get_integer_asm,
        _ => panic!(
          "Invalid child type in factor: {}",
          child.get_type().as_str()
        ),
      };
      f(child, out_vec);
    }
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
}

#[derive(Debug, Serialize, Eq, PartialEq)]
pub enum NodeType {
  Program,
  Function,
  Statement,
  Expression,
  AndExpression,
  EqualityExpression,
  RelationalExpression,
  AdditiveExpression,
  Term,
  Factor,
  Integer,
  UnaryOp,
  BinaryOp,
}

impl NodeType {
  fn as_str(&self) -> &str {
    match self {
      NodeType::Program => "Program",
      NodeType::Function => "Function",
      NodeType::Statement => "Statement",
      NodeType::Expression => "Expression",
      NodeType::Term => "Term",
      NodeType::Factor => "Factor",
      NodeType::Integer => "Integer",
      NodeType::UnaryOp => "UnaryOp",
      NodeType::BinaryOp => "BinaryOp",
      NodeType::AndExpression => "AndExpression",
      NodeType::EqualityExpression => "EqualityExpression",
      NodeType::RelationalExpression => "RelationalExpression",
      NodeType::AdditiveExpression => "AdditiveExpression",
    }
  }
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
