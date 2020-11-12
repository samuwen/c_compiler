use crate::Prog;
use log::*;
use serde::Serialize;
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

pub fn generate(ast: Prog) -> String {
  let function = ast.get_function();
  let mut asm_vec = vec![];
  function.get_function_asm(&mut asm_vec);
  asm_vec.join("\n")
}

#[derive(Debug, Serialize)]
pub struct Node<T> {
  _type: NodeType,
  child_count: usize,
  data: Vec<T>,
  children: Vec<Node<T>>,
}

impl<T> Node<T> {
  pub fn new(node_type: NodeType) -> Node<T> {
    Node {
      _type: node_type,
      data: vec![],
      children: vec![],
      child_count: 0,
    }
  }

  pub fn add_child(&mut self, child: Node<T>) {
    self.child_count += 1;
    self.children.push(child);
  }

  pub fn add_data(&mut self, data: T) {
    self.data.push(data);
  }

  pub fn get_children(&self) -> &Vec<Node<T>> {
    &self.children
  }

  pub fn get_type(&self) -> &NodeType {
    &self._type
  }

  pub fn get_data(&self) -> &Vec<T> {
    &self.data
  }
}

impl Node<String> {
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

  fn add_function_log(&self, out_vec: &mut Vec<String>) {
    let f_string = format!(
      "FUN {} {}:\n\tparams: {}{}\n\tbody:",
      self.data[DATA_TYPE_INDEX].to_ascii_uppercase(),
      self.data[FUNCTION_NAME_INDEX],
      self.data[OPAREN_INDEX],
      self.data[CPAREN_INDEX]
    );
    out_vec.push(f_string);
    for child in self.children.iter() {
      child.add_statement_log(out_vec);
    }
  }

  fn add_statement_log(&self, out_vec: &mut Vec<String>) {
    if self.child_count == 0 {
      let mut statement = String::from("\t\t");
      statement.push_str(&self.data.join(" "));
      statement.push_str(";");
      out_vec.push(statement);
    }
    for child in self.children.iter() {
      let mut statement_vec = vec![];
      match self.get_type() {
        NodeType::AssignmentStatement => {
          statement_vec.push(self.data.join(" ")); // type and id
          child.add_assignment_log(&mut statement_vec);
        }
        NodeType::ExpressionStatement => {
          child.add_logical_or_log(&mut statement_vec);
        }
        NodeType::ReturnStatement => {
          statement_vec.push(String::from("return "));
          child.add_logical_or_log(&mut statement_vec);
        }
        _ => panic!("Invalid statement type: {:?}", self.get_type()),
      };
      let mut tabs = String::from("\t\t");
      let mut statement_string = statement_vec.join(" ");
      statement_string.push_str(";");
      tabs.push_str(&statement_string);
      out_vec.push(tabs);
    }
  }

  fn add_assignment_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    out_vec.push(self.data.join("")); // type and identifier
    for child in self.children.iter() {
      child.add_logical_or_log(out_vec);
    }
    self.data.clone()
  }

  fn add_logical_or_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.add_generic_log(out_vec);
    self.data.clone()
  }

  fn add_logical_and_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.add_generic_log(out_vec);
    self.data.clone()
  }

  fn add_bitwise_or_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.add_generic_log(out_vec);
    self.data.clone()
  }

  fn add_bitwise_xor_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.add_generic_log(out_vec);
    self.data.clone()
  }

  fn add_bitwise_and_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.add_generic_log(out_vec);
    self.data.clone()
  }

  fn add_equality_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.add_generic_log(out_vec);
    self.data.clone()
  }

  fn add_relational_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.add_generic_log(out_vec);
    self.data.clone()
  }

  fn add_shift_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.add_generic_log(out_vec);
    self.data.clone()
  }

  fn add_additive_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.add_generic_log(out_vec);
    self.data.clone()
  }

  fn add_term_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.add_generic_log(out_vec);
    self.data.clone()
  }

  fn add_factor_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.add_generic_log(out_vec);
    self.data.clone()
  }

  fn add_binary_op_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.add_generic_log(out_vec);
    self.data.clone()
  }

  fn add_unary_op_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    out_vec.push(self.data.get(0).expect("Unary op has no op").to_owned());
    for child in self.children.iter() {
      child.add_factor_log(out_vec);
    }
    self.data.clone()
  }

  fn add_integer_log(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    out_vec.push(self.data.join(" "));
    self.data.clone()
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
    let mut data = vec![];
    for child in self.children.iter() {
      let mut d = match child.get_type() {
        NodeType::BinaryOp => child.get_binary_op_asm(out_vec),
        NodeType::Integer => child.get_integer_asm(out_vec),
        NodeType::Factor => child.get_factor_asm(out_vec),
        _ => panic!("unknown child type {:?}", child.get_type()),
      };
      data.append(&mut d);
    }
    // if nothing else has been added, we know we just have a single number
    if out_vec.len() == 2 {
      self.load_eax_reg(out_vec, data.get(0).unwrap());
    }
    if self.get_type() == &NodeType::ReturnStatement {
      out_vec.push(format!("\tret"));
    }
  }

  fn get_generic_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    let mut data = vec![];
    for child in self.children.iter() {
      let f = self.get_function_for_node_type(child.get_type(), true);
      let mut d = f(child, out_vec);
      data.append(&mut d);
    }
    data
  }

  fn get_logical_or_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.get_generic_asm(out_vec)
  }

  fn get_logical_and_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.get_generic_asm(out_vec)
  }

  fn get_bitwise_or_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.get_generic_asm(out_vec)
  }

  fn get_bitwise_xor_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.get_generic_asm(out_vec)
  }

  fn get_bitwise_and_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.get_generic_asm(out_vec)
  }

  fn get_equality_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.get_generic_asm(out_vec)
  }

  fn get_relational_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.get_generic_asm(out_vec)
  }

  fn get_shift_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.get_generic_asm(out_vec)
  }

  fn get_additive_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.get_generic_asm(out_vec)
  }

  fn get_term_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.get_generic_asm(out_vec)
  }

  fn get_binary_op_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    let mut data: Vec<String> = vec![];
    for child in self.children.iter() {
      match child.get_type() {
        NodeType::BinaryOp => {
          child.get_binary_op_asm(out_vec);
        }
        NodeType::Integer => {
          data.append(&mut child.get_data().clone());
        }
        _ => panic!("Unknown child for binary op: {:?}", child.get_type()),
      }
    }
    if data.len() != 2 {
      warn!("Binary op data vec has {} elements", data.len());
    }
    let d2 = data.get(1);
    let d1 = data.get(0).unwrap();
    let mut handle_comparison = || {
      self.load_arithmetic_asm(out_vec, d1, d2);
      out_vec.push(format!("\tcmpl\t%eax, %ecx"));
      out_vec.push(format!("\tmovl\t$0, %eax"));
    };
    match self.data.get(0).unwrap().as_str() {
      "+" => {
        self.load_arithmetic_asm(out_vec, d1, d2);
        out_vec.push(format!("\taddl\t%ecx, %eax"));
      }
      "*" => {
        self.load_arithmetic_asm(out_vec, d1, d2);
        out_vec.push(format!("\timul\t%ecx, %eax"));
      }
      "-" => {
        self.load_arithmetic_asm(out_vec, d1, d2);
        out_vec.push(format!("\tsubl\t%eax, %ecx"));
        out_vec.push(format!("\tmovl\t%ecx, %eax"));
      }
      "/" => {
        match d2 {
          Some(value) => {
            self.load_eax_reg(out_vec, &d1);
            self.load_reg(out_vec, &value, "ecx");
          }
          None => {
            out_vec.push(String::from("\tmovl\t%eax, %ecx"));
            self.load_eax_reg(out_vec, &d1);
          }
        }
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
        match d2 {
          Some(value) => {
            self.load_eax_reg(out_vec, d1);
            out_vec.push(format!("\tcmpl\t$0, %eax"));
            out_vec.push(format!("\tje\t{}", label1));
            out_vec.push(format!("\tmovl\t$1, %eax"));
            out_vec.push(format!("\tjmp\t{}", label2));
            out_vec.push(format!("{}:", label1));
            self.load_eax_reg(out_vec, value);
            out_vec.push(format!("\tcmpl\t$0, %eax"));
            out_vec.push(format!("\tmovl\t$0, %eax"));
            out_vec.push(format!("\tsetne\t%al"));
            out_vec.push(format!("{}:", label2));
          }
          None => {
            self.load_eax_reg(out_vec, d1);
            out_vec.push(format!("\tcmpl\t$0, %eax"));
            out_vec.push(format!("\tmovl\t$0, %eax"));
            out_vec.push(format!("\tsetne\t%al"));
            out_vec.push(format!("{}:", label2));
          }
        }
      }
      "&&" => {
        let label1 = get_next_label();
        let label2 = get_next_label();
        match d2 {
          Some(value) => {
            self.load_eax_reg(out_vec, d1);
            out_vec.push(format!("\tcmpl\t$0, %eax"));
            out_vec.push(format!("\tjne\t{}", label1));
            out_vec.push(format!("\tjmp\t{}", label2));
            out_vec.push(format!("{}:", label1));
            self.load_eax_reg(out_vec, value);
            out_vec.push(format!("\tcmpl\t$0, %eax"));
            out_vec.push(format!("\tmovl\t$0, %eax"));
            out_vec.push(format!("\tsetne\t%al"));
            out_vec.push(format!("{}:", label2));
          }
          None => {
            self.load_eax_reg(out_vec, d1);
            out_vec.push(format!("\tcmpl\t$0, %eax"));
            out_vec.push(format!("\tmovl\t$0, %eax"));
            out_vec.push(format!("\tsetne\t%al"));
            out_vec.push(format!("{}:", label2));
          }
        }
      }
      "%" => {
        self.load_eax_reg(out_vec, &d1);
        self.load_reg(out_vec, d2.expect("no d2 found"), "ecx");
        out_vec.push(format!("\tcdq"));
        out_vec.push(format!("\tidivl\t%ecx"));
        out_vec.push(format!("\tmovl\t%edx, %eax"));
      }
      "&" => {
        self.load_arithmetic_asm(out_vec, d1, d2);
        out_vec.push(format!("\tand\t%ecx, %eax"));
      }
      "|" => {
        self.load_arithmetic_asm(out_vec, d1, d2);
        out_vec.push(format!("\tor\t%ecx, %eax"));
      }
      "^" => {
        self.load_arithmetic_asm(out_vec, d1, d2);
        out_vec.push(format!("\txor\t%ecx, %eax"));
      }
      "<<" => {
        self.load_reg(out_vec, &d2.expect("No d2 found"), "ecx");
        self.load_eax_reg(out_vec, d1);
        out_vec.push(format!("\tsall\t%cl, %eax"));
      }
      ">>" => {
        self.load_reg(out_vec, d2.expect("No d2 found"), "ecx");
        self.load_eax_reg(out_vec, d1);
        out_vec.push(format!("\tsarl\t%cl, %eax"));
      }
      _ => panic!("Couldn't find assembly for op: {}", self.data[0]),
    }
    data
  }

  fn load_reg(&self, out_vec: &mut Vec<String>, num: &String, reg: &str) {
    out_vec.push(format!("\tmovl\t${}, %{}", num, reg))
  }

  fn load_eax_reg(&self, out_vec: &mut Vec<String>, num: &String) {
    self.load_reg(out_vec, num, "eax");
  }

  fn load_arithmetic_asm(&self, out: &mut Vec<String>, d1: &String, d2: Option<&String>) {
    match d2 {
      Some(value) => {
        self.load_reg(out, d1, "ecx");
        // out.push(format!("\tpush\t%eax"));
        self.load_eax_reg(out, &value);
        // out.push(format!("\tpop\t%ecx"));
      }
      None => {
        // out.push(format!("\tpush\t%eax"));
        out.push(format!("\tmovl\t%eax, %ecx"));
        self.load_eax_reg(out, &d1);
        // out.push(format!("\tpop\t%ecx"));
      }
    }
  }

  fn get_factor_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    self.get_generic_asm(out_vec)
  }

  fn get_unary_op_asm(&self, out_vec: &mut Vec<String>) -> Vec<String> {
    let mut data = vec![];
    for child in self.children.iter() {
      let mut result = child.get_factor_asm(out_vec);
      data.append(&mut result);
    }
    self.load_eax_reg(out_vec, data.get(0).unwrap());
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
    data
  }

  fn get_integer_asm(&self, _: &mut Vec<String>) -> Vec<String> {
    self.data.clone()
  }

  fn get_function_for_node_type(
    &self,
    nt: &NodeType,
    is_asm: bool,
  ) -> fn(&Node<String>, &mut Vec<String>) -> Vec<String> {
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
  // Program,
  Function,
  ReturnStatement,
  ExpressionStatement,
  AssignmentStatement,
  AssignmentExpression,
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
    let mut out_vec = vec![];
    self.add_function_log(&mut out_vec);
    write!(f, "\n{}", out_vec.join("\n"))
  }
}
