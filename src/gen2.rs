use crate::Tree;
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::vec::Vec;

static WS_COUNT: AtomicUsize = AtomicUsize::new(0);
static COUNT: AtomicUsize = AtomicUsize::new(0);

fn get_next_label() -> String {
  format!("label{}", COUNT.fetch_add(1, Ordering::Relaxed))
}

fn get_separator() -> String {
  let mut ret_string = String::new();
  for _ in 0..WS_COUNT.load(Ordering::Relaxed) {
    ret_string.push_str("  ");
  }
  ret_string
}

pub fn generate(mut tree: Tree) -> String {
  let mut out_vec = vec![];
  tree.generate_asm(&mut out_vec);
  out_vec.join("\n")
}

pub struct Node<String> {
  _type: NodeType,
  data: Vec<String>,
  children: Vec<Node<String>>,
}

impl Node<String> {
  pub fn new(_type: NodeType) -> Node<String> {
    Node {
      _type: _type,
      data: vec![],
      children: vec![],
    }
  }

  pub fn add_data(&mut self, data: String) {
    self.data.push(data);
  }

  pub fn add_child(&mut self, child: Node<String>) {
    self.children.push(child);
  }

  pub fn add_children(&mut self, mut children: Vec<Node<String>>) {
    self.children.append(&mut children);
  }

  fn get_type(&self) -> &NodeType {
    &self._type
  }

  pub fn generate_asm(&mut self, out_vec: &mut Vec<String>) {
    self.generate_program_asm(out_vec);
  }

  pub fn generate_program_asm(&mut self, out_vec: &mut Vec<String>) {
    for child in self.children.iter_mut() {
      child.generate_function_asm(out_vec);
    }
  }

  pub fn generate_function_asm(&mut self, out_vec: &mut Vec<String>) {
    let name = self.data.get(0).unwrap();
    out_vec.push(format!(".globl {}", name));
    out_vec.push(format!("{}:", name));
    WS_COUNT.fetch_add(1, Ordering::Relaxed);
    for statement in self.children.iter_mut() {
      statement.generate_statement_asm(out_vec);
    }
  }

  pub fn generate_statement_asm(&mut self, out_vec: &mut Vec<String>) {
    for child in self.children.iter_mut() {
      match child.get_type() {
        NodeType::Integer => child.generate_integer_asm(out_vec),
        NodeType::UnaryOp => child.generate_unary_op_asm(out_vec),
        NodeType::BinaryOp => child.generate_binary_op_asm(out_vec),
        _ => panic!("Unexpected node type: {:?}", child.get_type()),
      }
    }
    out_vec.push(format!("{}ret", get_separator()));
  }

  fn generate_unary_op_asm(&mut self, out_vec: &mut Vec<String>) {
    for child in self.children.iter_mut() {
      match child.get_type() {
        NodeType::Integer => child.generate_integer_asm(out_vec),
        NodeType::UnaryOp => child.generate_unary_op_asm(out_vec),
        _ => panic!("Unexpected node type: {:?}", child.get_type()),
      }
    }
    let sep = get_separator();
    let operator = self.data.get(0).unwrap();
    match operator.as_str() {
      "~" => out_vec.push(format!("{}not\t%eax", sep)),
      "-" => out_vec.push(format!("{}neg\t%eax", sep)),
      "!" => {
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}movl\t$0, %eax", sep));
        out_vec.push(format!("{}sete\t%al", sep));
      }
      _ => panic!("Unexpected operation: {}", operator),
    }
  }

  pub fn generate_binary_op_asm(&mut self, out_vec: &mut Vec<String>) {
    let sep = get_separator();
    let child1 = self.children.remove(0);
    let child2 = self.children.remove(0);
    let operator = self.data.remove(0);
    match operator.as_str() {
      "+" => {
        self.generate_standard_op_setup(child1, child2, out_vec);
        out_vec.push(format!("{}addl\t%ecx, %eax", sep));
      }
      "*" => {
        self.generate_standard_op_setup(child1, child2, out_vec);
        out_vec.push(format!("{}imul\t%ecx, %eax", sep));
      }
      "-" => {
        self.generate_standard_op_setup(child1, child2, out_vec);
        out_vec.push(format!("{}subl\t%eax, %ecx", sep));
        out_vec.push(format!("{}movl\t%ecx, %eax", sep));
      }
      "/" => {
        self.generate_child_asm(child2, out_vec);
        out_vec.push(format!("{}push\t%eax", sep));
        self.generate_child_asm(child1, out_vec);
        out_vec.push(format!("{}pop\t%ecx", sep));
        out_vec.push(format!("{}cdq", sep));
        out_vec.push(format!("{}idivl\t%ecx", sep));
      }
      "==" => {
        self.generate_relational_setup(child1, child2, out_vec);
        out_vec.push(format!("{}sete\t%al", sep));
      }
      "!=" => {
        self.generate_relational_setup(child1, child2, out_vec);
        out_vec.push(format!("{}setne\t%al", sep));
      }
      "<" => {
        self.generate_relational_setup(child1, child2, out_vec);
        out_vec.push(format!("{}setl\t%al", sep));
      }
      "<=" => {
        self.generate_relational_setup(child1, child2, out_vec);
        out_vec.push(format!("{}setle\t%al", sep));
      }
      ">" => {
        self.generate_relational_setup(child1, child2, out_vec);
        out_vec.push(format!("{}setg\t%al", sep));
      }
      ">=" => {
        self.generate_relational_setup(child1, child2, out_vec);
        out_vec.push(format!("{}setge\t%al", sep));
      }
      "&&" => {
        let label1 = get_next_label();
        let label2 = get_next_label();
        self.generate_child_asm(child1, out_vec);
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}jne\t{}", sep, label1));
        out_vec.push(format!("{}jmp\t{}", sep, label2));
        out_vec.push(format!("{}:", label1));
        self.generate_child_asm(child2, out_vec);
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}movl\t$0, %eax", sep));
        out_vec.push(format!("{}setne\t%al", sep));
        out_vec.push(format!("{}:", label2));
      }
      "||" => {
        let label1 = get_next_label();
        let label2 = get_next_label();
        self.generate_child_asm(child1, out_vec);
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}je\t{}", sep, label1));
        out_vec.push(format!("{}movl\t$1, %eax", sep));
        out_vec.push(format!("{}jmp\t{}", sep, label2));
        out_vec.push(format!("{}:", label1));
        self.generate_child_asm(child2, out_vec);
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}movl\t$0, %eax", sep));
        out_vec.push(format!("{}setne\t%al", sep));
        out_vec.push(format!("{}:", label2));
      }
      _ => panic!("Unexpected operation: {}", operator),
    }
  }

  pub fn generate_integer_asm(&mut self, out_vec: &mut Vec<String>) {
    let sep = get_separator();
    out_vec.push(format!("{}movl\t${}, %eax", sep, self.data.get(0).unwrap()));
  }

  pub fn generate_child_asm(&self, mut child: Node<String>, out_vec: &mut Vec<String>) {
    match child.get_type() {
      NodeType::Integer => child.generate_integer_asm(out_vec),
      NodeType::BinaryOp => child.generate_binary_op_asm(out_vec),
      _ => panic!("Unexpected node type: {:?}", child.get_type()),
    };
  }

  fn generate_standard_op_setup(
    &self,
    c1: Node<String>,
    c2: Node<String>,
    out_vec: &mut Vec<String>,
  ) {
    let sep = get_separator();
    self.generate_child_asm(c1, out_vec);
    out_vec.push(format!("{}push\t%eax", sep));
    self.generate_child_asm(c2, out_vec);
    out_vec.push(format!("{}pop\t%ecx", sep));
  }

  fn generate_relational_setup(
    &self,
    c1: Node<String>,
    c2: Node<String>,
    out_vec: &mut Vec<String>,
  ) {
    let sep = get_separator();
    self.generate_standard_op_setup(c1, c2, out_vec);
    out_vec.push(format!("{}cmpl\t%eax, %ecx", sep));
    out_vec.push(format!("{}movl\t$0, %eax", sep));
  }

  fn format_self(&self, count: usize) -> String {
    let mut tabs = String::new();
    for _ in 0..count {
      tabs.push_str("  ");
    }
    let mut return_str = format!(
      "{}type: {:?} | data: {} | children:\n",
      tabs,
      self._type,
      self.data.join(", ")
    );
    for child in self.children.iter() {
      return_str.push_str(&child.format_self(count + 1));
    }
    return_str
  }
}

impl fmt::Display for Node<String> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.format_self(0))
  }
}

#[derive(Debug, PartialEq)]
pub enum NodeType {
  Program,
  Function,
  Statement,
  Integer,
  UnaryOp,
  BinaryOp,
}
