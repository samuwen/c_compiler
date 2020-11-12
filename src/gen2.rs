use crate::Tree;
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::vec::Vec;

static WS_COUNT: AtomicUsize = AtomicUsize::new(0);

fn get_separator() -> String {
  let mut ret_string = String::new();
  for _ in 0..WS_COUNT.load(Ordering::Relaxed) {
    ret_string.push_str("  ");
  }
  ret_string
}

pub fn generate(tree: Tree) -> String {
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

  pub fn generate_asm(&self, out_vec: &mut Vec<String>) {
    self.generate_program_asm(out_vec);
  }

  pub fn generate_program_asm(&self, out_vec: &mut Vec<String>) {
    for child in self.children.iter() {
      child.generate_function_asm(out_vec);
    }
  }

  pub fn generate_function_asm(&self, out_vec: &mut Vec<String>) {
    let name = self.data.get(1).unwrap();
    out_vec.push(format!(".globl {}", name));
    out_vec.push(format!("{}:", name));
    WS_COUNT.fetch_add(1, Ordering::Relaxed);
    for statement in self.children.iter() {
      statement.generate_statement_asm(out_vec);
    }
  }

  pub fn generate_statement_asm(&self, out_vec: &mut Vec<String>) {
    for child in self.children.iter() {
      child.generate_expression_asm(out_vec);
    }
    out_vec.push(format!("{}ret", get_separator()));
  }

  pub fn generate_expression_asm(&self, out_vec: &mut Vec<String>) {
    for child in self.children.iter() {
      child.generate_integer_asm(out_vec);
    }
  }

  pub fn generate_integer_asm(&self, out_vec: &mut Vec<String>) {
    let sep = get_separator();
    out_vec.push(format!(
      "{}movl{}${}, %eax",
      sep,
      sep,
      self.data.get(0).unwrap()
    ));
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

#[derive(Debug)]
pub enum NodeType {
  Program,
  Function,
  Statement,
  Expression,
  Integer,
}
