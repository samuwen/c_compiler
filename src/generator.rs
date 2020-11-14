use crate::Tree;
use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::vec::Vec;

static WS_COUNT: AtomicUsize = AtomicUsize::new(0);
static LB_COUNT: AtomicUsize = AtomicUsize::new(0);

fn get_next_label() -> String {
  format!("label{}", LB_COUNT.fetch_add(1, Ordering::Relaxed))
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

  pub fn get_data(&self) -> &Vec<String> {
    &self.data
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
    let mut map: HashMap<String, isize> = HashMap::new();
    let mut stack_index: isize = -4;
    let name = self.data.get(0).unwrap();
    out_vec.push(format!(".globl {}", name));
    out_vec.push(format!("{}:", name));
    WS_COUNT.fetch_add(1, Ordering::Relaxed);
    let sep = get_separator();
    out_vec.push(format!("{}push\t%ebp", sep));
    out_vec.push(format!("{}movl\t%esp, %ebp", sep));
    let has_return = self
      .children
      .iter()
      .any(|c| c.get_type() == &NodeType::ReturnStatement);
    match has_return {
      true => {
        for statement in self.children.iter_mut() {
          statement.generate_statement_asm(out_vec, &mut map, &mut stack_index);
        }
      }
      false => {
        out_vec.push(format!("{}movl\t$0, %eax", sep));
      }
    }
    let sep = get_separator();
    out_vec.push(format!("{}movl\t%ebp, %esp", sep));
    out_vec.push(format!("{}pop\t%ebp", sep));
    out_vec.push(format!("{}ret", get_separator()));
  }

  fn generate_statement_asm(
    &mut self,
    out_vec: &mut Vec<String>,
    var_map: &mut HashMap<String, isize>,
    stack_index: &mut isize,
  ) {
    let sep = get_separator();
    match self.get_type() {
      NodeType::ReturnStatement => {
        self.handle_statement_children(out_vec, var_map, stack_index);
      }
      NodeType::ExpressionStatement => {
        self.handle_statement_children(out_vec, var_map, stack_index);
      }
      NodeType::DeclareStatement => {
        let var_name = self.data.remove(0);
        if var_map.contains_key(&var_name) {
          panic!("Cannot declare variable {} twice", var_name);
        }
        match self.children.len() {
          0 => out_vec.push(format!("{}movl\t$0, %eax", sep)),
          _ => self.handle_statement_children(out_vec, var_map, stack_index),
        }
        out_vec.push(format!("{}pushl\t%eax", sep));
        var_map.insert(var_name, *stack_index);
        *stack_index -= 4;
      }
      _ => panic!("Unexpected node type: {:?}", self.get_type()),
    }
  }

  fn handle_statement_children(
    &mut self,
    out_vec: &mut Vec<String>,
    var_map: &mut HashMap<String, isize>,
    stack_index: &mut isize,
  ) {
    for child in self.children.iter_mut() {
      match child.get_type() {
        NodeType::Integer => child.generate_integer_asm(out_vec),
        NodeType::UnaryOp => child.generate_unary_op_asm(out_vec, var_map, stack_index),
        NodeType::BinaryOp => child.generate_binary_op_asm(out_vec, var_map, stack_index),
        NodeType::Assignment => child.generate_assignment_asm(out_vec, var_map, stack_index),
        NodeType::Variable => child.generate_variable_asm(out_vec, var_map, stack_index),
        _ => panic!("Unexpected node type: {:?}", child.get_type()),
      }
    }
  }

  fn generate_unary_op_asm(
    &mut self,
    out_vec: &mut Vec<String>,
    var_map: &mut HashMap<String, isize>,
    stack_index: &mut isize,
  ) {
    for child in self.children.iter_mut() {
      match child.get_type() {
        NodeType::Integer => child.generate_integer_asm(out_vec),
        NodeType::UnaryOp => child.generate_unary_op_asm(out_vec, var_map, stack_index),
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

  fn generate_binary_op_asm(
    &mut self,
    out_vec: &mut Vec<String>,
    var_map: &mut HashMap<String, isize>,
    stack_index: &mut isize,
  ) {
    let sep = get_separator();
    let child1 = self.children.remove(0);
    let child2 = self.children.remove(0);
    let operator = self.data.remove(0);
    match operator.as_str() {
      "+" => {
        self.generate_standard_op_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("{}addl\t%ecx, %eax", sep));
      }
      "*" => {
        self.generate_standard_op_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("{}imul\t%ecx, %eax", sep));
      }
      "-" => {
        self.generate_standard_op_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("{}subl\t%eax, %ecx", sep));
        out_vec.push(format!("{}movl\t%ecx, %eax", sep));
      }
      "/" => {
        self.generate_reverse_op_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("{}cdq", sep));
        out_vec.push(format!("{}idivl\t%ecx", sep));
      }
      "==" => {
        self.generate_relational_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("{}sete\t%al", sep));
      }
      "!=" => {
        self.generate_relational_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("{}setne\t%al", sep));
      }
      "<" => {
        self.generate_relational_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("{}setl\t%al", sep));
      }
      "<=" => {
        self.generate_relational_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("{}setle\t%al", sep));
      }
      ">" => {
        self.generate_relational_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("{}setg\t%al", sep));
      }
      ">=" => {
        self.generate_relational_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("{}setge\t%al", sep));
      }
      "&&" => {
        let label1 = get_next_label();
        let label2 = get_next_label();
        self.generate_child_asm(child1, out_vec, var_map, stack_index);
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}jne\t{}", sep, label1));
        out_vec.push(format!("{}jmp\t{}", sep, label2));
        out_vec.push(format!("{}:", label1));
        self.generate_child_asm(child2, out_vec, var_map, stack_index);
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}movl\t$0, %eax", sep));
        out_vec.push(format!("{}setne\t%al", sep));
        out_vec.push(format!("{}:", label2));
      }
      "||" => {
        let label1 = get_next_label();
        let label2 = get_next_label();
        self.generate_child_asm(child1, out_vec, var_map, stack_index);
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}je\t{}", sep, label1));
        out_vec.push(format!("{}movl\t$1, %eax", sep));
        out_vec.push(format!("{}jmp\t{}", sep, label2));
        out_vec.push(format!("{}:", label1));
        self.generate_child_asm(child2, out_vec, var_map, stack_index);
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}movl\t$0, %eax", sep));
        out_vec.push(format!("{}setne\t%al", sep));
        out_vec.push(format!("{}:", label2));
      }
      "%" => {
        self.generate_reverse_op_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("\tcdq"));
        out_vec.push(format!("\tidivl\t%ecx"));
        out_vec.push(format!("\tmovl\t%edx, %eax"));
      }
      "&" => {
        self.generate_standard_op_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("\tand\t%ecx, %eax"));
      }
      "|" => {
        self.generate_standard_op_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("\tor\t%ecx, %eax"));
      }
      "^" => {
        self.generate_standard_op_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("\txor\t%ecx, %eax"));
      }
      "<<" => {
        self.generate_reverse_op_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("\tsall\t%cl, %eax"));
      }
      ">>" => {
        self.generate_reverse_op_setup(child1, child2, out_vec, var_map, stack_index);
        out_vec.push(format!("\tsarl\t%cl, %eax"));
      }
      _ => panic!("Unexpected operation: {}", operator),
    }
  }

  fn generate_integer_asm(&mut self, out_vec: &mut Vec<String>) {
    let sep = get_separator();
    out_vec.push(format!("{}movl\t${}, %eax", sep, self.data.get(0).unwrap()));
  }

  fn generate_assignment_asm(
    &mut self,
    out_vec: &mut Vec<String>,
    var_map: &mut HashMap<String, isize>,
    stack_index: &mut isize,
  ) {
    let var_name = self.data.remove(0);
    for child in self.children.iter_mut() {
      match child.get_type() {
        NodeType::BinaryOp => child.generate_binary_op_asm(out_vec, var_map, stack_index),
        NodeType::Integer => child.generate_integer_asm(out_vec),
        NodeType::Assignment => child.generate_assignment_asm(out_vec, var_map, stack_index),
        NodeType::Variable => child.generate_variable_asm(out_vec, var_map, stack_index),
        _ => panic!("Unexpected node type: {:?}", child.get_type()),
      };
      let offset = var_map
        .get(&var_name)
        .expect("Variable accessed before declaration");
      out_vec.push(format!("{}movl\t%eax, {}(%ebp)", get_separator(), offset));
    }
  }

  fn generate_variable_asm(
    &mut self,
    out_vec: &mut Vec<String>,
    var_map: &mut HashMap<String, isize>,
    stack_index: &mut isize,
  ) {
    let var_name = self.data.remove(0);
    let offset = var_map
      .get(&var_name)
      .expect("Variable accessed before declaration");
    out_vec.push(format!("{}movl\t{}(%ebp), %eax", get_separator(), offset));
  }

  fn generate_child_asm(
    &self,
    mut child: Node<String>,
    out_vec: &mut Vec<String>,
    var_map: &mut HashMap<String, isize>,
    stack_index: &mut isize,
  ) {
    match child.get_type() {
      NodeType::Integer => child.generate_integer_asm(out_vec),
      NodeType::BinaryOp => child.generate_binary_op_asm(out_vec, var_map, stack_index),
      NodeType::UnaryOp => child.generate_unary_op_asm(out_vec, var_map, stack_index),
      NodeType::Variable => child.generate_variable_asm(out_vec, var_map, stack_index),
      _ => panic!("Unexpected node type: {:?}", child.get_type()),
    };
  }

  fn generate_standard_op_setup(
    &self,
    c1: Node<String>,
    c2: Node<String>,
    out_vec: &mut Vec<String>,
    var_map: &mut HashMap<String, isize>,
    stack_index: &mut isize,
  ) {
    let sep = get_separator();
    self.generate_child_asm(c1, out_vec, var_map, stack_index);
    out_vec.push(format!("{}push\t%eax", sep));
    self.generate_child_asm(c2, out_vec, var_map, stack_index);
    out_vec.push(format!("{}pop\t%ecx", sep));
  }

  fn generate_reverse_op_setup(
    &self,
    c1: Node<String>,
    c2: Node<String>,
    out_vec: &mut Vec<String>,
    var_map: &mut HashMap<String, isize>,
    stack_index: &mut isize,
  ) {
    let sep = get_separator();
    self.generate_child_asm(c2, out_vec, var_map, stack_index);
    out_vec.push(format!("{}push\t%eax", sep));
    self.generate_child_asm(c1, out_vec, var_map, stack_index);
    out_vec.push(format!("{}pop\t%ecx", sep));
  }

  fn generate_relational_setup(
    &self,
    c1: Node<String>,
    c2: Node<String>,
    out_vec: &mut Vec<String>,
    var_map: &mut HashMap<String, isize>,
    stack_index: &mut isize,
  ) {
    let sep = get_separator();
    self.generate_standard_op_setup(c1, c2, out_vec, var_map, stack_index);
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
  ReturnStatement,
  DeclareStatement,
  ExpressionStatement,
  Integer,
  UnaryOp,
  BinaryOp,
  Variable,
  Assignment,
}
