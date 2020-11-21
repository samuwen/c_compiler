use crate::Tree;
use log::*;
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

  pub fn get_type(&self) -> &NodeType {
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
    let sep = get_separator();
    out_vec.push(format!("{}pushl\t%ebp", sep));
    out_vec.push(format!("{}movl\t%esp, %ebp", sep));
    let has_return = self.has_return_statement();
    // C supports int function with no return statement. In that event, they return 0
    match has_return {
      true => self.generate_block(out_vec, None),
      false => {
        out_vec.push(format!("{}movl\t$0, %eax", sep));
      }
    }
    let sep = get_separator();
    out_vec.push(format!("end:"));
    out_vec.push(format!("{}movl\t%ebp, %esp", sep));
    out_vec.push(format!("{}pop\t%ebp", sep));
    out_vec.push(format!("{}ret", get_separator()));
  }

  fn generate_block(&mut self, out_vec: &mut Vec<String>, scope: Option<Scope>) {
    let map = match scope {
      Some(map) => Scope::from(map),
      None => Scope::new(),
    };
    trace!("{:?}", map);
    self.generate_block_item_asm(out_vec, map);
  }

  fn generate_block_item_asm(&mut self, out_vec: &mut Vec<String>, scope: Scope) {
    let mut map = scope;
    for child in self.children.iter_mut() {
      match child.is_statement() {
        true => {
          child.generate_statement_asm(out_vec, &map);
        }
        false => {
          map = child.generate_declaration_asm(out_vec, map);
        }
      }
    }
    // kinda hacky but deallocate memory BEFORE the jmp
    let index = match out_vec.get(out_vec.len() - 1).unwrap().contains("jmp end") {
      true => out_vec.len() - 2,
      false => out_vec.len() - 1,
    };
    let b_to_dealloc = 4 * map.vec.len();
    out_vec.insert(
      index,
      format!("{}addl\t${}, %esp", get_separator(), b_to_dealloc),
    );
  }

  fn has_return_statement(&self) -> bool {
    self.children.iter().any(|c| {
      if c.get_type() == &NodeType::ReturnStatement {
        return true;
      } else {
        return c.has_return_statement();
      }
    })
  }

  fn generate_statement_asm(&mut self, out_vec: &mut Vec<String>, scope: &Scope) {
    match self.get_type() {
      NodeType::ReturnStatement => {
        self.handle_statement_children(out_vec, scope);
        out_vec.push(format!("{}jmp end", get_separator()));
      }
      NodeType::ExpressionStatement => {
        self.handle_statement_children(out_vec, scope);
      }
      NodeType::IfStatement => self.generate_if_statement_asm(out_vec, scope),
      NodeType::CompoundStatement => self.generate_block(out_vec, Some(scope.clone())),
      NodeType::NullStatement => (),
      NodeType::WhileStatement => self.generate_while_statement_asm(out_vec, scope),
      NodeType::DoStatement => self.generate_do_statement_asm(out_vec, scope),
      NodeType::ForStatement => self.generate_for_statement_asm(out_vec, scope),
      NodeType::ForDeclStatement => self.generate_for_decl_statement_asm(out_vec, scope),
      _ => panic!("Unexpected node type: {:?}", self.get_type()),
    }
  }

  fn generate_declaration_asm(&mut self, out_vec: &mut Vec<String>, scope: Scope) -> Scope {
    let mut map = scope;
    let sep = get_separator();
    let var_name = self.data.remove(0);
    match self.children.len() {
      0 => out_vec.push(format!("{}movl\t$0, %eax", sep)),
      _ => self.handle_statement_children(out_vec, &map),
    }
    out_vec.push(format!("{}pushl\t%eax", sep));
    map.add_var(var_name.to_owned());
    trace!("{:?}", map);
    map
  }

  fn generate_if_statement_asm(&mut self, out_vec: &mut Vec<String>, scope: &Scope) {
    let start_label = get_next_label();
    let end_label = get_next_label();
    let sep = get_separator();
    let mut condition = self.children.remove(0);
    match condition.get_type() {
      NodeType::BinaryOp => condition.generate_binary_op_asm(out_vec, scope),
      NodeType::UnaryOp => condition.generate_unary_op_asm(out_vec, scope),
      NodeType::Integer => condition.generate_integer_asm(out_vec),
      NodeType::Variable => condition.generate_variable_asm(out_vec, scope),
      _ => panic!(
        "Unexpected condition type for if statement: {:?}",
        condition.get_type()
      ),
    }
    out_vec.push(format!("{}cmpl\t$0, %eax", sep));
    out_vec.push(format!("{}je\t{}", sep, start_label));
    let mut true_block = self.children.remove(0);
    true_block.generate_block(out_vec, Some(scope.clone()));
    out_vec.push(format!("{}jmp\t{}", sep, end_label));
    out_vec.push(format!("{}:", start_label));
    if self.children.len() > 0 {
      let mut false_block = self.children.remove(0);
      false_block.generate_block(out_vec, Some(scope.clone()));
    }
    out_vec.push(format!("{}:", end_label));
  }

  fn generate_while_statement_asm(&mut self, out_vec: &mut Vec<String>, scope: &Scope) {
    let start_label = get_next_label();
    let end_label = get_next_label();
    let sep = get_separator();
    out_vec.push(format!("{}:", start_label));
    let mut expression = self.children.remove(0);
    match expression.get_type() {
      NodeType::BinaryOp => expression.generate_binary_op_asm(out_vec, scope),
      NodeType::Integer => expression.generate_integer_asm(out_vec),
      NodeType::Variable => expression.generate_variable_asm(out_vec, scope),
      _ => panic!(
        "Unexpected expression type for while statement: {:?}",
        expression.get_type()
      ),
    }
    out_vec.push(format!("{}cmpl\t$0, %eax", sep));
    out_vec.push(format!("{}je\t{}", sep, end_label));
    let mut statement = self.children.remove(0);
    statement.generate_statement_asm(out_vec, scope);
    out_vec.push(format!("{}jmp\t{}", sep, start_label));
    out_vec.push(format!("{}:", end_label));
  }

  fn generate_do_statement_asm(&mut self, out_vec: &mut Vec<String>, scope: &Scope) {
    let start_label = get_next_label();
    let end_label = get_next_label();
    let sep = get_separator();
    out_vec.push(format!("{}:", start_label));
    let mut statement = self.children.remove(0);
    statement.generate_statement_asm(out_vec, scope);
    let mut expression = self.children.remove(0);
    match expression.get_type() {
      NodeType::BinaryOp => expression.generate_binary_op_asm(out_vec, scope),
      NodeType::Integer => expression.generate_integer_asm(out_vec),
      NodeType::Variable => expression.generate_variable_asm(out_vec, scope),
      _ => panic!(
        "Unexpected expression type for while statement: {:?}",
        expression.get_type()
      ),
    }
    out_vec.push(format!("{}cmpl\t$0, %eax", sep));
    out_vec.push(format!("{}je\t{}", sep, end_label));
    out_vec.push(format!("{}jmp\t{}", sep, start_label));
    out_vec.push(format!("{}:", end_label));
  }

  fn generate_for_statement_asm(&mut self, out_vec: &mut Vec<String>, scope: &Scope) {
    let start_label = get_next_label();
    let end_label = get_next_label();
    let sep = get_separator();
    let mut init = self.children.remove(0);
    init.generate_statement_asm(out_vec, scope);
    let mut condition = self.children.remove(0);
    out_vec.push(format!("{}:", start_label));
    condition.generate_statement_asm(out_vec, scope);
    out_vec.push(format!("{}cmpl\t$0, %eax", sep));
    out_vec.push(format!("{}je\t{}", sep, end_label));
    let mut post_expression = self.children.remove(0);
    let mut statement = self.children.remove(0);
    statement.generate_statement_asm(out_vec, scope);
    post_expression.generate_statement_asm(out_vec, scope);
    let b_to_dealloc = 4 * scope.vec.len();
    out_vec.push(format!("{}addl\t${}, %esp", get_separator(), b_to_dealloc));
    out_vec.push(format!("{}jmp\t{}", sep, start_label));
    out_vec.push(format!("{}:", end_label));
  }

  fn generate_for_decl_statement_asm(&mut self, out_vec: &mut Vec<String>, scope: &Scope) {
    let start_label = get_next_label();
    let end_label = get_next_label();
    let sep = get_separator();
    let mut init = self.children.remove(0);
    init.generate_block_item_asm(out_vec, scope.clone());
    let mut condition = self.children.remove(0);
    out_vec.push(format!("{}:", start_label));
    condition.generate_statement_asm(out_vec, &scope);
    out_vec.push(format!("{}cmpl\t$0, %eax", sep));
    out_vec.push(format!("{}je\t{}", sep, end_label));
    let mut post_expression = self.children.remove(0);
    let mut statement = self.children.remove(0);
    statement.generate_statement_asm(out_vec, &scope);
    post_expression.generate_statement_asm(out_vec, &scope);
    out_vec.push(format!("{}jmp\t{}", sep, start_label));
    out_vec.push(format!("{}:", end_label));
  }

  fn handle_statement_children(&mut self, out_vec: &mut Vec<String>, scope: &Scope) {
    for child in self.children.iter_mut() {
      match child.get_type() {
        NodeType::Integer => child.generate_integer_asm(out_vec),
        NodeType::UnaryOp => child.generate_unary_op_asm(out_vec, scope),
        NodeType::BinaryOp => child.generate_binary_op_asm(out_vec, scope),
        NodeType::Assignment => child.generate_assignment_asm(out_vec, scope),
        NodeType::Variable => child.generate_variable_asm(out_vec, scope),
        NodeType::Conditional => child.generate_conditional_asm(out_vec, scope),
        _ => panic!("Unexpected node type: {:?}", child.get_type()),
      }
    }
  }

  fn generate_unary_op_asm(&mut self, out_vec: &mut Vec<String>, scope: &Scope) {
    for child in self.children.iter_mut() {
      match child.get_type() {
        NodeType::Integer => child.generate_integer_asm(out_vec),
        NodeType::UnaryOp => child.generate_unary_op_asm(out_vec, scope),
        NodeType::Variable => child.generate_variable_asm(out_vec, scope),
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
      "++" => {
        out_vec.push(format!("{}movl\t$1, %ecx", sep));
        out_vec.push(format!("{}addl\t%ecx, %eax", sep));
      }
      "--" => {
        out_vec.push(format!("{}movl\t%eax, %ecx", sep));
        out_vec.push(format!("{}movl\t$1, %eax", sep));
        out_vec.push(format!("{}subl\t%eax, %ecx", sep));
        out_vec.push(format!("{}movl\t%ecx, %eax", sep));
      }
      _ => panic!("Unexpected operation: {}", operator),
    }
  }

  fn generate_binary_op_asm(&mut self, out_vec: &mut Vec<String>, scope: &Scope) {
    let sep = get_separator();
    let child1 = self.children.remove(0);
    let child2 = self.children.remove(0);
    let operator = self.data.remove(0);
    match operator.as_str() {
      "+" => {
        self.generate_standard_op_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("{}addl\t%ecx, %eax", sep));
      }
      "*" => {
        self.generate_standard_op_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("{}imul\t%ecx, %eax", sep));
      }
      "-" => {
        self.generate_standard_op_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("{}subl\t%eax, %ecx", sep));
        out_vec.push(format!("{}movl\t%ecx, %eax", sep));
      }
      "/" => {
        self.generate_reverse_op_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("{}cdq", sep));
        out_vec.push(format!("{}idivl\t%ecx", sep));
      }
      "==" => {
        self.generate_relational_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("{}sete\t%al", sep));
      }
      "!=" => {
        self.generate_relational_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("{}setne\t%al", sep));
      }
      "<" => {
        self.generate_relational_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("{}setl\t%al", sep));
      }
      "<=" => {
        self.generate_relational_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("{}setle\t%al", sep));
      }
      ">" => {
        self.generate_relational_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("{}setg\t%al", sep));
      }
      ">=" => {
        self.generate_relational_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("{}setge\t%al", sep));
      }
      "&&" => {
        let start_label = get_next_label();
        let end_label = get_next_label();
        self.generate_child_asm(child1, out_vec, scope);
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}jne\t{}", sep, start_label));
        out_vec.push(format!("{}jmp\t{}", sep, end_label));
        out_vec.push(format!("{}:", start_label));
        self.generate_child_asm(child2, out_vec, scope);
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}movl\t$0, %eax", sep));
        out_vec.push(format!("{}setne\t%al", sep));
        out_vec.push(format!("{}:", end_label));
      }
      "||" => {
        let start_label = get_next_label();
        let end_label = get_next_label();
        self.generate_child_asm(child1, out_vec, scope);
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}je\t{}", sep, start_label));
        out_vec.push(format!("{}movl\t$1, %eax", sep));
        out_vec.push(format!("{}jmp\t{}", sep, end_label));
        out_vec.push(format!("{}:", start_label));
        self.generate_child_asm(child2, out_vec, scope);
        out_vec.push(format!("{}cmpl\t$0, %eax", sep));
        out_vec.push(format!("{}movl\t$0, %eax", sep));
        out_vec.push(format!("{}setne\t%al", sep));
        out_vec.push(format!("{}:", end_label));
      }
      "%" => {
        self.generate_reverse_op_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("\tcdq"));
        out_vec.push(format!("\tidivl\t%ecx"));
        out_vec.push(format!("\tmovl\t%edx, %eax"));
      }
      "&" => {
        self.generate_standard_op_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("\tand\t%ecx, %eax"));
      }
      "|" => {
        self.generate_standard_op_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("\tor\t%ecx, %eax"));
      }
      "^" => {
        self.generate_standard_op_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("\txor\t%ecx, %eax"));
      }
      "<<" => {
        self.generate_reverse_op_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("\tsall\t%cl, %eax"));
      }
      ">>" => {
        self.generate_reverse_op_setup(child1, child2, out_vec, scope);
        out_vec.push(format!("\tsarl\t%cl, %eax"));
      }
      "," => {
        out_vec.push(format!(
          "{}movl\t${}, %eax",
          sep,
          child2.get_data().get(0).unwrap()
        ));
      }
      _ => panic!("Unexpected operation: {}", operator),
    }
  }

  fn generate_integer_asm(&mut self, out_vec: &mut Vec<String>) {
    let sep = get_separator();
    out_vec.push(format!("{}movl\t${}, %eax", sep, self.data.get(0).unwrap()));
  }

  fn generate_assignment_asm(&mut self, out_vec: &mut Vec<String>, scope: &Scope) {
    let var_name = self.data.remove(0);
    for child in self.children.iter_mut() {
      match child.get_type() {
        NodeType::BinaryOp => child.generate_binary_op_asm(out_vec, scope),
        NodeType::Integer => child.generate_integer_asm(out_vec),
        NodeType::Assignment => child.generate_assignment_asm(out_vec, scope),
        NodeType::Variable => child.generate_variable_asm(out_vec, scope),
        NodeType::UnaryOp => child.generate_unary_op_asm(out_vec, scope),
        NodeType::Conditional => child.generate_conditional_asm(out_vec, scope),
        _ => panic!("Unexpected node type: {:?}", child.get_type()),
      };
      let offset = scope.get_var(&var_name);
      out_vec.push(format!("{}movl\t%eax, {}(%ebp)", get_separator(), offset));
    }
  }

  fn generate_variable_asm(&mut self, out_vec: &mut Vec<String>, scope: &Scope) {
    let var_name = self.data.remove(0);
    let offset = scope.get_var(&var_name);
    out_vec.push(format!("{}movl\t{}(%ebp), %eax", get_separator(), offset));
  }

  fn generate_conditional_asm(&mut self, out_vec: &mut Vec<String>, scope: &Scope) {
    let start_label = get_next_label();
    let end_label = get_next_label();
    let sep = get_separator();
    let mut child1 = self.children.remove(0);
    match child1.get_type() {
      NodeType::BinaryOp => child1.generate_binary_op_asm(out_vec, scope),
      NodeType::Integer => child1.generate_integer_asm(out_vec),
      NodeType::Variable => child1.generate_variable_asm(out_vec, scope),
      _ => panic!("Expected BinOp or Int, got {:?}", child1.get_type()),
    }
    out_vec.push(format!("{}cmpl\t$0, %eax", sep));
    out_vec.push(format!("{}je\t{}", sep, start_label));
    let mut child2 = self.children.remove(0);
    match child2.get_type() {
      NodeType::BinaryOp => child2.generate_binary_op_asm(out_vec, scope),
      NodeType::Integer => child2.generate_integer_asm(out_vec),
      NodeType::Conditional => child2.generate_conditional_asm(out_vec, scope),
      NodeType::Assignment => child2.generate_assignment_asm(out_vec, scope),
      _ => panic!(
        "Expected BinOp, Int, or Conditional, got {:?}",
        child2.get_type()
      ),
    }
    out_vec.push(format!("{}jmp\t{}", sep, end_label));
    out_vec.push(format!("{}:", start_label));
    let mut child3 = self.children.remove(0);
    match child3.get_type() {
      NodeType::Integer => child3.generate_integer_asm(out_vec),
      NodeType::Conditional => child3.generate_conditional_asm(out_vec, scope),
      NodeType::Assignment => child3.generate_assignment_asm(out_vec, scope),
      _ => panic!("Expected Int, got {:?}", child3.get_type()),
    }
    out_vec.push(format!("{}:", end_label));
  }

  fn generate_child_asm(&self, mut child: Node<String>, out_vec: &mut Vec<String>, scope: &Scope) {
    match child.get_type() {
      NodeType::Integer => child.generate_integer_asm(out_vec),
      NodeType::BinaryOp => child.generate_binary_op_asm(out_vec, scope),
      NodeType::UnaryOp => child.generate_unary_op_asm(out_vec, scope),
      NodeType::Variable => child.generate_variable_asm(out_vec, scope),
      _ => panic!("Unexpected node type: {:?}", child.get_type()),
    };
  }

  fn generate_standard_op_setup(
    &self,
    c1: Node<String>,
    c2: Node<String>,
    out_vec: &mut Vec<String>,
    scope: &Scope,
  ) {
    let sep = get_separator();
    self.generate_child_asm(c1, out_vec, scope);
    out_vec.push(format!("{}pushl\t%eax", sep));
    self.generate_child_asm(c2, out_vec, scope);
    out_vec.push(format!("{}pop\t%ecx", sep));
  }

  fn generate_reverse_op_setup(
    &self,
    c1: Node<String>,
    c2: Node<String>,
    out_vec: &mut Vec<String>,
    scope: &Scope,
  ) {
    let sep = get_separator();
    self.generate_child_asm(c2, out_vec, scope);
    out_vec.push(format!("{}pushl\t%eax", sep));
    self.generate_child_asm(c1, out_vec, scope);
    out_vec.push(format!("{}pop\t%ecx", sep));
  }

  fn generate_relational_setup(
    &self,
    c1: Node<String>,
    c2: Node<String>,
    out_vec: &mut Vec<String>,
    scope: &Scope,
  ) {
    let sep = get_separator();
    self.generate_standard_op_setup(c1, c2, out_vec, scope);
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

  fn is_statement(&self) -> bool {
    match self.get_type() {
      NodeType::IfStatement
      | NodeType::ReturnStatement
      | NodeType::ExpressionStatement
      | NodeType::CompoundStatement
      | NodeType::NullStatement
      | NodeType::WhileStatement
      | NodeType::DoStatement
      | NodeType::ForStatement
      | NodeType::ForDeclStatement => true,
      _ => false,
    }
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
  Declaration,
  ReturnStatement,
  ExpressionStatement,
  IfStatement,
  CompoundStatement,
  NullStatement,
  ForStatement,
  ForDeclStatement,
  DoStatement,
  WhileStatement,
  BreakStatement,
  ContinueStatement,
  Integer,
  UnaryOp,
  BinaryOp,
  Variable,
  Assignment,
  Conditional,
}

#[derive(Debug, Clone)]
struct Scope {
  map: HashMap<String, isize>,
  vec: Vec<String>,
  stack_index: isize,
  start_label: String,
  end_label: String,
}

impl Scope {
  fn new() -> Scope {
    trace!("Creating new map");
    Scope {
      map: HashMap::new(),
      vec: Vec::with_capacity(8),
      stack_index: -4,
      start_label: get_next_label(),
      end_label: get_next_label(),
    }
  }

  fn from(scope: Scope) -> Scope {
    trace!("Cloning map");
    Scope {
      map: scope.map,
      vec: Vec::with_capacity(8),
      stack_index: scope.stack_index,
      start_label: get_next_label(),
      end_label: get_next_label(),
    }
  }

  fn add_var(&mut self, name: String) {
    trace!("Attempting to add variable '{}' to map", name);
    match self.vec.contains(&name) {
      false => {
        self.map.insert(name.to_owned(), self.stack_index);
        self.vec.push(name);
        self.stack_index -= 4;
      }
      true => panic!("Variable {} already declared in this scope", name),
    };
  }

  fn get_var(&self, name: &String) -> isize {
    self
      .map
      .get(name)
      .expect(&format!("Variable '{}' accessed before declaration", name))
      .clone()
  }

  fn get_start_label(&self) -> &String {
    &self.start_label
  }

  fn get_end_label(&self) -> &String {
    &self.end_label
  }
}
