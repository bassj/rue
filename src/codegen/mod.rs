use std::{collections::HashMap, fmt::Pointer};

use inkwell::values::{BasicValueEnum, PointerValue};

pub mod llvm;

pub struct RueScope<'ctx> {
    pub is_global: bool,
    pub parent_scope: Option<&'ctx RueScope<'ctx>>,
    scope_variables: HashMap<String, PointerValue<'ctx>>,
}
impl<'ctx> RueScope<'ctx> {
    pub fn global() -> Self {
        Self {
            is_global: true,
            parent_scope: None,
            scope_variables: HashMap::new(),
        }
    }

    pub fn make_sub_scope(&'ctx self) -> Self {
        Self {
            is_global: false,
            parent_scope: Some(self),
            scope_variables: HashMap::new(),
        }
    }

    pub fn add_variable<S: Into<String>>(&mut self, var_name: S, var_value: PointerValue<'ctx>) {
        let var_name = var_name.into();
        self.scope_variables.insert(var_name, var_value);
    }

    pub fn find_variable<S: Into<String>>(&self, var_name: S) -> Option<&PointerValue<'ctx>> {
        let var_name = var_name.into();

        match self.scope_variables.get(&var_name) {
            Some(var_ptr) => Some(var_ptr),
            None => match self.parent_scope {
                None => None,
                Some(parent_scope) => parent_scope.find_variable(var_name),
            },
        }
    }
}
