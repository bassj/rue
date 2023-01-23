use std::collections::HashMap;

use inkwell::values::BasicValueEnum;

pub mod llvm;

pub struct RueScope<'ctx> {
    pub is_global: bool,
    pub parent_scope: Option<&'ctx RueScope<'ctx>>,
    scope_variables: HashMap<String, BasicValueEnum<'ctx>>
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
}