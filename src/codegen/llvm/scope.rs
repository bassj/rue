use std::collections::HashMap;

use inkwell::values::PointerValue;

pub struct ScopeGraph<'ctx> {
    pub root_scope: Scope<'ctx>,
}

impl<'ctx> ScopeGraph<'ctx> {
    pub fn new() -> Self {
        let global_scope = Scope {
            parent_scope: None,
            sub_scopes: Vec::new(),
            scope_variables: HashMap::new(),
        };

        ScopeGraph {
            root_scope: global_scope,
        }
    }
}
pub struct Scope<'ctx> {
    parent_scope: Option<*const Scope<'ctx>>,
    sub_scopes: Vec<Scope<'ctx>>,
    scope_variables: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> Scope<'ctx> {
    pub fn new_sub_scope<'a>(&'a mut self) -> &'a mut Scope<'ctx> {
        let sub_scope = Scope {
            parent_scope: Some(self as *const Scope<'ctx>),
            sub_scopes: Vec::new(),
            scope_variables: HashMap::new(),
        };

        self.sub_scopes.push(sub_scope);
        self.sub_scopes.last_mut().unwrap()
    }

    pub fn find_variable(&self, variable_name: &str) -> Option<PointerValue<'ctx>> {
        if !self.scope_variables.contains_key(variable_name) {
            match self.parent_scope {
                Some(parent_scope) => unsafe {
                    return match parent_scope.as_ref() {
                        Some(parent_scope) => parent_scope.find_variable(variable_name),
                        None => None,
                    };
                },
                None => None,
            }
        } else {
            let pointer_value = self.scope_variables.get(variable_name).unwrap();
            Some(*pointer_value)
        }
    }

    pub fn add_variable(&mut self, variable_name: &str, value: PointerValue<'ctx>) {
        self.scope_variables
            .insert(variable_name.to_string(), value);
    }
}

#[cfg(test)]
mod tests {
    use llvm_sys_160::prelude::LLVMValueRef;
    use crate::codegen::llvm::scope::ScopeGraph;
    use inkwell::values::PointerValue;

    #[test]
    fn test_find_variable() {

        let mut scope_graph = ScopeGraph::new();

        let dummy_value = unsafe { PointerValue::new(1 as LLVMValueRef) };

        scope_graph.root_scope.add_variable("root_var", dummy_value);

        {
            let sub_scope_a = scope_graph.root_scope.new_sub_scope();
            sub_scope_a.add_variable("scope_a_var", dummy_value);
        }

        {
            let sub_scope_b = scope_graph.root_scope.new_sub_scope();
            sub_scope_b.add_variable("scope_b_var", dummy_value);
        }

        assert!(scope_graph.root_scope.find_variable("root_var").is_some());
        assert!(scope_graph.root_scope.find_variable("scope_a_var").is_none());
        assert!(scope_graph.root_scope.find_variable("scope_b_var").is_none());

        {
            let scope_a = &scope_graph.root_scope.sub_scopes[0];

            assert!(scope_a.find_variable("root_var").is_some());
            assert!(scope_a.find_variable("scope_a_var").is_some());
            assert!(scope_a.find_variable("scope_b_var").is_none());
        }

        {
            let scope_a = &scope_graph.root_scope.sub_scopes[1];

            assert!(scope_a.find_variable("root_var").is_some());
            assert!(scope_a.find_variable("scope_a_var").is_none());
            assert!(scope_a.find_variable("scope_b_var").is_some());
        }
    }
}
