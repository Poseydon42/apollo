use crate::ast::*;
use super::function_context::FunctionContext;
use super::expr_resolver::ExprResolver;
use super::type_resolver::resolve_type;

pub struct TypeChecker {
    current_function: Option<FunctionContext>,
}

impl<'ast> TypeChecker {
    pub fn new() -> Self {
        Self {
            current_function: None,
        }
    }
    
    pub fn check(mut self, module: &'ast mut Module) -> bool {
        MutVisitor::visit_module(&mut self, module)
    }

    fn add_variable(&mut self, name: String, ty: ResolvedType) {
        if let Some(func_ctx) = &mut self.current_function {
            func_ctx.add_variable(name, ty);
        } else {
            panic!("Trying to add a variable outside of a function context");
        }
    }

    fn resolve_expr_type(&mut self, expr: &mut Expr) -> Option<ResolvedType> {
        if let Some(func_ctx) = &self.current_function {
            let mut resolver = ExprResolver::new(func_ctx);
            resolver.visit_expr(expr)
        } else {
            panic!("Trying to resolve expression type outside of a function context");
        }
    }
}

impl <'ast> MutVisitor<'ast, bool> for TypeChecker {
    fn visit_module(&mut self, module: &'ast mut Module) -> bool {
        module.decls.iter_mut().all(|decl| match &mut decl.data {
            DeclKind::Function(func) => self.visit_func_decl(func),
            DeclKind::Variable(..) => {
                panic!("Variable declarations outside of functions are not allowed.");
            }
        })
    }

    fn visit_func_decl(&mut self, func: &'ast mut FunctionDecl) -> bool {
        let Some(return_ty) = resolve_type(func.return_ty.span.text()) else { panic!("Could not resoplve return type of function {}", func.name.text()); };
        func.return_ty.resolved = Some(return_ty.clone());
        self.current_function = Some(FunctionContext::new(return_ty));
        
        let result = func.body.iter_mut().all(|stmt| self.visit_stmt(stmt));

        self.current_function = None;
        result
    }

    fn visit_variable_decl(&mut self, decl: &'ast mut VariableDecl) -> bool {
        let resolved_ty = match resolve_type(decl.ty.span.text()) {
            Some(ty) => ty,
            None => {
                // FIXME: add as a diagnostic message instead
                println!("Could not resolve type {} of variable {}", decl.ty.span.text(), decl.name.text());
                return false;
            }
        };
        
        let init_type = match self.resolve_expr_type(&mut decl.init) {
            Some(ty) => ty,
            None => {
                // FIXME: add as a diagnostic message instead
                println!("Could not determine the type of the initializer of variable {}", decl.name.text());
                return false;
            }
        };
        
        if init_type != resolved_ty {
            println!("Type mismatch for variable {}: expected {:?}, found {:?}", 
            decl.name.text(), resolved_ty, init_type);
            return false;
        }
        self.add_variable(decl.name.text().to_owned(), resolved_ty.clone());
        decl.ty.resolved = Some(resolved_ty);
        
        true
    }

    fn visit_decl_stmt(&mut self, decl: &'ast mut Decl) -> bool {
        match &mut decl.data {
            DeclKind::Function(_) => {
                panic!("Function declarations within function declarations are not allowed.");
            },
            DeclKind::Variable(variable) => self.visit_variable_decl(variable),
        }
    }

    fn visit_return_stmt(&mut self, value: &'ast mut Expr) -> bool {
        match self.resolve_expr_type(value) {
            Some(expr_ty) => self.current_function.as_ref().unwrap().return_type() == &expr_ty,
            None => {
                println!("Could not resolve return expression type");
                false
            }
        }
    }
}
