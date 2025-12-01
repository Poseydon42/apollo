use crate::ast::*;
use crate::create_diagnostic;
use crate::diagnostic::Reporter;
use super::diagnostics::*;
use super::function_context::FunctionContext;
use super::type_resolver::resolve_type;

pub struct TypeChecker<'reporter, R> {
    reporter: &'reporter mut R,
    current_function: Option<FunctionContext>,
}

impl<'ast, 'reporter, R: Reporter> TypeChecker<'reporter, R> {
    pub fn new(reporter: &'reporter mut R) -> Self {
        Self {
            reporter,
            current_function: None,
        }
    }
    
    pub fn check(mut self, module: &'ast mut Module) -> bool {
        MutVisitor::visit_module(&mut self, module).is_some()
    }

    fn add_variable(&mut self, name: String, ty: ResolvedType) {
        if let Some(func_ctx) = &mut self.current_function {
            func_ctx.add_variable(name, ty);
        } else {
            panic!("Trying to add a variable outside of a function context");
        }
    }
}

impl<'ast, 'reporter, R: Reporter> MutVisitor<'ast, Option<ResolvedType>> for TypeChecker<'reporter, R> {
    fn visit_module(&mut self, module: &'ast mut Module) -> Option<ResolvedType> {
        module.decls.iter_mut().all(|decl| match &mut decl.data {
            DeclKind::Function(func) => self.visit_func_decl(func),
            DeclKind::Variable(..) => {
                panic!("Variable declarations outside of functions are not allowed.");
            }
        }.is_some()).then(|| ResolvedType::Empty)
    }

    fn visit_func_decl(&mut self, func: &'ast mut FunctionDecl) -> Option<ResolvedType> {
        let Some(return_ty) = resolve_type(func.return_ty.span.text()) else { panic!("Could not resoplve return type of function {}", func.name.text()); };
        func.return_ty.resolved = Some(return_ty.clone());
        self.current_function = Some(FunctionContext::new(return_ty));
        
        match self.visit_expr(&mut func.body) {
            Some(ty) if &ty == self.current_function.as_ref().unwrap().return_type() => {},
            Some(ResolvedType::Empty) => {
                // A missing return expression is acceptable for functions with non-void return type if there's an explicit return statement
                if !self.current_function.as_ref().unwrap().explicit_return_guaranteed {
                    self.reporter.report(create_diagnostic!(MissingReturn, func.body.span.clone(), func.name.text().to_owned(), func.return_ty.span.text().to_owned()));
                    return None;
                }
            }
            Some(ty) => {
                println!("Return type mismatch in function {}: expected {:?}, found {:?}", 
                func.name.text(), self.current_function.as_ref().unwrap().return_type(), ty);
                return None;
            },
            None => {
                return None;
            }
        }

        self.current_function = None;

        // NOTE: technically function declarations *can* produce a type, and we probably can implement that in the future
        Some(ResolvedType::Empty)
    }

    fn visit_variable_decl(&mut self, decl: &'ast mut VariableDecl) -> Option<ResolvedType> {
        let resolved_ty = match resolve_type(decl.ty.span.text()) {
            Some(ty) => ty,
            None => {
                // FIXME: add as a diagnostic message instead
                println!("Could not resolve type {} of variable {}", decl.ty.span.text(), decl.name.text());
                return None;
            }
        };
        
        let init_type = match self.visit_expr(&mut decl.init) {
            Some(ty) => ty,
            None => {
                // FIXME: add as a diagnostic message instead
                println!("Could not determine the type of the initializer of variable {}", decl.name.text());
                return None;
            }
        };
        
        if init_type != resolved_ty {
            println!("Type mismatch for variable {}: expected {:?}, found {:?}", 
            decl.name.text(), resolved_ty, init_type);
            return None;
        }
        self.add_variable(decl.name.text().to_owned(), resolved_ty.clone());
        decl.ty.resolved = Some(resolved_ty.clone());
        
        // NOTE: variable declarations should evaluate to their value if used as an expression
        Some(resolved_ty)
    }

    fn visit_block(&mut self, block_expr: &'ast mut Block) -> Option<ResolvedType> {
        for expr in &mut block_expr.ignored_exprs {
            if self.visit_expr(expr).is_none() {
                return None;
            }
        }

        match &mut block_expr.last_expr {
            Some(last_expr) => self.visit_expr(last_expr),
            None => Some(ResolvedType::Empty),
        }
    }

    fn visit_integer_literal(&mut self, expr: &'ast mut IntegerLiteral) -> Option<ResolvedType> {
        expr.ty = Some(ResolvedType::BuiltIn(BuiltInType::Int));
        Some(ResolvedType::BuiltIn(BuiltInType::Int))
    }

    fn visit_variable_reference(&mut self, expr: &'ast mut VariableReference) -> Option<ResolvedType> {
        if self.current_function.is_none() {
            panic!("Variable reference outside of function context is not allowed");
        }
        if let Some(ty) = self.current_function.as_ref().unwrap().get_variable(&expr.name.text()) {
            expr.ty = Some(ty.clone());
            Some(ty.clone())
        } else {
            println!("Variable {} not found in current function context", expr.name.text());
            None
        }
    }

    fn visit_binary_expr(&mut self, expr: &'ast mut BinaryExpr) -> Option<ResolvedType> {
        let lhs_type = self.visit_expr(&mut expr.lhs);
        let rhs_type = self.visit_expr(&mut expr.rhs);

        if lhs_type.is_none() || rhs_type.is_none() {
            return None;
        }

        if lhs_type == rhs_type {
            Some(lhs_type.unwrap())
        } else {
            println!("Type mismatch in binary expression: {:?} and {:?}", lhs_type, rhs_type);
            None
        }
    }

    fn visit_return(&mut self, expr: &'ast mut Return) -> Option<ResolvedType> {
        if self.current_function.is_none() {
            panic!("Return expression outside of function context is not allowed");
        }
        self.current_function.as_mut().unwrap().explicit_return_guaranteed = true;
        
        // Return always evaluates to (), but it must match the function's return type
        let value_type = self.visit_expr(&mut expr.value)?;
        let expected_return_type = self.current_function.as_ref().unwrap().return_type();
        if &value_type != expected_return_type {
            println!("Return type mismatch: expected {:?}, found {:?}", expected_return_type, value_type);
            return None;
        }

        Some(ResolvedType::Empty)
    }
}
