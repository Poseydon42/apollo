use crate::ast::*;
use super::function_context::FunctionContext;

#[derive(Debug)]
pub struct ExprResolver<'f> {
    function: &'f FunctionContext,
}

impl<'f> ExprResolver<'f> {
    pub fn new(function: &'f FunctionContext) -> Self {
        Self {
            function,
        }
    }
}

impl<'ast> MutVisitor<'ast, Option<ResolvedType>> for ExprResolver<'ast> {
    fn visit_integer_literal_expr(&mut self, expr: &'ast mut IntegerLiteralExpr) -> Option<ResolvedType> {
        expr.ty = Some(ResolvedType::BuiltIn(BuiltInType::Int));
        Some(ResolvedType::BuiltIn(BuiltInType::Int))
    }

    fn visit_variable_reference_expr(&mut self, expr: &'ast mut VariableReferenceExpr) -> Option<ResolvedType> {
       if let Some(ty) = self.function.get_variable(&expr.name.text()) {
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
}
