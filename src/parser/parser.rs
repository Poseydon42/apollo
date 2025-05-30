use crate::ast::*;
use crate::create_diagnostic;
use crate::diagnostic;
use crate::lexer::*;
use crate::span::Span;
use super::diagnostics::*;

pub struct Parser<'a, R> {
    lexems: Vec<Lexem>,
    cursor: usize,
    reporter: &'a mut R,
}

#[allow(dead_code)]
impl<'a, R: diagnostic::Reporter> Parser<'a, R> {
    pub fn new(lexems: Vec<Lexem>, reporter: &'a mut R) -> Self {
        Self {
            lexems,
            cursor: 0,
            reporter
        }
    }

    pub fn parse(&mut self) -> Module {
        let mut decls = vec![];

        while self.match_decl() {
            if let Some(decl) = self.parse_decl() {
                decls.push(decl)
            }
        }

        Module::new(decls)
    }

    fn peek(&self) -> Option<&Lexem> {
        self.lexems.get(self.cursor)
    }

    fn eat(&mut self, kind: LexemKind) -> Option<&Lexem> {
        if let Some(result) = self.lexems.get(self.cursor) {
            self.cursor += 1;
            if result.kind == kind {
                Some(result)
            } else {
                self.reporter.report(create_diagnostic!(UnexpectedLexem, self.eof_location(), kind, result.kind));
                None
            }
        } else {
            self.reporter.report(create_diagnostic!(UnexpectedEOF, self.eof_location(), kind));
            None
        }
    }

    fn advance(&mut self)  {
        self.cursor += 1;
    }

    fn eof_location(&self) -> Span {
        Span::empty_eof()
    }

    fn match_lexem(&self, kind: LexemKind) -> bool {
        self.peek().is_some_and(|l| l.kind == kind)
    }

    fn match_binary_op(&self) -> Option<BinaryOp> {
        match self.peek()?.kind {
            LexemKind::Plus => Some(BinaryOp::Add),
            LexemKind::Minus => Some(BinaryOp::Sub),
            _ => None
        }
    }

    // ----- MATCHERS -----
    fn match_decl(&self) -> bool {
        self.match_func_decl() | self.match_variable_decl()
    }

    fn match_func_decl(&self) -> bool {
        self.match_lexem(LexemKind::Fn)
    }

    fn match_variable_decl(&self) -> bool {
        self.match_lexem(LexemKind::Let)
    }

    fn match_ty(&self) -> bool {
        self.match_name_ty()
    }

    fn match_name_ty(&self) -> bool {
        self.match_lexem(LexemKind::Identifier)
    }

    fn match_expr(&self) -> bool {
        self.match_integer_literal_expr() |
        self.match_identifier_expr()
    }

    fn match_integer_literal_expr(&self) -> bool {
        self.match_lexem(LexemKind::IntegerLiteral)
    }

    fn match_identifier_expr(&self) -> bool {
        self.match_lexem(LexemKind::Identifier)
    }

    // ----- PARSERS -----
    fn parse_decl(&mut self) -> Option<Decl> {
        if self.match_func_decl() {
            self.parse_func_decl()
        } else {
            None
        }
    }

    fn parse_func_decl(&mut self) -> Option<Decl> {
        let start = self.eat(LexemKind::Fn)?.span.clone();

        let name = self.eat(LexemKind::Identifier)?.span.clone();

        self.eat(LexemKind::LeftParen)?;
        self.eat(LexemKind::RightParen)?;

        self.eat(LexemKind::Arrow)?;
        let return_ty = self.parse_ty()?;

        self.eat(LexemKind::LeftBrace)?;

        let mut body = vec![];
        while let Some(lexem) = self.peek() {
            if lexem.kind == LexemKind::RightBrace {
                break;
            }

            let stmt = self.parse_stmt()?;
            body.push(stmt);
        }

        let end = &self.eat(LexemKind::RightBrace)?.span;

        let func = FunctionDecl {
            name,
            return_ty,
            body
        };
        let span = start.merge_with(end);
        Some(Decl::new(DeclKind::Function(func), span))
    }

    fn parse_variable_decl(&mut self) -> Option<Decl> {
        let start = self.eat(LexemKind::Let)?.span.clone();

        let name = self.eat(LexemKind::Identifier)?.span.clone();

        self.eat(LexemKind::Colon)?;
        let ty = self.parse_ty()?;

        self.eat(LexemKind::Equals)?;
        let init = self.parse_expr()?;

        let end = &self.eat(LexemKind::Semicolon)?.span;

        let variable = VariableDecl {
            name,
            ty,
            init
        };
        let span = start.merge_with(end);
        Some(Decl::new(DeclKind::Variable(variable), span))
    }

    fn parse_ty(&mut self) -> Option<Type> {
        if self.match_name_ty() {
            self.parse_name_ty()
        } else {
            None
        }
    }

    fn parse_name_ty(&mut self) -> Option<Type> {
        let name = self.eat(LexemKind::Identifier)?;
        Some(Type::new(name.span.clone()))
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        let mut lhs = self.parse_primary_expr()?;
        while let Some(op) = self.match_binary_op() {
            self.advance();
            let rhs = self.parse_primary_expr()?; // FIXME: Pratt parsing
            let span = lhs.span.clone().merge_with(&rhs.span);
            lhs = Expr::new(ExprKind::Binary(BinaryExpr::new(lhs, op, rhs)), span);
        }
        Some(lhs)
    }

    fn parse_integer_literal_expr(&mut self) -> Option<Expr> {
        let literal = self.eat(LexemKind::IntegerLiteral)?;
        Some(Expr::new(ExprKind::IntegerLiteral(IntegerLiteralExpr::new(literal.span.clone())), literal.span.clone()))
    }

    fn parse_identifier_expr(&mut self) -> Option<Expr> {
        let identifier = self.eat(LexemKind::Identifier)?;
        Some(Expr::new(ExprKind::VariableReference(VariableReferenceExpr::new(identifier.span.clone())), identifier.span.clone()))
    }

    fn parse_primary_expr(&mut self) -> Option<Expr> {
        if self.match_integer_literal_expr() {
            self.parse_integer_literal_expr()
        } else if self.match_identifier_expr() {
            self.parse_identifier_expr()
        } else {
            None
        }
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        if self.match_lexem(LexemKind::Return) {
            self.parse_return_stmt()
        } else if self.match_variable_decl() {
            let decl = self.parse_variable_decl()?;
            let span = decl.span.clone();
            Some(Stmt::new(StmtKind::Decl(decl), span))
        } else {
            None
        }
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        let start = self.eat(LexemKind::Return)?.span.clone();
        let value = self.parse_expr()?;
        let end = self.eat(LexemKind::Semicolon)?;

        let span = start.merge_with(&end.span);
        Some(Stmt::new(StmtKind::Return(value), span))
    }
}
