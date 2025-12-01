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

        // NOTE: for now, the only top-level declarations are functions
        while self.match_func_decl() {
            if let Some(decl) = self.parse_func_decl() {
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

    fn try_eat(&mut self, kind: LexemKind) -> Option<&Lexem> {
        match self.peek() {
            Some(lexem) if lexem.kind == kind => {
                self.eat(kind)
            }
            _ => None,
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
        self.match_block() |
        self.match_decl() |
        self.match_integer_literal() |
        self.match_identifier() |
        self.match_return()
    }

    fn match_block(&self) -> bool {
        self.match_lexem(LexemKind::LeftBrace)
    }

    fn match_integer_literal(&self) -> bool {
        self.match_lexem(LexemKind::IntegerLiteral)
    }

    fn match_identifier(&self) -> bool {
        self.match_lexem(LexemKind::Identifier)
    }

    fn match_return(&self) -> bool {
        self.match_lexem(LexemKind::Return)
    }

    // ----- PARSERS -----
    fn parse_decl(&mut self) -> Option<Decl> {
        if self.match_func_decl() {
            self.parse_func_decl()
        } else if self.match_variable_decl() {
            self.parse_variable_decl()
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

        let body = self.parse_block()?;
        let span = start.merge_with(&body.span);

        let func = FunctionDecl {
            name,
            return_ty,
            body
        };
        Some(Decl::new(DeclKind::Function(func), span))
    }

    fn parse_variable_decl(&mut self) -> Option<Decl> {
        let start = self.eat(LexemKind::Let)?.span.clone();

        let name = self.eat(LexemKind::Identifier)?.span.clone();

        self.eat(LexemKind::Colon)?;
        let ty = self.parse_ty()?;

        self.eat(LexemKind::Equals)?;
        let init = self.parse_expr()?;
        
        let span = start.merge_with(&init.span);
        let variable = VariableDecl {
            name,
            ty,
            init
        };
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
        if self.match_block() {
            self.parse_block()
        } else if self.match_decl() {
            let decl = self.parse_decl()?;
            let span = decl.span.clone();
            Some(Expr::new(ExprKind::Decl(Box::new(decl)), span))
        } else if self.match_return() {
            self.parse_return()
        } else {
            self.parse_general_expr()
        }
    }

    fn parse_block(&mut self) -> Option<Expr> {
        let start = self.eat(LexemKind::LeftBrace)?.span.clone();

        let mut stmts = vec![];
        let mut last_expr = None;
        while self.match_expr() {
            let expr = self.parse_expr()?;
            if last_expr.is_some() {
                println!("{:?}", last_expr);
                self.reporter.report(create_diagnostic!(UnexpectedExprAfterLastExprInBlock, self.peek().unwrap().span.clone()));
            }
            match self.try_eat(LexemKind::Semicolon) {
                Some(..) => {
                    stmts.push(expr);
                }
                None => {
                    last_expr = Some(expr);
                }
            }
        }

        let end = self.eat(LexemKind::RightBrace)?;
        let span = start.merge_with(&end.span);

        Some(Expr::new(ExprKind::Block(Block::new(stmts, last_expr)), span))
    }

    fn parse_integer_literal(&mut self) -> Option<Expr> {
        let literal = self.eat(LexemKind::IntegerLiteral)?;
        Some(Expr::new(ExprKind::IntegerLiteral(IntegerLiteral::new(literal.span.clone())), literal.span.clone()))
    }

    fn parse_identifier(&mut self) -> Option<Expr> {
        let identifier = self.eat(LexemKind::Identifier)?;
        Some(Expr::new(ExprKind::VariableReference(VariableReference::new(identifier.span.clone())), identifier.span.clone()))
    }

    fn parse_return(&mut self) -> Option<Expr> {
        let start = self.eat(LexemKind::Return)?.span.clone();
        let value = self.parse_expr()?;

        let span = start.merge_with(&value.span);
        Some(Expr::new(ExprKind::Return(Return::new(value)), span))
    }

    fn parse_primary_expr(&mut self) -> Option<Expr> {
        if self.match_integer_literal() {
            self.parse_integer_literal()
        } else if self.match_identifier() {
            self.parse_identifier()
        } else {
            None
        }
    }

    fn parse_general_expr(&mut self) -> Option<Expr> {
        let mut lhs = self.parse_primary_expr()?;
        while let Some(op) = self.match_binary_op() {
            self.advance();
            let rhs = self.parse_primary_expr()?; // FIXME: Pratt parsing
            let span = lhs.span.clone().merge_with(&rhs.span);
            lhs = Expr::new(ExprKind::Binary(BinaryExpr::new(lhs, op, rhs)), span);
        }
        Some(lhs)
    }
}
