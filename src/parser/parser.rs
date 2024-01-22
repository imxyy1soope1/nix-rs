use std::error::Error;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;

type PrefixParseFn = fn(&mut Parser) -> Box<dyn Expression>;
type InfixParseFn = fn(&mut Parser, Box<dyn Expression>) -> Box<dyn Expression>;

type Result = std::result::Result<Box<dyn Expression>, Box<dyn Error>>;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    LOWEST,

    ASSIGN,
    FDLOWER,
    FUNCDEF,
    EQ,
    BOOL,
    CMP,
    SUM,
    MUL,
    PREFIX,
    CALL,
    HASATTR,
    ATTR,

    HIGHEST,
}

pub struct Parser {
    l: Lexer,

    cur_token: Option<Token>,
    next_token: Option<Token>,
}

impl Parser {
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: None,
            next_token: None,
        };

        p.next();
        p.next();

        p
    }

    fn prefix_parser(&self, t: &Token) -> Option<PrefixParseFn> {
        macro_rules! strexpr {
            ($token:tt, $exprtype:tt) => {
                Some(|s| {
                    Box::new($exprtype::new(
                        if let Token::$token(string) = s.cur_token.clone().unwrap() {
                            s.next();
                            string
                        } else {
                            unreachable!()
                        },
                    ))
                })
            };
        }
        macro_rules! parser {
            ($parsername:tt) => {
                Some(|s| s.$parsername())
            };
        }

        use Token::*;
        match t {
            IDENT(_) => strexpr!(IDENT, IdentifierExpr),
            INT(_) => strexpr!(INT, IntLiteralExpr),
            FLOAT(_) => strexpr!(FLOAT, FloatLiteralExpr),
            STRING(..) => Some(|s| {
                if let Token::STRING(string, replaces) = s.cur_token.clone().unwrap() {
                    s.next();
                    Box::new(StringLiteralExpr::new(string, replaces))
                } else {
                    unreachable!()
                }
            }),
            TRUE | FALSE => Some(|s| {
                let b = s.cur_is(TRUE);
                s.next();
                Box::new(BoolLiteralExpr::new(b))
            }),
            NULL => Some(|s| {
                s.next();
                Box::new(NullLiteralExpr)
            }),
            ELLIPSIS => Some(|s| {
                s.next();
                Box::new(EllipsisLiteralExpr)
            }),
            MINUS | BANG => parser!(parse_prefix),
            // LANGLE => |s| s.parse_spath(,
            LPAREN => parser!(parse_group),
            IF => parser!(parse_if),
            LBRACE => parser!(parse_attrs),
            LBRACKET => parser!(parse_list),
            LET => parser!(parse_let),
            WITH => parser!(parse_with),
            ASSERT => parser!(parse_assert),
            REC => parser!(parse_rec),
            INHERIT => parser!(parse_inherit),
            DOT => parser!(parse_rel_path),
            PARENT => parser!(parse_rel_path),
            SLASH => parser!(parse_abs_path),
            LANGLE => parser!(parse_search_path),
            _ => None,
        }
    }

    fn infix_parser(&self, t: &Token) -> Option<InfixParseFn> {
        macro_rules! parser {
            ($parsername:tt) => {
                Some(|s, e| s.$parsername(e))
            };
        }
        use Token::*;
        match t {
            PLUS | MINUS | MUL | SLASH | EQ | NEQ | LANGLE | RANGLE | LEQ | GEQ | CONCAT
            | UPDATE | IMPL | AND | OR | QUEST | DOT => parser!(parse_infix),
            ASSIGN => parser!(parse_binding),
            COLON => parser!(parse_function),
            AT => parser!(parse_argset_with_alias),

            _ => None,
        }
    }

    /* fn parse_path(&mut self) -> Box<dyn Expression> {

    } */

    fn parse_prefix(&mut self) -> Box<dyn Expression> {
        let token = self.unwrap_cur().clone();
        self.next();
        Box::new(PrefixExpr::new(token, self.parse_expr(Precedence::PREFIX)))
    }

    fn parse_group(&mut self) -> Box<dyn Expression> {
        self.next();
        let expr = self.parse_expr(Precedence::LOWEST);
        if !self.cur_is(Token::RPAREN) {
            panic!()
        }
        self.next();

        let a = expr.as_any();
        assert!(!a.is::<BindingExpr>());

        expr
    }

    fn parse_if(&mut self) -> Box<dyn Expression> {
        self.next();

        let cond = self.parse_expr(Precedence::LOWEST);
        let a = cond.as_any();
        assert!(!a.is::<BindingExpr>());

        if !self.cur_is(Token::THEN) {
            panic!()
        }
        self.next();

        let consq = self.parse_expr(Precedence::LOWEST);
        let a = consq.as_any();
        assert!(!a.is::<BindingExpr>());

        if !self.cur_is(Token::ELSE) {
            panic!()
        }
        self.next();

        let alter = self.parse_expr(Precedence::LOWEST);
        let a = alter.as_any();
        assert!(!a.is::<BindingExpr>());

        Box::new(IfExpr::new(cond, consq, alter))
    }

    fn parse_binding(&mut self, name: Box<dyn Expression>) -> Box<dyn Expression> {
        let a = name.as_any();
        assert!(
            a.is::<IdentifierExpr>()
                || a.is::<StringLiteralExpr>()
                || a.is::<NullLiteralExpr>()
                || a.is::<BoolLiteralExpr>()
                || a.is::<InheritExpr>()
                || (a.is::<InfixExpr>()
                    && a.downcast_ref::<InfixExpr>().unwrap().token == Token::DOT)
        );
        self.next();
        let expr = self.parse_expr(Precedence::ASSIGN);
        assert!(!expr.as_any().is::<BindingExpr>());
        Box::new(BindingExpr::new(name, expr))
    }

    fn parse_attrs(&mut self) -> Box<dyn Expression> {
        use Token::*;

        self.next();
        let mut bindings: Vec<Box<dyn Expression>> = Vec::new();

        let is_attrs = {
            match self.unwrap_next() {
                ASSIGN | DOT => true,
                IDENT(_) | STRING(..) | LPAREN => {
                    if !self.cur_is(INHERIT) {
                        panic!()
                    }
                    true
                }
                COMMA | QUEST => false,
                _ => panic!(),
            }
        };
        let mut allow_more = false;
        while !self.cur_is(RBRACE) {
            if is_attrs {
                let expr = self.parse_expr(Precedence::LOWEST);
                let a = expr.as_any();
                assert!(a.is::<BindingExpr>() || a.is::<InheritExpr>());
                println!("{} {}", self.unwrap_cur(), &expr);
                bindings.push(expr);
                if !self.cur_is(SEMI) {
                    panic!()
                }
                self.next();
            } else {
                let expr = self.parse_expr(Precedence::LOWEST);
                let a = expr.as_any();
                assert!(
                    a.is::<IdentifierExpr>()
                        || a.is::<EllipsisLiteralExpr>()
                        || (a.is::<InfixExpr>()
                            && a.downcast_ref::<InfixExpr>().unwrap().token == QUEST)
                );
                if a.is::<EllipsisLiteralExpr>() {
                    allow_more = true;
                } else {
                    bindings.push(expr);
                }
                match self.unwrap_cur() {
                    COMMA => {
                        if allow_more {
                            panic!("expect formals to end")
                        }
                        self.next()
                    }
                    RBRACE => (),
                    invalid => panic!("unexpected {}", invalid),
                }
            }
        }
        self.next();

        if is_attrs {
            Box::new(AttrsLiteralExpr::new(bindings, false))
        } else {
            let alias = if self.cur_is(AT) {
                self.next();
                match self.unwrap_cur() {
                    IDENT(_) => (),
                    _ => panic!(),
                }
                Some(self.parse_expr(Precedence::HIGHEST))
            } else {
                None
            };

            Box::new(ArgSetExpr::new(bindings, allow_more, alias))
        }
    }

    fn parse_argset_with_alias(&mut self, alias: Box<dyn Expression>) -> Box<dyn Expression> {
        use Token::*;

        self.next();
        self.next();

        let mut args: Vec<Box<dyn Expression>> = Vec::new();
        let mut allow_more = false;

        while !self.cur_is(RBRACE) {
            let expr = self.parse_expr(Precedence::LOWEST);
            let a = expr.as_any();
            assert!(
                a.is::<IdentifierExpr>()
                    || a.is::<EllipsisLiteralExpr>()
                    || (a.is::<InfixExpr>()
                        && a.downcast_ref::<InfixExpr>().unwrap().token == QUEST)
            );
            if a.is::<EllipsisLiteralExpr>() {
                allow_more = true;
            } else {
                args.push(expr);
            }
            match self.unwrap_cur() {
                COMMA => {
                    if allow_more {
                        panic!("expect formals to end")
                    }
                    self.next()
                }
                RBRACE => (),
                invalid => panic!("unexpected {}", invalid),
            }
        }
        self.next();

        Box::new(ArgSetExpr::new(args, allow_more, Some(alias)))
    }

    fn parse_list(&mut self) -> Box<dyn Expression> {
        self.next();
        let mut items: Vec<Box<dyn Expression>> = Vec::new();

        while !self.cur_is(Token::RBRACKET) {
            items.push(self.parse_expr(Precedence::HIGHEST));
        }
        self.next();

        Box::new(ListLiteralExpr::new(items))
    }

    fn parse_let(&mut self) -> Box<dyn Expression> {
        use Token::*;

        self.next();
        let mut bindings: Vec<Box<dyn Expression>> = Vec::new();

        while !self.cur_is(IN) {
            match self.unwrap_cur() {
                IDENT(_) | STRING(..) => (),
                _ => panic!(),
            }
            if !self.next_is(ASSIGN) {
                panic!()
            }
            let ident = self.parse_expr(Precedence::HIGHEST);
            bindings.push(self.parse_binding(ident));
            if !self.cur_is(SEMI) {
                panic!()
            }
            self.next();
        }
        self.next();

        Box::new(LetExpr::new(bindings, self.parse_expr(Precedence::LOWEST)))
    }

    fn parse_with(&mut self) -> Box<dyn Expression> {
        self.next();
        let attrs = self.parse_expr(Precedence::LOWEST);
        if !self.cur_is(Token::SEMI) {
            panic!()
        }
        self.next();

        Box::new(WithExpr::new(attrs, self.parse_expr(Precedence::LOWEST)))
    }

    fn parse_assert(&mut self) -> Box<dyn Expression> {
        self.next();
        let assertion = self.parse_expr(Precedence::LOWEST);
        if !self.cur_is(Token::SEMI) {
            panic!()
        }
        self.next();

        Box::new(AssertExpr::new(
            assertion,
            self.parse_expr(Precedence::LOWEST),
        ))
    }

    fn parse_rec(&mut self) -> Box<dyn Expression> {
        use Token::*;

        if !self.next_is(LBRACE) {
            panic!()
        }
        self.next();
        self.next();
        let mut bindings: Vec<Box<dyn Expression>> = Vec::new();

        while !self.cur_is(RBRACE) {
            match self.unwrap_cur() {
                IDENT(_) | STRING(..) | NULL | TRUE | FALSE => (),
                INHERIT => {
                    bindings.push(self.parse_inherit());
                    continue;
                }
                _ => panic!(),
            }
            if !self.next_is(ASSIGN) && !self.next_is(DOT) {
                panic!()
            }
            let ident = self.parse_expr(Precedence::CALL);
            bindings.push(self.parse_binding(ident));
            if !self.cur_is(SEMI) {
                panic!()
            }
            self.next();
        }
        self.next();

        Box::new(AttrsLiteralExpr::new(bindings, true))
    }

    fn parse_inherit(&mut self) -> Box<dyn Expression> {
        self.next();
        let from = if self.cur_is(Token::LPAREN) {
            self.next();
            let from = Some(self.parse_expr(Precedence::LOWEST));
            if !self.cur_is(Token::RPAREN) {
                panic!()
            }
            self.next();
            from
        } else {
            None
        };

        let mut inherits: Vec<Box<dyn Expression>> = Vec::new();

        use Token::*;
        while match self.unwrap_cur() {
            IDENT(_) | STRING(..) => true,
            SEMI => false,
            _ => panic!(),
        } {
            inherits.push(self.parse_expr(Precedence::HIGHEST));
        }
        if !self.cur_is(Token::SEMI) {
            panic!()
        }

        Box::new(InheritExpr::new(inherits, from))
    }

    fn parse_rel_path(&mut self) -> Box<dyn Expression> {
        use Token::*;

        let mut literal = String::new();
        literal.push_str(&format!("{}", self.unwrap_cur()));
        self.next();

        while self.cur_is(SLASH) {
            literal.push('/');
            match self.unwrap_next() {
                IDENT(s) => literal.push_str(s),
                DOT => literal.push('.'),
                PARENT => literal.push_str(".."),
                _ => panic!("unexpected '.'"),
            }
            self.next();
            self.next();
        }

        if literal.len() <= 2 {
            println!("{}", literal);
            panic!("unexpected '.'")
        }

        Box::new(PathLiteralExpr::new(literal, true))
    }

    fn parse_abs_path(&mut self) -> Box<dyn Expression> {
        use Token::*;

        let mut literal = String::new();

        while self.cur_is(SLASH) {
            literal.push('/');
            match self.unwrap_next() {
                IDENT(s) => literal.push_str(s),
                DOT => literal.push('.'),
                PARENT => literal.push_str(".."),
                _ => panic!("unexpected '/'"),
            }
            self.next();
            self.next();
        }

        if literal.len() == 1 {
            panic!("unexpected '/'")
        }

        Box::new(PathLiteralExpr::new(literal, false))
    }

    fn parse_search_path(&mut self) -> Box<dyn Expression> {
        use Token::*;

        self.next();

        let mut literal = String::from("./");

        match self.unwrap_cur() {
            IDENT(s) => {
                literal.push_str(s);
                self.next();
            }
            DOT => {
                literal.push('.');
                self.next();
            }
            PARENT => {
                literal.push_str("..");
                self.next();
            }
            _ => (),
        }

        while self.cur_is(SLASH) {
            literal.push('/');
            match self.unwrap_next() {
                IDENT(s) => literal.push_str(s),
                DOT => literal.push('.'),
                PARENT => literal.push_str(".."),
                _ => panic!("unexpected '<'"),
            }
            self.next();
            self.next();
        }

        if literal.len() == 2 || !self.cur_is(RANGLE) {
            panic!("unexpected '<'")
        }
        self.next();

        Box::new(SearchPathExpr::new(Box::new(PathLiteralExpr::new(
            literal, true,
        ))))
    }

    fn _precedence(t: &Token) -> Precedence {
        use Token::*;
        match t {
            EQ | NEQ => Precedence::EQ,
            AND | OR | IMPL => Precedence::BOOL,
            LANGLE | RANGLE | LEQ | GEQ => Precedence::CMP,
            PLUS | MINUS => Precedence::SUM,
            SLASH | MUL => Precedence::MUL,
            ASSIGN => Precedence::ASSIGN,
            QUEST => Precedence::HASATTR,
            DOT => Precedence::ATTR,
            COLON | AT => Precedence::FUNCDEF,

            _ => Precedence::LOWEST,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        Self::_precedence(self.unwrap_cur())
    }

    fn next_precedence(&self) -> Precedence {
        Self::_precedence(self.unwrap_next())
    }

    fn parse_infix(&mut self, left: Box<dyn Expression>) -> Box<dyn Expression> {
        let token = self.unwrap_cur().clone();
        let precedence = self.cur_precedence();
        self.next();

        Box::new(InfixExpr::new(token, left, self.parse_expr(precedence)))
    }

    fn parse_function(&mut self, arg: Box<dyn Expression>) -> Box<dyn Expression> {
        self.next();
        Box::new(FunctionLiteralExpr::new(
            arg,
            self.parse_expr(Precedence::FDLOWER),
        ))
    }

    #[inline]
    fn cur_is(&self, t: Token) -> bool {
        self.unwrap_cur() == &t
    }

    #[inline]
    fn next_is(&self, t: Token) -> bool {
        self.unwrap_next() == &t
    }

    #[inline]
    fn unwrap_cur(&self) -> &Token {
        self.cur_token.as_ref().unwrap()
    }

    #[inline]
    fn unwrap_next(&self) -> &Token {
        self.next_token.as_ref().unwrap()
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Box<dyn Expression> {
        let mut left =
            self.prefix_parser(self.unwrap_cur())
                .expect(&format!("unexpected token: {}", self.unwrap_cur()))(self);

        while !self.cur_is(Token::SEMI)
            && !self.cur_is(Token::EOF)
            && precedence < self.cur_precedence()
        {
            match self.infix_parser(self.unwrap_cur()) {
                None => return left,
                Some(f) => {
                    left = f(self, left);
                }
            }
        }

        while !self.cur_is(Token::SEMI)
            && !self.cur_is(Token::EOF)
            && Precedence::CALL > precedence
            && self.prefix_parser(self.unwrap_cur()).is_some()
        {
            left = Box::new(FunctionCallExpr::new(
                left,
                self.parse_expr(Precedence::CALL),
            ));
        }

        left
    }

    fn next(&mut self) {
        self.cur_token = self.next_token.clone();
        self.next_token = self.l.next();
    }

    pub fn parse(&mut self) -> Box<dyn Expression> {
        let expr = self.parse_expr(Precedence::LOWEST);
        expr
    }
}
