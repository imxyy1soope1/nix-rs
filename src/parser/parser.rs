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

#[allow(dead_code)]
impl Parser {
    pub fn from_lexer(l: Lexer) -> Parser {
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
            STRING(_) => strexpr!(STRING, StringLiteralExpr),
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
            PLUS | MINUS | MUL | EQ | NEQ | LANGLE | RANGLE | LEQ | GEQ | CONCAT | UPDATE
            | IMPL | AND | OR | QUEST | DOT => parser!(parse_infix),
            ASSIGN => parser!(parse_binding),
            COLON => parser!(parse_function),

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

        expr
    }

    fn parse_if(&mut self) -> Box<dyn Expression> {
        self.next();

        let cond = self.parse_expr(Precedence::LOWEST);

        if !self.cur_is(Token::THEN) {
            panic!()
        }
        self.next();

        let consq = self.parse_expr(Precedence::LOWEST);

        if !self.cur_is(Token::ELSE) {
            panic!()
        }
        self.next();

        let alter = self.parse_expr(Precedence::LOWEST);

        Box::new(IfExpr::new(cond, consq, alter))
    }

    fn parse_binding(&mut self, name: Box<dyn Expression>) -> Box<dyn Expression> {
        self.next();
        Box::new(BindingExpr::new(name, self.parse_expr(Precedence::ASSIGN)))
    }

    fn parse_attrs(&mut self) -> Box<dyn Expression> {
        use Token::*;

        self.next();
        let mut bindings: Vec<Box<dyn Expression>> = Vec::new();

        let is_attrs = {
            match self.unwrap_next() {
                ASSIGN => true,
                IDENT(_) | STRING(_) | LPAREN => {
                    if !self.cur_is(INHERIT) {
                        panic!()
                    }
                    true
                }
                COMMA | QUEST => false,
                _ => panic!(),
            }
        };
        while !self.cur_is(RBRACE) {
            if is_attrs {
                match self.unwrap_cur() {
                    IDENT(_) | STRING(_) | NULL | TRUE | FALSE => (),
                    INHERIT => {
                        bindings.push(self.parse_inherit());
                        continue;
                    }
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
            } else {
                match self.unwrap_cur() {
                    IDENT(_) | NULL | TRUE | FALSE => (),
                    ELLIPSIS => {
                        if !self.next_is(RBRACE) {
                            panic!()
                        }
                        self.next();
                        self.next();
                        return Box::new(ArgSetExpr::new(bindings, true));
                    }
                    _ => panic!(),
                }
                match self.unwrap_next() {
                    COMMA | QUEST | RBRACE => (),
                    _ => panic!(),
                }
                bindings.push(self.parse_expr(Precedence::LOWEST));
                match self.unwrap_cur() {
                    COMMA => self.next(),
                    RBRACE => (),
                    _ => panic!(),
                }
            }
        }
        self.next();

        if is_attrs {
            Box::new(AttrsLiteralExpr::new(bindings, false))
        } else {
            Box::new(ArgSetExpr::new(bindings, false))
        }
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
                IDENT(_) | STRING(_) => (),
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

        Box::new(AssertExpr::new(assertion, self.parse_expr(Precedence::LOWEST)))
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
                IDENT(_) | STRING(_) | NULL | TRUE | FALSE => (),
                INHERIT => {
                    bindings.push(self.parse_inherit());
                    continue;
                }
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
            IDENT(_) | STRING(_) => true,
            SEMI => false,
            _ => panic!(),
        } {
            inherits.push(self.parse_expr(Precedence::HIGHEST));
        }
        if !self.cur_is(Token::SEMI) {
            panic!()
        }
        self.next();

        Box::new(InheritExpr::new(inherits, from))
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
            COLON => Precedence::FUNCDEF,

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
        let mut left = self.prefix_parser(self.unwrap_cur()).unwrap()(self);

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
