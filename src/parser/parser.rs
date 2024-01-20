use core::panic;
use std::ops::Add;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;

type PrefixParseFn = fn(&mut Parser) -> Box<dyn Expression>;
type InfixParseFn = fn(&mut Parser, Box<dyn Expression>) -> Box<dyn Expression>;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    LOWEST,

    ASSIGN,
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
    LIST,

    HIGHEST,
}
/*
    impl Precedence {
    fn to_int(&self) -> u8 {
        match self {
            Self::LOWEST => 0,
            Self::ASSIGN => 1,
            Self::FUNCDEF => 2,
            Self::EQ => 3,
            Self::BOOL => 4,
            Self::CMP => 5,
            Self::SUM => 6,
            Self::MUL => 7,
            Self::PREFIX => 8,
            Self::CALL => 9,
            Self::HASATTR => 10,
            Self::ATTR => 11,
            Self::HIGHEST => 12,
        }
    }

    fn from_int(i: u8) -> Self {
        match i {
            0 => Self::LOWEST,
            1 => Self::ASSIGN,
            2 => Self::FUNCDEF,
            3 => Self::EQ,
            4 => Self::BOOL,
            5 => Self::CMP,
            6 => Self::SUM,
            7 => Self::MUL,
            8 => Self::PREFIX,
            9 => Self::CALL,
            10 => Self::HASATTR,
            11 => Self::ATTR,
            12 => Self::HIGHEST,
            _ => panic!(),
        }
    }

    pub fn higher(&self, level: u8) -> Self {
        Self::from_int(self.to_int() + level)
    }

    pub fn lower(&self, level: u8) -> Self {
        Self::from_int(self.to_int() - level)
    }
}
*/
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

            /* IDENT(_) | INT(_) | FLOAT(_) | STRING(_) | TRUE | FALSE | NULL | LPAREN | LBRACE
            | LBRACKET | LET | WITH | REC => parser!(parse_call), */
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
        self.next();
        let mut bindings: Vec<Box<dyn Expression>> = Vec::new();

        let mut is_attrs = true;
        while !self.cur_is(Token::RBRACE) {
            bindings.push(self.parse_expr(Precedence::LOWEST));
            use Token::*;
            match self.unwrap_cur() {
                SEMI => {
                    self.next();
                }
                COMMA => {
                    is_attrs = false;
                    self.next();
                }
                RBRACE => (),
                _ => panic!(),
            }
        }
        self.next();

        if is_attrs {
            Box::new(AttrsLiteralExpr::new(bindings, false))
        } else {
            Box::new(ArgSetExpr::new(bindings))
        }
    }

    fn parse_list(&mut self) -> Box<dyn Expression> {
        self.next();
        let mut items: Vec<Box<dyn Expression>> = Vec::new();

        while !self.cur_is(Token::RBRACKET) {
            items.push(self.parse_expr(Precedence::LIST));
        }
        self.next();

        Box::new(ListLiteralExpr::new(items))
    }

    fn parse_let(&mut self) -> Box<dyn Expression> {
        self.next();
        let mut bindings: Vec<Box<dyn Expression>> = Vec::new();

        while !self.cur_is(Token::IN) {
            bindings.push(self.parse_expr(Precedence::LOWEST));
            if !self.cur_is(Token::SEMI) {
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

    fn parse_rec(&mut self) -> Box<dyn Expression> {
        if !self.next_is(Token::LBRACE) {
            panic!()
        }
        self.next();
        self.next();
        let mut bindings: Vec<Box<dyn Expression>> = Vec::new();

        while !self.cur_is(Token::RBRACE) {
            bindings.push(self.parse_expr(Precedence::LOWEST));
            if !self.cur_is(Token::SEMI) {
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

            /* IDENT(_) | INT(_) | FLOAT(_) | STRING(_) | TRUE | FALSE | NULL | LPAREN | LBRACE
            | LBRACKET | LET | WITH | REC => Precedence::CALL, */
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
            self.parse_expr(Precedence::FUNCDEF),
        ))
    }

    fn parse_call(&mut self, func: Box<dyn Expression>) -> Box<dyn Expression> {
        Box::new(FunctionCallExpr::new(
            func,
            self.parse_expr(Precedence::CALL),
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

        if !self.cur_is(Token::SEMI)
            && !self.cur_is(Token::EOF)
            && Precedence::CALL > precedence
            && precedence != Precedence::LIST
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
