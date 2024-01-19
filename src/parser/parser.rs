use core::panic;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;

type PrefixParseFn = fn(&mut Parser) -> Box<dyn Expression>;
type InfixParseFn = fn(&mut Parser, Box<dyn Expression>) -> Box<dyn Expression>;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    LOWEST,

    ASSIGN,
    CALL,
    EQ,
    BOOL,
    CMP,
    SUM,
    MUL,
    PREFIX,
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

    fn prefix_parser(&self, t: &Token) -> PrefixParseFn {
        let parse_prefix = |s: &mut Parser| s.parse_prefix();

        macro_rules! strexpr {
            ($token:tt, $exprtype:tt) => {
                |s| {
                    Box::new($exprtype::new(
                        if let Token::$token(s) = s.cur_token.as_ref().unwrap() {
                            s
                        } else {
                            unreachable!()
                        },
                    ))
                }
            };
        }
        macro_rules! parser {
            ($funcname:tt) => {
                |s| s.$funcname()
            };
        }

        use Token::*;
        match t {
            IDENT(_) => strexpr!(IDENT, IdentifierExpr),
            INT(_) => strexpr!(INT, IntLiteralExpr),
            FLOAT(_) => strexpr!(FLOAT, FloatLiteralExpr),
            STRING(_) => strexpr!(STRING, StringLiteralExpr),
            TRUE | FALSE => {
                |s| Box::new(BoolLiteralExpr::new(s.cur_token.as_ref().unwrap() == &TRUE))
            }
            NULL => |_| Box::new(NullLiteralExpr),
            // MINUS | BANG => parse_prefix,
            MINUS | BANG => parser!(parse_prefix),
            // LANGLE => |s| s.parse_spath(,
            // LPAREN =>
            IF => parser!(parse_if),
            invalid => panic!("no such prefix op: {invalid}"),
        }
    }

    fn infix_parser(&self, t: &Token) -> Option<InfixParseFn> {
        let parse_infix = |s: &mut Parser, e: Box<dyn Expression>| s.parse_infix(e);

        use Token::*;
        match *t {
            PLUS | MINUS | MUL | EQ | NEQ | LANGLE | RANGLE | LEQ | GEQ | CONCAT | UPDATE
            | IMPL | AND | OR => Some(parse_infix),
            _ => None,
        }
    }

    /* fn parse_path(&mut self) -> Box<dyn Expression> {

    } */

    fn parse_prefix(&mut self) -> Box<dyn Expression> {
        let token = self.cur_token.as_ref().unwrap().clone();
        self.next();
        Box::new(PrefixExpr::new(token, self.parse_expr(Precedence::PREFIX)))
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

    fn cur_precedence(&self) -> Precedence {
        Self::_precedence(self.cur_token.as_ref().unwrap())
    }

    fn next_precedence(&self) -> Precedence {
        Self::_precedence(self.next_token.as_ref().unwrap())
    }

    fn parse_infix(&mut self, left: Box<dyn Expression>) -> Box<dyn Expression> {
        let token = self.cur_token.as_ref().unwrap().clone();
        let precedence = self.cur_precedence();
        self.next();

        Box::new(InfixExpr::new(token, left, self.parse_expr(precedence)))
    }

    fn _precedence(t: &Token) -> Precedence {
        use Token::*;
        match t {
            EQ | NEQ => Precedence::EQ,
            AND | OR | IMPL => Precedence::BOOL,
            LANGLE | RANGLE | LEQ | GEQ => Precedence::CMP,
            PLUS | MINUS => Precedence::SUM,
            SLASH | MUL => Precedence::MUL,

            _ => Precedence::LOWEST,
        }
    }

    #[inline]
    fn cur_is(&self, t: Token) -> bool {
        self.cur_token.as_ref().unwrap() == &t
    }

    #[inline]
    fn next_is(&self, t: Token) -> bool {
        self.next_token.as_ref().unwrap() == &t
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Box<dyn Expression> {
        let mut left = self.prefix_parser(self.cur_token.as_ref().unwrap())(self);

        while !self.cur_is(Token::SEMI)
            && !self.cur_is(Token::EOF)
            && precedence < self.next_precedence()
        {
            match self.infix_parser(self.next_token.as_ref().unwrap()) {
                None => return left,
                Some(f) => {
                    self.next();
                    left = f(self, left);
                }
            }
        }

        self.next();

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
