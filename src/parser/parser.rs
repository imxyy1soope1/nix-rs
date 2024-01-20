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
    FUNCDEF,
    CALL,
    EQ,
    BOOL,
    CMP,
    SUM,
    MUL,
    PREFIX,
    HASATTR,
    ATTR,
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
            ($parsername:tt) => {
                |s| s.$parsername()
            };
        }

        use Token::*;
        match t {
            IDENT(_) => strexpr!(IDENT, IdentifierExpr),
            INT(_) => strexpr!(INT, IntLiteralExpr),
            FLOAT(_) => strexpr!(FLOAT, FloatLiteralExpr),
            STRING(_) => strexpr!(STRING, StringLiteralExpr),
            TRUE | FALSE => |s| Box::new(BoolLiteralExpr::new(s.cur_is(TRUE))),
            NULL => |_| Box::new(NullLiteralExpr),
            ELLIPSIS => |_| Box::new(EllipsisLiteralExpr),
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
            invalid => panic!("no such prefix op: {invalid}"),
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
        if !self.next_is(Token::RPAREN) {
            panic!()
        }

        expr
    }

    fn parse_if(&mut self) -> Box<dyn Expression> {
        self.next();

        let cond = self.parse_expr(Precedence::LOWEST);

        if !self.next_is(Token::THEN) {
            panic!()
        }
        self.next();
        self.next();

        let consq = self.parse_expr(Precedence::LOWEST);

        if !self.next_is(Token::ELSE) {
            panic!()
        }
        self.next();
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
            match self.unwrap_next() {
                SEMI => {
                    self.next();
                    self.next();
                }
                COMMA => {
                    is_attrs = false;
                    self.next();
                    self.next();
                }
                RBRACE => self.next(),
                _ => panic!(),
            }
        }

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
            items.push(self.parse_expr(Precedence::LOWEST));
            self.next();
        }

        Box::new(ListLiteralExpr::new(items))
    }

    fn parse_let(&mut self) -> Box<dyn Expression> {
        self.next();
        let mut bindings: Vec<Box<dyn Expression>> = Vec::new();

        while !self.cur_is(Token::IN) {
            bindings.push(self.parse_expr(Precedence::LOWEST));
            if !self.next_is(Token::SEMI) {
                panic!()
            }
            self.next();
            self.next();
        }

        self.next();

        Box::new(LetExpr::new(bindings, self.parse_expr(Precedence::LOWEST)))
    }

    fn parse_with(&mut self) -> Box<dyn Expression> {
        self.next();
        let attrs = self.parse_expr(Precedence::LOWEST);
        if !self.next_is(Token::SEMI) {
            panic!()
        }
        self.next();
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
            if !self.next_is(Token::SEMI) {
                panic!()
            }
            self.next();
            self.next();
        }

        Box::new(AttrsLiteralExpr::new(bindings, true))
    }

    fn parse_inherit(&mut self) -> Box<dyn Expression> {
        self.next();
        let from = if self.cur_is(Token::LPAREN) {
            let from = Some(self.parse_expr(Precedence::LOWEST));
            if !self.next_is(Token::RPAREN) {
                panic!()
            }
            self.next();
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
            inherits.push(self.parse_expr(Precedence::LOWEST));
            self.next();
        }
        if !self.cur_is(Token::SEMI) {
            panic!()
        }

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
        Box::new(FunctionLiteral::new(
            arg,
            self.parse_expr(Precedence::FUNCDEF),
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
        let mut left = self.prefix_parser(self.unwrap_cur())(self);

        while !self.next_is(Token::SEMI)
            && !self.next_is(Token::EOF)
            && precedence < self.next_precedence()
        {
            match self.infix_parser(self.unwrap_next()) {
                None => return left,
                Some(f) => {
                    self.next();
                    left = f(self, left);
                }
            }
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
