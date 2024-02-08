use std::error::Error;
use std::rc::Rc;

use crate::token::Token;
use crate::{ast::*, convany};

type PrefixParseFn = fn(&mut Parser) -> Rc<dyn Expression>;
type InfixParseFn = fn(&mut Parser, Rc<dyn Expression>) -> Rc<dyn Expression>;

type Result = std::result::Result<Rc<dyn Expression>, Box<dyn Error>>;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    LOWEST,

    ASSIGN,
    FDLOWER,
    FUNCDEF,
    IMPLLOWER,
    IMPL,
    OR,
    AND,
    EQ,
    CMP,
    UPDATE,
    NOT,
    SUM,
    MUL,
    CONCAT,
    HASATTR,
    NEG,
    CALL,
    ATTR,

    HIGHEST,
}

pub struct Parser {
    l: Box<dyn Iterator<Item = Token>>,

    cur_token: Option<Token>,
    next_token: Option<Token>,
}

impl Parser {
    pub fn new(l: Box<dyn Iterator<Item = Token>>) -> Parser {
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
                    Rc::new($exprtype::new(
                        if let Token::$token(string) = s.consume().unwrap() {
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
                if let Token::STRING(string, interpolates) = s.consume().unwrap() {
                    if !interpolates.is_empty() {
                        Rc::new(InterpolateStringExpr::new(
                            string,
                            interpolates
                                .iter()
                                .map(|r| {
                                    (r.0, Parser::new(Box::new(r.1.clone().into_iter())).parse())
                                })
                                .collect(),
                        ))
                    } else {
                        Rc::new(StringLiteralExpr::new(string))
                    }
                } else {
                    unreachable!()
                }
            }),
            INTER(_) => Some(|s| {
                println!("{:?} {:?}", s.cur_token, s.next_token);
                if let Token::INTER(tokens) = s.consume().unwrap() {
                    Rc::new(InterpolateExpr::new(
                        Parser::new(Box::new(tokens.into_iter())).parse(),
                    ))
                } else {
                    unreachable!()
                }
            }),
            ELLIPSIS => Some(|s| {
                s.next();
                Rc::new(EllipsisLiteralExpr)
            }),
            MINUS | BANG => parser!(parse_prefix),
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
            WAVY => parser!(parse_rel_path),
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
            | UPDATE | IMPL | AND | OR | QUEST | DOT | ORKW => parser!(parse_infix),
            ASSIGN => parser!(parse_binding),
            COLON => parser!(parse_function),
            AT => parser!(parse_argset_with_alias),

            _ => None,
        }
    }

    fn parse_prefix(&mut self) -> Rc<dyn Expression> {
        let token = self.unwrap_cur().clone();
        self.next();
        Rc::new(PrefixExpr::new(
            token.clone(),
            self.parse_expr(match token {
                Token::MINUS => Precedence::NEG,
                Token::BANG => Precedence::NOT,
                _ => unreachable!(),
            }),
        ))
    }

    fn parse_group(&mut self) -> Rc<dyn Expression> {
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

    fn parse_if(&mut self) -> Rc<dyn Expression> {
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

        Rc::new(IfExpr::new(cond, consq, alter))
    }

    fn parse_binding(&mut self, name: Rc<dyn Expression>) -> Rc<dyn Expression> {
        let a = name.as_any();
        assert!(
            a.is::<IdentifierExpr>()
                || a.is::<StringLiteralExpr>()
                || a.is::<InheritExpr>()
                || (a.is::<InfixExpr>()
                    && a.downcast_ref::<InfixExpr>().unwrap().token == Token::DOT)
        );
        self.next();
        let expr = self.parse_expr(Precedence::ASSIGN);
        assert!(!expr.as_any().is::<BindingExpr>());
        Rc::new(BindingExpr::new(name, expr))
    }

    fn parse_attrs(&mut self) -> Rc<dyn Expression> {
        use Token::*;

        self.next();
        let mut bindings: Vec<Rc<dyn Expression>> = Vec::new();
        let mut args: Vec<(String, Option<Rc<dyn Expression>>)> = Vec::new();

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
                _ => {
                    if self.cur_is(RBRACE) {
                        true
                    } else {
                        panic!()
                    }
                }
            }
        };
        let mut allow_more = false;
        while !self.cur_is(RBRACE) {
            if is_attrs {
                let expr = self.parse_expr(Precedence::LOWEST);
                let a = expr.as_any();
                assert!(a.is::<BindingExpr>() || a.is::<InheritExpr>());
                bindings.push(expr);
                if !self.cur_is(SEMI) {
                    panic!()
                }
                self.next();
            } else {
                let expr = self.parse_expr(Precedence::LOWEST);
                let a = expr.as_any();
                if a.is::<IdentifierExpr>() {
                    args.push((convany!(a, IdentifierExpr).ident.clone(), None));
                } else if a.is::<InfixExpr>() {
                    let e = convany!(a, InfixExpr);
                    assert_eq!(e.token, Token::QUEST);
                    args.push((
                        convany!(e.left.as_any(), IdentifierExpr).ident.clone(),
                        Some(e.right.clone()),
                    ));
                } else if a.is::<EllipsisLiteralExpr>() {
                    allow_more = true;
                } else {
                    panic!()
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
            Rc::new(AttrsLiteralExpr::new(bindings, false))
        } else {
            let alias = if self.cur_is(AT) {
                self.next();
                match self.unwrap_cur() {
                    IDENT(_) => (),
                    _ => panic!(),
                }
                Some(
                    convany!(
                        self.parse_expr(Precedence::HIGHEST).as_any(),
                        IdentifierExpr
                    )
                    .ident
                    .clone(),
                )
            } else {
                None
            };

            Rc::new(ArgSetExpr::new(args, allow_more, alias))
        }
    }

    fn parse_argset_with_alias(&mut self, alias: Rc<dyn Expression>) -> Rc<dyn Expression> {
        use Token::*;

        self.next();
        self.next();

        let mut args: Vec<(String, Option<Rc<dyn Expression>>)> = Vec::new();
        let mut allow_more = false;

        while !self.cur_is(RBRACE) {
            let expr = self.parse_expr(Precedence::LOWEST);
            let a = expr.as_any();
            if a.is::<IdentifierExpr>() {
                args.push((convany!(a, IdentifierExpr).ident.clone(), None));
            } else if a.is::<InfixExpr>() {
                let e = convany!(a, InfixExpr);
                assert_eq!(e.token, Token::QUEST);
                args.push((
                    convany!(e.left.as_any(), IdentifierExpr).ident.clone(),
                    Some(e.right.clone()),
                ));
            } else if a.is::<EllipsisLiteralExpr>() {
                allow_more = true;
            } else {
                panic!()
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

        Rc::new(ArgSetExpr::new(
            args,
            allow_more,
            Some(convany!(alias.as_any(), IdentifierExpr).ident.clone()),
        ))
    }

    fn parse_list(&mut self) -> Rc<dyn Expression> {
        self.next();
        let mut items: Vec<Rc<dyn Expression>> = Vec::new();

        while !self.cur_is(Token::RBRACKET) {
            items.push(self.parse_expr(Precedence::HIGHEST));
        }
        self.next();

        Rc::new(ListLiteralExpr::new(items))
    }

    fn parse_let(&mut self) -> Rc<dyn Expression> {
        use Token::*;

        self.next();
        let mut bindings: Vec<Rc<dyn Expression>> = Vec::new();

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

        Rc::new(LetExpr::new(bindings, self.parse_expr(Precedence::LOWEST)))
    }

    fn parse_with(&mut self) -> Rc<dyn Expression> {
        self.next();
        let attrs = self.parse_expr(Precedence::LOWEST);
        if !self.cur_is(Token::SEMI) {
            panic!()
        }
        self.next();

        Rc::new(WithExpr::new(attrs, self.parse_expr(Precedence::LOWEST)))
    }

    fn parse_assert(&mut self) -> Rc<dyn Expression> {
        self.next();
        let assertion = self.parse_expr(Precedence::LOWEST);
        if !self.cur_is(Token::SEMI) {
            panic!()
        }
        self.next();

        Rc::new(AssertExpr::new(
            assertion,
            self.parse_expr(Precedence::LOWEST),
        ))
    }

    fn parse_rec(&mut self) -> Rc<dyn Expression> {
        use Token::*;

        if !self.next_is(LBRACE) {
            panic!()
        }
        self.next();
        self.next();
        let mut bindings: Vec<Rc<dyn Expression>> = Vec::new();

        while !self.cur_is(RBRACE) {
            match self.unwrap_cur() {
                IDENT(_) | STRING(..) /*| NULL | TRUE | FALSE*/ => (),
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

        Rc::new(AttrsLiteralExpr::new(bindings, true))
    }

    fn parse_inherit(&mut self) -> Rc<dyn Expression> {
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

        let mut inherits: Vec<Rc<dyn Expression>> = Vec::new();

        use Token::*;
        while match self.unwrap_cur() {
            IDENT(_) => true,
            STRING(_, v) => {
                if v.is_empty() {
                    true
                } else {
                    panic!()
                }
            }
            SEMI => false,
            _ => panic!(),
        } {
            inherits.push(self.parse_expr(Precedence::HIGHEST));
        }
        if !self.cur_is(Token::SEMI) {
            panic!()
        }

        Rc::new(InheritExpr::new(inherits, from))
    }

    fn parse_rel_path(&mut self) -> Rc<dyn Expression> {
        use Token::*;

        let mut literal = String::new();
        literal.push_str(&self.unwrap_cur().to_string());
        self.next();

        while self.cur_is(SLASH) {
            literal.push('/');
            match self.unwrap_next() {
                IDENT(s) => literal.push_str(s),
                DOT => literal.push('.'),
                PARENT => literal.push_str(".."),
                _ => panic!("unexpected '{literal}'"),
            }
            self.next();
            self.next();
        }

        if literal.len() <= 2 {
            panic!("unexpected '.'")
        }

        Rc::new(PathLiteralExpr::new(literal, true))
    }

    fn parse_abs_path(&mut self) -> Rc<dyn Expression> {
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

        Rc::new(PathLiteralExpr::new(literal, false))
    }

    fn parse_search_path(&mut self) -> Rc<dyn Expression> {
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

        Rc::new(SearchPathExpr::new(Rc::new(PathLiteralExpr::new(
            literal, true,
        ))))
    }

    fn parse_interpolate(&mut self) -> Rc<dyn Expression> {
        self.next();

        let expr = self.parse_expr(Precedence::HIGHEST);
        assert!(self.cur_is(Token::RBRACE));
        self.next();

        Rc::new(InterpolateExpr::new(expr))
    }

    fn _precedence(t: &Token) -> Precedence {
        use Token::*;
        match t {
            EQ | NEQ => Precedence::EQ,
            AND => Precedence::AND,
            OR => Precedence::OR,
            IMPL => Precedence::IMPL,
            LANGLE | RANGLE | LEQ | GEQ => Precedence::CMP,
            PLUS | MINUS => Precedence::SUM,
            SLASH | MUL => Precedence::MUL,
            ASSIGN => Precedence::ASSIGN,
            QUEST => Precedence::HASATTR,
            UPDATE => Precedence::UPDATE,
            CONCAT => Precedence::CONCAT,
            DOT | ORKW => Precedence::ATTR,
            COLON | AT => Precedence::FUNCDEF,

            _ => Precedence::LOWEST,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        Self::_precedence(self.unwrap_cur())
    }

    fn parse_infix(&mut self, left: Rc<dyn Expression>) -> Rc<dyn Expression> {
        let token = self.consume().unwrap();
        let precedence = if token == Token::IMPL {
            Precedence::IMPLLOWER
        } else {
            Self::_precedence(&token)
        };

        Rc::new(InfixExpr::new(token, left, self.parse_expr(precedence)))
    }

    fn parse_function(&mut self, arg: Rc<dyn Expression>) -> Rc<dyn Expression> {
        let a = arg.as_any();
        assert!(a.is::<IdentifierExpr>() || a.is::<ArgSetExpr>());

        self.next();
        Rc::new(FunctionLiteralExpr::new(
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

    fn parse_expr(&mut self, precedence: Precedence) -> Rc<dyn Expression> {
        let mut left = self
            .prefix_parser(self.unwrap_cur())
            .unwrap_or_else(|| panic!("unexpected token: {}", self.unwrap_cur()))(
            self
        );

        println!("{left} {:?} {:?}", self.cur_token, self.next_token);
        while precedence < self.cur_precedence() {
            match self.infix_parser(self.unwrap_cur()) {
                None => return left,
                Some(f) => {
                    left = f(self, left);
                }
            }
        }

        if precedence < Precedence::CALL {
            while !self.cur_is(Token::SEMI)
                && !self.cur_is(Token::EOF)
                && self.prefix_parser(self.unwrap_cur()).is_some()
            {
                left = Rc::new(FunctionCallExpr::new(
                    left,
                    self.parse_expr(Precedence::CALL),
                ));
            }
        }

        left
    }

    fn next(&mut self) {
        self.consume();
    }

    fn consume(&mut self) -> Option<Token> {
        let cur = std::mem::replace(&mut self.cur_token, std::mem::take(&mut self.next_token));
        self.next_token = self.l.next();
        cur
    }

    pub fn parse(&mut self) -> Rc<dyn Expression> {
        self.parse_expr(Precedence::LOWEST)
    }
}
