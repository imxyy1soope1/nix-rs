use crate::error::{EvalError, NixRsError, ParserError};
use crate::eval::{Env, Environment};
use crate::token::Token;
use crate::{ast::*, Object};

use std::cell::RefCell;
use std::rc::Rc;

type PrefixParseFn = fn(&mut Parser, &Env) -> ParseResult;
type InfixParseFn = fn(&mut Parser, Expression, &Env) -> ParseResult;

pub type ParseResult = Result<Expression, Box<dyn NixRsError>>;

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
        macro_rules! parser {
            ($parsername:tt) => {
                Some(|s, e| s.$parsername(e))
            };
        }

        use Token::*;
        match t {
            IDENT(_) => Some(|s, e| {
                if let Token::IDENT(ident) = s.unwrap_cur() {
                    let id = ident.clone();
                    s.next();
                    /* if !e.borrow().exsits(&id) {
                        Err(EvalError::from(format!("undefined variable '{id}'")).into())
                    } else { */
                    Expression::Ident(id, e.clone()).into()
                    // }
                } else {
                    unreachable!()
                }
            }),
            INT(_) => Some(|s, _e| {
                if let Token::INT(int) = s.unwrap_cur() {
                    let i = int.clone();
                    s.next();
                    Expression::IntLiteral(i.parse().unwrap()).into()
                } else {
                    unreachable!()
                }
            }),
            FLOAT(_) => Some(|s, _e| {
                if let Token::FLOAT(float) = s.unwrap_cur() {
                    let f = float.clone();
                    s.next();
                    Expression::FloatLiteral(f.parse().unwrap()).into()
                } else {
                    unreachable!()
                }
            }),
            STRING(..) => Some(|s, e| {
                if let Token::STRING(string, interpolates) = s.cur_token.clone().unwrap() {
                    s.next();
                    if !interpolates.is_empty() {
                        Expression::InterpolateString(string, {
                            let mut v = Vec::new();
                            for i in interpolates.iter() {
                                v.push((
                                    i.0,
                                    Parser::new(Box::new(i.1.clone().into_iter()))
                                        .parse_with_env(e)?,
                                ))
                            }
                            v
                        })
                        .into()
                    } else {
                        Expression::StringLiteral(string).into()
                    }
                } else {
                    unreachable!()
                }
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
            DOLLARCURLY => parser!(parse_interpolate),
            _ => None,
        }
    }

    fn infix_parser(&self, t: &Token) -> Option<InfixParseFn> {
        macro_rules! parser {
            ($parsername:tt) => {
                Some(|s, expr, env| s.$parsername(expr, env))
            };
        }
        use Token::*;
        match t {
            PLUS | MINUS | MUL | SLASH | EQ | NEQ | LANGLE | RANGLE | LEQ | GEQ | CONCAT
            | UPDATE | IMPL | AND | OR | QUEST | DOT | ORKW => parser!(parse_infix),
            ASSIGN => parser!(parse_binding),
            COLON => parser!(parse_function),
            AT => parser!(parse_formals_set_with_alias),

            _ => None,
        }
    }

    fn parse_prefix(&mut self, env: &Env) -> ParseResult {
        let token = self.unwrap_cur().clone();
        self.next();
        Expression::Prefix(
            token.clone(),
            self.parse_expr(
                match token {
                    Token::MINUS => Precedence::NEG,
                    Token::BANG => Precedence::NOT,
                    _ => unreachable!(),
                },
                env,
            )?
            .into(),
        )
        .into()
    }

    fn parse_group(&mut self, env: &Env) -> ParseResult {
        self.next();
        let expr = self.parse_expr(Precedence::LOWEST, env);
        if !self.cur_is(Token::RPAREN) {
            panic!()
        }
        self.next();

        expr
    }

    fn parse_if(&mut self, env: &Env) -> ParseResult {
        self.next();

        let cond = self.parse_expr(Precedence::LOWEST, env)?;

        if !self.cur_is(Token::THEN) {
            panic!()
        }
        self.next();

        let consq = self.parse_expr(Precedence::LOWEST, env)?;

        if !self.cur_is(Token::ELSE) {
            panic!()
        }
        self.next();

        let alter = self.parse_expr(Precedence::LOWEST, env)?;

        Expression::If(cond.into(), consq.into(), alter.into()).into()
    }

    fn parse_binding(&mut self, name: Expression, env: &Env) -> ParseResult {
        use Expression::*;
        match name {
            Ident(..) | StringLiteral(..) | InterpolateString(..) | Interpolate(..) => Ok(()),
            Infix(ref token, ..) => {
                if token == &Token::DOT {
                    Ok(())
                } else {
                    Err(ParserError::from_string(format!(
                        "invalid binding name: {name}"
                    )))
                }
            }
            _ => Err(ParserError::from_string(format!(
                "invalid binding name: {name}"
            ))),
        }?;
        self.next();
        let expr = self.parse_expr(Precedence::ASSIGN, env)?;
        if let Binding(..) = expr {
            Err(ParserError::from_string(format!(
                "invalid binding value: {expr}"
            )))
        } else {
            Binding(name.into(), expr.into()).into()
        }
    }

    fn _inherit_apply(
        expr: Expression,
        env: &Env,
    ) -> Result<Vec<(String, Node)>, Box<dyn NixRsError>> {
        use Expression::*;

        if let Inherit(inherits, from) = expr {
            let mut ret = Vec::new();
            let env = if let Some(f) = from {
                let ident = match *f {
                    StringLiteral(string) => Ok(string),
                    Ident(ident, _) => Ok(ident),
                    _ => Err(EvalError::from("expect a set").into()),
                }?;
                env.borrow_mut().get(&ident).unwrap()
            } else {
                Object::Attrs(env.clone()).into()
            };
            for i in inherits.iter() {
                ret.push((i.clone(), env.get_attr(&i)?));
            }
            Ok(ret)
        } else {
            unreachable!()
        }
    }

    fn _binding_pair(expr: Expression, env: &Env) -> Result<(String, Node), Box<dyn NixRsError>> {
        use Expression::*;

        if let Binding(name, value) = expr {
            match *name {
                Ident(ident, _) => Ok((ident, Node::Expr(value))),
                StringLiteral(string) => Ok((string, Node::Expr(value))),
                Interpolate(_) => Ok((
                    if let Object::Str(s) = name.clone().eval()? {
                        s
                    } else {
                        unreachable!()
                    },
                    Node::Expr(value),
                )),
                InterpolateString(..) => Ok((
                    if let Object::Str(s) = name.clone().eval()? {
                        s
                    } else {
                        unreachable!()
                    },
                    Node::Expr(value),
                )),

                Infix(_token, left, right) => {
                    let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
                    let binding = Parser::_binding_pair(*right, env)?;
                    let _ = newenv.borrow_mut().set(binding.0, binding.1);
                    Ok((
                        left.eval()?
                            .try_into()
                            .map_err(|e: EvalError| -> Box<dyn NixRsError> { Box::new(e) })?,
                        Node::Value(Object::Attrs(newenv).into()),
                    ))
                }

                _ => unreachable!(),
            }
        } else {
            unreachable!()
        }
    }

    fn _parse_attrs(&mut self, env: &Env, rec: bool) -> ParseResult {
        use Expression::*;
        use Token::*;

        let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
        while !self.cur_is(RBRACE) {
            let expr = self.parse_expr(Precedence::LOWEST, if rec { &newenv } else { env })?;
            match expr {
                Inherit(..) => {
                    let inherits = Self::_inherit_apply(expr, env)?;
                    for i in inherits.into_iter() {
                        newenv
                            .borrow_mut()
                            .set(i.0, i.1)
                            .map_err(|e| -> Box<dyn NixRsError> { Box::new(e) })?;
                    }
                    Ok(())
                }
                Binding(..) => {
                    let (sym, obj) = Self::_binding_pair(expr, if rec { &newenv } else { env })?;
                    newenv
                        .borrow_mut()
                        .set(sym, obj)
                        .map_err(|e| -> Box<dyn NixRsError> { Box::new(e) })
                }
                invalid => Err(ParserError::from_string(format!(
                    "invalid expression in attrs: {invalid}"
                ))),
            }?;
            if !self.cur_is(SEMI) {
                panic!()
            }
            self.next();
        }
        self.next();

        AttrsLiteral(newenv, rec).into()
    }

    fn parse_attrs(&mut self, env: &Env) -> ParseResult {
        use Expression::*;
        use Token::*;

        self.next();

        fn parse_formals_set(s: &mut Parser, env: &Env) -> ParseResult {
            let mut args: Vec<(String, Option<Expression>)> = Vec::new();
            let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
            let mut allow_more = false;

            while !s.cur_is(RBRACE) {
                if s.cur_is(ELLIPSIS) {
                    allow_more = true;
                } else {
                    let expr = s.parse_expr(Precedence::LOWEST, &newenv)?;
                    match expr {
                        Ident(ident, _) => Ok(args.push((ident, None))),
                        Infix(token, left, right) => {
                            if token == QUEST {
                                let left = match left.as_ref() {
                                    Ident(ident, _) => Ok(ident),
                                    _ => Err(ParserError::from_string(format!("invalid formal"))),
                                }?;
                                Ok(args.push((left.clone(), Some(*right))))
                            } else {
                                Err(ParserError::from_string(format!("invalid formal")))
                            }
                        }
                        _ => Err(ParserError::from_string(format!("invalid formal: {expr}"))),
                    }?;
                }
                match s.unwrap_cur() {
                    COMMA => {
                        if allow_more {
                            panic!("expect formals to end")
                        }
                        s.next()
                    }
                    RBRACE => (),
                    invalid => panic!("unexpected {}", invalid),
                }
            }
            s.next();

            let alias = if s.cur_is(AT) {
                s.next();
                let ident = match s.unwrap_cur() {
                    IDENT(ident) => Ok(ident.clone()),
                    invalid => Err(ParserError::from_string(format!(
                        "unexpected token: {invalid}"
                    ))),
                }?;
                s.next();
                Some(ident)
            } else {
                None
            };

            FormalSet(args, alias, allow_more, newenv).into()
        }

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

        if is_attrs {
            self._parse_attrs(env, false)
        } else {
            parse_formals_set(self, env)
        }
    }

    fn parse_formals_set_with_alias(&mut self, alias: Expression, env: &Env) -> ParseResult {
        use Expression::*;
        use Token::*;

        self.next();
        self.next();

        let mut args: Vec<(String, Option<Expression>)> = Vec::new();
        let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
        let mut allow_more = false;

        while !self.cur_is(RBRACE) {
            if self.cur_is(ELLIPSIS) {
                allow_more = true;
                self.next();
            } else {
                let expr = self.parse_expr(Precedence::LOWEST, &newenv)?;
                match expr {
                    Ident(ident, _) => Ok(args.push((ident, None))),
                    Infix(token, left, right) => {
                        if token == QUEST {
                            let left = match &*left {
                                Ident(ident, _) => Ok(ident),
                                _ => Err(ParserError::from_string(format!("invalid formal"))),
                            }?;
                            Ok(args.push((left.clone(), Some(*right))))
                        } else {
                            Err(ParserError::from_string(format!("invalid formal")))
                        }
                    }
                    _ => Err(ParserError::from_string(format!("invalid formal: {expr}"))),
                }?;
            }
            match self.unwrap_cur() {
                COMMA => {
                    if allow_more {
                        Err(ParserError::new("expect formals to end"))
                    } else {
                        Ok(self.next())
                    }
                }
                RBRACE => Ok(()),
                invalid => Err(ParserError::from_string(format!("unexpected {}", invalid))),
            }?;
        }
        self.next();

        let alias = if let Ident(ident, _) = alias {
            Ok(ident)
        } else {
            Err(ParserError::from_string(format!(
                "invalid expression: {alias}"
            )))
        }?;

        Ok(FormalSet(args, Some(alias), allow_more, newenv))
    }

    fn parse_list(&mut self, env: &Env) -> ParseResult {
        self.next();
        let mut items: Vec<Expression> = Vec::new();

        while !self.cur_is(Token::RBRACKET) {
            items.push(self.parse_expr(Precedence::HIGHEST, env)?);
        }
        self.next();

        Ok(Expression::ListLiteral(items))
    }

    fn parse_let(&mut self, env: &Env) -> ParseResult {
        use Token::*;

        self.next();
        let _bindings: Vec<Expression> = Vec::new();
        let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));

        while !self.cur_is(IN) {
            match self.unwrap_cur() {
                IDENT(_) | STRING(..) => (),
                _ => panic!(),
            }
            if !self.next_is(ASSIGN) {
                panic!()
            }
            let ident = self.parse_expr(Precedence::HIGHEST, env)?;
            if let Expression::Binding(name, value) = self.parse_binding(ident, &newenv)? {
                if let Expression::Ident(ident, _) = *name {
                    newenv
                        .borrow_mut()
                        .set(ident, value.into())
                        .map_err(|e| e.into())?;
                } else if let Expression::StringLiteral(name) = *name {
                    newenv
                        .borrow_mut()
                        .set(name, value.into())
                        .map_err(|e| e.into())?;
                } else {
                    return Err(Box::new(ParserError::from(
                        "dynamic attributes not allowed in let",
                    )));
                }
            }
            if !self.cur_is(SEMI) {
                panic!()
            }
            self.next();
        }
        self.next();

        let expr = self.parse_expr(Precedence::LOWEST, &newenv)?;
        Ok(Expression::Let(newenv, expr.into()))
    }

    fn parse_with(&mut self, env: &Env) -> ParseResult {
        self.next();
        let attrs = self.parse_expr(Precedence::LOWEST, env)?;
        let name = attrs.to_string();
        let attrs = if let Object::Attrs(newenv) = attrs.eval()? {
            newenv
        } else {
            return Err(EvalError::from("expected a set").into());
        };
        if !self.cur_is(Token::SEMI) {
            panic!()
        }
        self.next();

        Ok(Expression::With(
            name,
            self.parse_expr(Precedence::LOWEST, &attrs)?.into(),
        ))
    }

    fn parse_assert(&mut self, env: &Env) -> ParseResult {
        self.next();
        let assertion = self.parse_expr(Precedence::LOWEST, env)?;
        if !self.cur_is(Token::SEMI) {
            panic!()
        }
        self.next();

        Ok(Expression::Assert(
            assertion.into(),
            self.parse_expr(Precedence::LOWEST, env)?.into(),
        ))
    }

    fn parse_rec(&mut self, env: &Env) -> ParseResult {
        use Token::*;

        if !self.next_is(LBRACE) {
            panic!()
        }
        self.next();
        let attrs = self._parse_attrs(env, true)?;
        if matches!(attrs, Expression::AttrsLiteral(..)) {
            attrs.into()
        } else {
            Err(ParserError::from("syntax error, expect a set").into())
        }
    }

    fn parse_inherit(&mut self, env: &Env) -> ParseResult {
        self.next();
        let from = if self.cur_is(Token::LPAREN) {
            self.next();
            let from = Some(self.parse_expr(Precedence::LOWEST, env)?.into());
            if !self.cur_is(Token::RPAREN) {
                panic!()
            }
            self.next();
            from
        } else {
            None
        };

        let mut inherits: Vec<String> = Vec::new();

        use Token::*;
        while match self.unwrap_cur() {
            IDENT(_) => Ok(true),
            STRING(_, v) => {
                if v.is_empty() {
                    Ok(true)
                } else {
                    Err(ParserError::new(
                        "dynamic attributes not allowed in inherit",
                    ))
                }
            }
            SEMI => Ok(false),
            invalid => Err(ParserError::from_string(format!(
                "unexpected token: {invalid}"
            ))),
        }? {
            let expr = self.parse_expr(Precedence::HIGHEST, env)?;
            inherits.push(if let Expression::Ident(ident, _) = expr {
                ident
            } else if let Expression::StringLiteral(string) = expr {
                string
            } else {
                return Err(ParserError::from("expected Id or string").into());
            })
        }
        if !self.cur_is(Token::SEMI) {
            panic!()
        }

        Ok(Expression::Inherit(inherits, from))
    }

    fn parse_rel_path(&mut self, _env: &Env) -> ParseResult {
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

        Ok(Expression::Path(literal, true))
    }

    fn parse_abs_path(&mut self, _env: &Env) -> ParseResult {
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

        Ok(Expression::Path(literal, false))
    }

    fn parse_search_path(&mut self, _env: &Env) -> ParseResult {
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

        Expression::SearchPath(Expression::Path(literal, false).into()).into()
    }

    fn parse_interpolate(&mut self, env: &Env) -> ParseResult {
        self.next();

        let expr = self.parse_expr(Precedence::HIGHEST, env)?;
        assert!(self.cur_is(Token::RBRACE));
        self.next();

        Expression::Interpolate(expr.into()).into()
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

    fn parse_infix(&mut self, left: Expression, env: &Env) -> ParseResult {
        let token = self.unwrap_cur().clone();
        let precedence = self.cur_precedence();
        self.next();

        Ok(Expression::Infix(
            token.clone(),
            left.into(),
            self.parse_expr(
                if token == Token::IMPL {
                    Precedence::IMPLLOWER
                } else {
                    precedence
                },
                env,
            )?
            .into(),
        ))
    }

    fn parse_function(&mut self, arg: Expression, env: &Env) -> ParseResult {
        match arg {
            Expression::Ident(..) => {
                let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
                self.next();
                Ok(Expression::FunctionLiteral(
                    arg.into(),
                    self.parse_expr(Precedence::FDLOWER, &newenv)?.into(),
                    newenv,
                ))
            }
            Expression::FormalSet(.., ref env) => {
                self.next();
                let body = self.parse_expr(Precedence::FDLOWER, env)?;
                let env = env.clone();
                Ok(Expression::FunctionLiteral(arg.into(), body.into(), env))
            }
            invalid => Err(ParserError::from_string(format!(
                "unexpected token: {invalid}"
            ))),
        }
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

    fn parse_expr(&mut self, precedence: Precedence, env: &Env) -> ParseResult {
        let mut left = self
            .prefix_parser(self.unwrap_cur())
            .unwrap_or_else(|| panic!("unexpected token: {}", self.unwrap_cur()))(
            self, env
        )?;

        while !self.cur_is(Token::SEMI)
            && !self.cur_is(Token::EOF)
            && precedence < self.cur_precedence()
        {
            match self.infix_parser(self.unwrap_cur()) {
                None => return Ok(left),
                Some(f) => {
                    left = f(self, left, env)?;
                }
            }
        }

        if precedence < Precedence::CALL {
            while !self.cur_is(Token::SEMI)
                && !self.cur_is(Token::EOF)
                && self.prefix_parser(self.unwrap_cur()).is_some()
            {
                left = Expression::FunctionCall(
                    left.into(),
                    self.parse_expr(Precedence::CALL, env)?.into(),
                )
            }
        }

        Ok(left)
    }

    fn next(&mut self) {
        self.cur_token = self.next_token.clone();
        self.next_token = self.l.next();
    }

    pub fn parse(&mut self) -> ParseResult {
        self.parse_expr(
            Precedence::LOWEST,
            &Rc::new(RefCell::new(Environment::with_builtins())),
        )
    }

    pub fn parse_with_env(&mut self, env: &Env) -> ParseResult {
        self.parse_expr(Precedence::LOWEST, env)
    }
}
