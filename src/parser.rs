use crate::ast::*;
use crate::error::{NixRsError, ParserError};
use crate::eval::Environment;
use crate::token::Token;

type PrefixParseFn = fn(&mut Parser) -> ParseResult;
type InfixParseFn = fn(&mut Parser, Expression) -> ParseResult;

type ParseResult = Result<Expression, Box<dyn NixRsError>>;

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
                Some(|s| s.$parsername())
            };
        }

        use Token::*;
        match t {
            IDENT(_) => Some(|s| {
                if let Token::IDENT(ident) = s.unwrap_cur() {
                    s.next();
                    Ok(Expression::Ident(ident.clone()))
                } else {
                    unreachable!()
                }
            }),
            INT(_) => Some(|s| {
                if let Token::INT(int) = s.unwrap_cur() {
                    s.next();
                    Ok(Expression::IntLiteral(int.parse().unwrap()))
                } else {
                    unreachable!()
                }
            }),
            FLOAT(_) => Some(|s| {
                if let Token::FLOAT(float) = s.unwrap_cur() {
                    s.next();
                    Ok(Expression::FloatLiteral(float.parse().unwrap()))
                } else {
                    unreachable!()
                }
            }),
            STRING(..) => Some(|s| {
                if let Token::STRING(string, interpolates) = s.cur_token.clone().unwrap() {
                    s.next();
                    if !interpolates.is_empty() {
                        Ok(Expression::InterpolateString(
                            string,
                            interpolates
                                .iter()
                                .map(|r| {
                                    (r.0, Parser::new(Box::new(r.1.clone().into_iter())).parse())
                                })
                                .collect(),
                        ))
                    } else {
                        Ok(Expression::StringLiteral(string))
                    }
                } else {
                    unreachable!()
                }
            }),
            /* ELLIPSIS => Some(|s| {
                s.next();
            }), */
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
                Some(|s, e| s.$parsername(e))
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

    fn parse_prefix(&mut self) -> ParseResult {
        let token = self.unwrap_cur().clone();
        self.next();
        Ok(Expression::Prefix(
            token.clone(),
            Box::new(self.parse_expr(match token {
                Token::MINUS => Precedence::NEG,
                Token::BANG => Precedence::NOT,
                _ => unreachable!(),
            })?),
        ))
    }

    fn parse_group(&mut self) -> ParseResult {
        self.next();
        let expr = self.parse_expr(Precedence::LOWEST);
        if !self.cur_is(Token::RPAREN) {
            panic!()
        }
        self.next();

        expr
    }

    fn parse_if(&mut self) -> ParseResult {
        self.next();

        let cond = self.parse_expr(Precedence::LOWEST)?;

        if !self.cur_is(Token::THEN) {
            panic!()
        }
        self.next();

        let consq = self.parse_expr(Precedence::LOWEST)?;

        if !self.cur_is(Token::ELSE) {
            panic!()
        }
        self.next();

        let alter = self.parse_expr(Precedence::LOWEST)?;

        Ok(Expression::If(cond.into(), consq.into(), alter.into()))
    }

    fn parse_binding(&mut self, name: Expression) -> ParseResult {
        use Expression::*;
        match name {
            Ident(..) | StringLiteral(..) | InterpolateString(..) | Interpolate(..) => Ok(()),
            Infix(token, ..) => {
                if token == Token::DOT {
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
        let expr = self.parse_expr(Precedence::ASSIGN)?;
        if let Binding(..) = expr {
            Err(ParserError::from_string(format!(
                "invalid binding value: {expr}"
            )))
        } else {
            Ok(Binding(name.into(), expr.into()))
        }
    }

    fn parse_attrs(&mut self) -> ParseResult {
        use Expression::*;
        use Token::*;

        self.next();
        let mut bindings: Vec<Expression> = Vec::new();
        let mut args: Vec<(String, Option<Expression>)> = Vec::new();

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
                let expr = self.parse_expr(Precedence::LOWEST)?;
                match expr {
                    Binding(..) | Inherit(..) => Ok(()),
                    invalid => Err(ParserError::from_string(format!(
                        "invalid expression in attrs: {invalid}"
                    ))),
                }?;
                bindings.push(expr);
                if !self.cur_is(SEMI) {
                    panic!()
                }
                self.next();
            } else {
                if self.cur_is(ELLIPSIS) {
                    allow_more = true;
                } else {
                    let expr = self.parse_expr(Precedence::LOWEST)?;
                    match expr {
                        Ident(ident) => Ok(args.push((ident, None))),
                        Infix(token, left, right) => {
                            if token == QUEST {
                                let left = match left.as_ref() {
                                    Ident(ident) => Ok(ident),
                                    _ => Err(ParserError::from_string(format!(
                                        "invalid formal: {expr}"
                                    ))),
                                }?;
                                Ok(args.push((left.clone(), Some(*right))))
                            } else {
                                Err(ParserError::from_string(format!("invalid formal: {expr}")))
                            }
                        }
                        _ => Err(ParserError::from_string(format!("invalid formal: {expr}"))),
                    }?;
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
            Ok(AttrsLiteral(bindings, false))
        } else {
            let alias = if self.cur_is(AT) {
                self.next();
                let ident = match self.unwrap_cur() {
                    IDENT(ident) => Ok(ident),
                    invalid => Err(ParserError::from_string(format!(
                        "unexpected token: {invalid}"
                    ))),
                }?;
                self.next();
                Some(ident)
            } else {
                None
            };

            Ok(FormalSet(args, alias, allow_more))
        }
    }

    fn parse_formals_set_with_alias(&mut self, alias: Expression) -> ParseResult {
        use Expression::*;
        use Token::*;

        self.next();
        self.next();

        let mut args: Vec<(String, Option<Expression>)> = Vec::new();
        let mut allow_more = false;

        while !self.cur_is(RBRACE) {
            if self.cur_is(ELLIPSIS) {
                allow_more = true;
            } else {
                let expr = self.parse_expr(Precedence::LOWEST)?;
                match expr {
                    Ident(ident) => Ok(args.push((ident, None))),
                    Infix(token, left, right) => {
                        if token == QUEST {
                            let left = match left {
                                Ident(ident) => Ok(ident),
                                _ => {
                                    Err(ParserError::from_string(format!("invalid formal: {expr}")))
                                }
                            }?;
                            Ok(args.push((left, Some(right))))
                        } else {
                            Err(ParserError::from_string(format!("invalid formal: {expr}")))
                        }
                    }
                    _ => Err(ParserError::from_string(format!("invalid formal: {expr}"))),
                }?;
            }
            match self.unwrap_cur() {
                COMMA => {
                    if allow_more {
                        Err(ParserError::new("expect formals to end"))
                    }
                    self.next()
                }
                RBRACE => (),
                invalid => Err(ParserError::from_string(format!("unexpected {}", invalid))),
            }?;
        }
        self.next();

        let alias = if let Ident(ident) = alias {
            Ok(ident)
        } else {
            Err(ParserError::from_string(format!(
                "invalid expression: {alias}"
            )))
        }?;

        Ok(FormalSet(args, Some(alias), allow_more))
    }

    fn parse_list(&mut self) -> ParseResult {
        self.next();
        let mut items: Vec<Expression> = Vec::new();

        while !self.cur_is(Token::RBRACKET) {
            items.push(self.parse_expr(Precedence::HIGHEST));
        }
        self.next();

        Ok(Expression::ListLiteral(items))
    }

    fn parse_let(&mut self) -> ParseResult {
        use Token::*;

        self.next();
        let mut bindings: Vec<Expression> = Vec::new();

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

        Ok(Expression::Let(
            bindings,
            self.parse_expr(Precedence::LOWEST)?,
        ))
    }

    fn parse_with(&mut self) -> ParseResult {
        self.next();
        let attrs = self.parse_expr(Precedence::LOWEST)?;
        if !self.cur_is(Token::SEMI) {
            panic!()
        }
        self.next();

        Ok(Expression::With(
            attrs,
            self.parse_expr(Precedence::LOWEST)?,
        ))
    }

    fn parse_assert(&mut self) -> ParseResult {
        self.next();
        let assertion = self.parse_expr(Precedence::LOWEST)?;
        if !self.cur_is(Token::SEMI) {
            panic!()
        }
        self.next();

        Ok(Expression::Assert(
            assertion,
            self.parse_expr(Precedence::LOWEST)?,
        ))
    }

    fn parse_rec(&mut self) -> ParseResult {
        use Token::*;

        if !self.next_is(LBRACE) {
            panic!()
        }
        self.next();
        self.next();
        let mut bindings: Vec<Expression> = Vec::new();

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

        Ok(Expression::AttrsLiteral(bindings, true))
    }

    fn parse_inherit(&mut self) -> ParseResult {
        self.next();
        let from = if self.cur_is(Token::LPAREN) {
            self.next();
            let from = Some(self.parse_expr(Precedence::LOWEST)?);
            if !self.cur_is(Token::RPAREN) {
                panic!()
            }
            self.next();
            from
        } else {
            None
        };

        let mut inherits: Vec<Expression> = Vec::new();

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
        } {
            inherits.push(self.parse_expr(Precedence::HIGHEST));
        }
        if !self.cur_is(Token::SEMI) {
            panic!()
        }

        Ok(Expression::Inherit(inherits, from))
    }

    fn parse_rel_path(&mut self) -> ParseResult {
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

    fn parse_abs_path(&mut self) -> ParseResult {
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

    fn parse_search_path(&mut self) -> ParseResult {
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

        Ok(Expression::SearchPath(Expression::Path(literal, false)))
    }

    fn parse_interpolate(&mut self) -> ParseResult {
        self.next();

        let expr = self.parse_expr(Precedence::HIGHEST);
        assert!(self.cur_is(Token::RBRACE));
        self.next();

        Ok(Expression::Interpolate(expr))
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

    fn parse_infix(&mut self, left: Expression) -> ParseResult {
        let token = self.unwrap_cur().clone();
        let precedence = self.cur_precedence();
        self.next();

        Ok(Expression::Infix(
            token.clone(),
            Box::new(left),
            Box::new(self.parse_expr(if token == Token::IMPL {
                Precedence::IMPLLOWER
            } else {
                precedence
            })),
        ))
    }

    fn parse_function(&mut self, arg: Expression) -> ParseResult {
        match arg {
            Expression::Ident(..) | Expression::FormalSet(..) => (),
            invalid => Err(ParserError::from_string(format!(
                "unexpected token: {invalid}"
            ))),
        }?;

        self.next();
        Ok(Expression::FunctionLiteral(
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

    fn parse_expr(&mut self, precedence: Precedence) -> ParseResult {
        let mut left = self
            .prefix_parser(self.unwrap_cur())
            .unwrap_or_else(|| panic!("unexpected token: {}", self.unwrap_cur()))(
            self
        )?;

        while !self.cur_is(Token::SEMI)
            && !self.cur_is(Token::EOF)
            && precedence < self.cur_precedence()
        {
            match self.infix_parser(self.unwrap_cur()) {
                None => return Ok(left),
                Some(f) => {
                    left = f(self, left)?;
                }
            }
        }

        if precedence < Precedence::CALL {
            while !self.cur_is(Token::SEMI)
                && !self.cur_is(Token::EOF)
                && self.prefix_parser(self.unwrap_cur()).is_some()
            {
                left = Expression::FunctionCall(Box::new(left), Box::new(self.parse_expr(Precedence::CALL)?))
            }
        }

        Ok(left)
    }

    fn next(&mut self) {
        self.cur_token = self.next_token.clone();
        self.next_token = self.l.next();
    }

    pub fn parse(&mut self) -> ParseResult {
        self.parse_expr(Precedence::LOWEST)
    }
}
