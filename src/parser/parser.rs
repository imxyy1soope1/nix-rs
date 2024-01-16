use crate::token::Token;
use crate::lexer::Lexer;
use crate::ast::Expression;

pub struct Parser {
    l: Lexer,

    cur_token: Option<Token>,
    next_token: Option<Token>,
}

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

    fn next(&mut self) {
        self.cur_token = self.next_token.clone();
        self.next_token = self.l.next();
    }

    pub fn parse(&mut self) -> Box<dyn Expression> {
        
    }
}
