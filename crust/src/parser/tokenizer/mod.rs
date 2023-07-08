#![allow(clippy::upper_case_acronyms)]

use std::fmt::Display;
use std::iter::Peekable;
use std::num::NonZeroU8;
use std::rc::Rc;
use std::str::CharIndices;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddrType {
    ABS,
    ZPG,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Uint(NonZeroU8),
    Int(NonZeroU8),
    Ref { typ: Rc<Self>, addr: AddrType },
}
impl Type {
    pub fn get_size(&self) -> NonZeroU8 {
        match self {
            Type::Uint(size) => *size,
            Type::Int(size) => *size,
            Type::Ref { typ: _, addr } => unsafe {
                match addr {
                    AddrType::ABS => NonZeroU8::new_unchecked(2),
                    AddrType::ZPG => NonZeroU8::new_unchecked(1),
                }
            },
        }
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Uint(n) => write!(f, "u{n}"),
            Type::Int(n) => write!(f, "i{n}"),
            Type::Ref { typ, addr } => match addr {
                AddrType::ABS => write!(f, "#{typ}"),
                AddrType::ZPG => write!(f, "${typ}"),
            },
            // Type::Ref(typ) => write!(f, "&{typ}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    ADD,
    SUB,
    // MUL,
    // DIV,
}
impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::ADD => write!(f, "+"),
            Operator::SUB => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Token {
    NUMBER(usize),
    OPERATOR(Operator),
    IDENTIFIER(Rc<str>),
    TYPE(Type),
    ADDRTYPE(AddrType),
    POINTER(AddrType),
    VAR,
    ASSIGN,
    SEMICOLON,
    COLON,
    OPEN,
    CLOSE,
    LOOP,
    BOPEN,
    BCLOSE,
    REFERENCE,
    DEREFERENCE,
}

fn symbol(c: char) -> Option<Token> {
    match c {
        '(' => Some(Token::OPEN),
        ')' => Some(Token::CLOSE),
        '{' => Some(Token::BOPEN),
        '}' => Some(Token::BCLOSE),
        ';' => Some(Token::SEMICOLON),
        ':' => Some(Token::COLON),
        '=' => Some(Token::ASSIGN),
        '+' => Some(Token::OPERATOR(Operator::ADD)),
        '-' => Some(Token::OPERATOR(Operator::SUB)),
        '&' => Some(Token::REFERENCE),
        '*' => Some(Token::DEREFERENCE),
        '$' => Some(Token::POINTER(AddrType::ZPG)),
        '#' => Some(Token::POINTER(AddrType::ABS)),
        _ => None,
    }
}
fn typ(s: &str) -> Option<Type> {
    let (c, n) = s.split_at(1);
    match c {
        "u" => {
            let n = NonZeroU8::new(u8::from_str(n).ok()?)?;
            Some(Type::Uint(n))
        }
        "i" => {
            let n = NonZeroU8::new(u8::from_str(n).ok()?)?;
            Some(Type::Int(n))
        }
        _ => None,
    }
}

pub struct Tokenizer<'a> {
    code_chars: Peekable<CharIndices<'a>>,
    code: &'a str,
}
impl<'a> Tokenizer<'a> {
    pub fn new(code: &'a str) -> Self {
        let code_chars = code.char_indices().peekable();

        Self { code_chars, code }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // leading whitespace will break tokenizer
        while let Some((_, c)) = self.code_chars.peek() {
            if c.is_whitespace() {
                self.code_chars.next();
            } else {
                break;
            }
        }

        let (start, c) = self.code_chars.next()?;

        if let Some(symbol) = symbol(c) {
            return Some(symbol);
        }

        let mut end = start;

        while let Some((_i, c)) = self.code_chars.peek() {
            if c.is_whitespace() || symbol(*c).is_some() {
                break;
            }
            let (i, _c) = self.code_chars.next().unwrap();
            end = i;
        }

        let token = &self.code[start..=end];

        match usize::from_str(token) {
            Ok(num) => Some(Token::NUMBER(num)),
            _ if token == "abs" => Some(Token::ADDRTYPE(AddrType::ABS)),
            _ if token == "zpg" => Some(Token::ADDRTYPE(AddrType::ZPG)),
            _ if token == "var" => Some(Token::VAR),
            _ if token == "loop" => Some(Token::LOOP),
            _ => {
                if let Some(typ) = typ(token) {
                    Some(Token::TYPE(typ))
                } else {
                    Some(Token::IDENTIFIER(Rc::from(token)))
                }
            }
        }
    }
}
