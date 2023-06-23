use std::cell::Cell;
use std::collections::HashMap;
use std::num::NonZeroU8;
use std::rc::Rc;
use std::{
    iter::Peekable,
    str::{CharIndices, FromStr},
};

#[derive(Debug, Clone, Copy)]
enum Operator {
    ADD,
    SUB,
    // MUL,
    // DIV,
}

#[derive(Debug, Clone)]
enum Token {
    NUMBER(usize),
    OPERATOR(Operator),
    IDENTIFIER(Rc<str>),
    VAR,
    ASSIGN,
    SEMICOLON,
    COLON,
    OPEN,
    CLOSE,
    LOOP,
    BOPEN,
    BCLOSE,
    REFRENCE,
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
        '&' => Some(Token::REFRENCE),
        // '*' => Some(Token::OPERATOR(Operator::MUL)),
        _ => None,
    }
}

struct Tokenizer<'a> {
    code_chars: Peekable<CharIndices<'a>>,
    code: &'a str,
}
impl<'a> Tokenizer<'a> {
    fn new(code: &'a str) -> Self {
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

        if let Ok(num) = usize::from_str(token) {
            Some(Token::NUMBER(num))
        } else if token == "loop" {
            Some(Token::LOOP)
        } else if token == "var" {
            Some(Token::VAR)
        } else {
            Some(Token::IDENTIFIER(Rc::from(token)))
        }
    }
}

#[derive(Debug, Clone)]
enum Expression {
    CONSTANT {
        value: usize,
        typ: Cell<Option<Type>>,
    },
    IDENTIFIER {
        ident: Rc<str>,
    },
    REFRENCE {
        ident: Rc<str>,
    },
    BINOP {
        lhs: Rc<Self>,
        op: Operator,
        rhs: Rc<Self>,
    },
}
#[derive(Debug, Clone)]
enum Instruction {
    ASSIGN {
        ident: Rc<str>,
        expr: Rc<Expression>,
    },
    LOOP {
        block: Option<Rc<Self>>,
    },
    // INSTRUCTIONS(Vec<Rc<Self>>),
    BIN {
        lhs: Rc<Self>,
        rhs: Rc<Self>,
    },
}

#[derive(Debug, Clone)]
enum AST {
    EXPRESSION(Rc<Expression>),
    INSTRUCTION(Rc<Instruction>),
    BLOCK(Option<Rc<Instruction>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Type {
    Uint(NonZeroU8),
    // Ref(Ref),
    // Ref(Box<Self>),
}
// #[derive(Debug, Clone, PartialEq, Eq)]
// enum Ref {
//     Var { ident: Rc<str>, typ: Type },
//     Ref(Rc<Self>),
// }
// impl Ref {
//     fn get_ident(&self) -> &Rc<str> {
//         match self {
//             Ref::Var { ident, typ: _ } => ident,
//             Ref::Ref(r) => r.get_ident(),
//         }
//     }
//     fn get_type(&self) -> &Type {
//         match self {
//             Ref::Var { ident: _, typ } => typ,
//             Ref::Ref(r) => r.get_type(),
//         }
//     }
// }

enum AddrType {
    Abs(u16),
    Zp(u8),
}
struct Var {
    addr: AddrType,
    typ: Type,
}

type SymbolTable = HashMap<Rc<str>, Option<Type>>;
type MemTable = HashMap<Rc<str>, Var>;

#[derive(Debug)]
enum Node {
    Token(Token),
    AST(AST),
}

fn parse(code: &str) -> (Rc<Instruction>, SymbolTable) {
    let mut stack = Vec::new();
    let mut tokenizer = Tokenizer::new(code).peekable();

    let mut symbol_table = SymbolTable::new();

    while let Some(token) = tokenizer.next() {
        stack.push(Node::Token(token));

        while match &stack[..] {
            [.., Node::AST(AST::INSTRUCTION(lhs)), Node::AST(AST::INSTRUCTION(rhs))] => {
                let lhs = lhs.clone();
                let rhs = rhs.clone();
                stack.pop();
                stack.pop();

                stack.push(Node::AST(AST::INSTRUCTION(Rc::new(Instruction::BIN {
                    lhs,
                    rhs,
                }))));
                true
            }
            [.., Node::Token(Token::NUMBER(n))] => {
                if let Some(Token::ASSIGN) = tokenizer.peek() {
                    false
                } else {
                    let n = *n;
                    stack.pop();
                    stack.push(Node::AST(AST::EXPRESSION(Rc::new(Expression::CONSTANT {
                        value: n,
                        typ: Cell::new(None),
                    }))));
                    true
                }
            }
            [.., Node::Token(Token::REFRENCE), Node::Token(Token::IDENTIFIER(ident))] => {
                let ident = ident.clone();
                stack.pop();
                stack.pop();
                stack.push(Node::AST(AST::EXPRESSION(Rc::new(Expression::REFRENCE {
                    ident,
                }))));
                true
            }
            [.., Node::Token(Token::IDENTIFIER(ident))] => {
                if let Some(Token::ASSIGN | Token::COLON) = tokenizer.peek() {
                    false
                } else {
                    let ident = ident.clone();
                    stack.pop();
                    stack.push(Node::AST(AST::EXPRESSION(Rc::new(
                        Expression::IDENTIFIER {
                            ident,
                            // typ: None
                        },
                    ))));
                    true
                }
            }
            [.., Node::Token(Token::OPEN), Node::AST(expr @ AST::EXPRESSION(_)), Node::Token(Token::CLOSE)] =>
            {
                let expr = expr.clone();
                stack.pop();
                stack.pop();
                stack.pop();
                stack.push(Node::AST(expr));
                true
            }
            [.., Node::AST(AST::EXPRESSION(lhs)), Node::Token(Token::OPERATOR(op)), Node::AST(AST::EXPRESSION(rhs))] =>
            {
                let lhs = lhs.clone();
                let rhs = rhs.clone();
                let op = *op;
                stack.pop();
                stack.pop();
                stack.pop();
                stack.push(Node::AST(AST::EXPRESSION(Rc::new(Expression::BINOP {
                    lhs,
                    op,
                    rhs,
                }))));

                true
            }
            [.., Node::Token(Token::VAR), Node::Token(Token::IDENTIFIER(ident)), Node::Token(Token::COLON), Node::Token(Token::NUMBER(n)), Node::Token(Token::ASSIGN), Node::AST(AST::EXPRESSION(expr)), Node::Token(Token::SEMICOLON)] =>
            {
                let ident = ident.clone();
                let expr = expr.clone();
                let n = *n;
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();

                let typ = Some(Type::Uint(
                    NonZeroU8::new(n as u8).expect("size can't be zero"),
                ));
                symbol_table.insert(ident.clone(), typ);

                stack.push(Node::AST(AST::INSTRUCTION(Rc::new(Instruction::ASSIGN {
                    ident,
                    expr,
                }))));
                true
            }
            [.., Node::Token(Token::VAR), Node::Token(Token::IDENTIFIER(ident)), Node::Token(Token::ASSIGN), Node::AST(AST::EXPRESSION(expr)), Node::Token(Token::SEMICOLON)] =>
            {
                let ident = ident.clone();
                let expr = expr.clone();
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();

                symbol_table.insert(ident.clone(), None);

                stack.push(Node::AST(AST::INSTRUCTION(Rc::new(Instruction::ASSIGN {
                    ident,
                    expr,
                }))));
                true
            }
            [.., Node::Token(Token::IDENTIFIER(ident)), Node::Token(Token::ASSIGN), Node::AST(AST::EXPRESSION(expr)), Node::Token(Token::SEMICOLON)] =>
            {
                let ident = ident.clone();
                let expr = expr.clone();
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();

                stack.push(Node::AST(AST::INSTRUCTION(Rc::new(Instruction::ASSIGN {
                    ident,
                    expr,
                }))));
                true
            }
            [.., Node::Token(Token::BOPEN), Node::Token(Token::BCLOSE)] => {
                stack.pop();
                stack.pop();

                stack.push(Node::AST(AST::BLOCK(None)));
                true
            }
            [.., Node::Token(Token::BOPEN), Node::AST(AST::INSTRUCTION(instructions)), Node::Token(Token::BCLOSE)] =>
            {
                let instructions = instructions.clone();
                stack.pop();
                stack.pop();
                stack.pop();

                stack.push(Node::AST(AST::BLOCK(Some(instructions))));
                true
            }
            [.., Node::Token(Token::LOOP), Node::AST(AST::BLOCK(block))] => {
                let block = block.clone();
                stack.pop();
                stack.pop();

                stack.push(Node::AST(AST::INSTRUCTION(Rc::new(Instruction::LOOP {
                    block,
                }))));
                true
            }
            _ => false,
        } {}
    }

    for node in &stack {
        println!("{node:#?}");
    }
    println!("{symbol_table:#?}");

    if stack.len() == 1 {
        if let Node::AST(AST::INSTRUCTION(ast)) = stack.pop().unwrap() {
            // println!("{ast:#?}");
            (ast, symbol_table)
        } else {
            panic!("node is not ast or instruction")
        }
    } else {
        panic!("stack is bigger then 1");
    }
}

/// `imp` used for implied typ incase type is not defined  
///
/// Returns the type of expression
fn analyse_expression(
    expr: &Expression,
    symbol_table: &SymbolTable,
    imp_typ: Option<Type>,
) -> Result<Option<Type>, String> {
    match expr {
        Expression::CONSTANT { value: _, typ } => {
            let expr_typ = typ.get();

            match (imp_typ, expr_typ) {
                (None, None) => Ok(None), // leave it for now (tried again later)
                (_, Some(exp_typ)) => Ok(Some(exp_typ)), // ignore implied
                (Some(imp_typ), None) => {
                    typ.set(Some(imp_typ));
                    Ok(Some(imp_typ))
                } // set implied
            }
            // (Some(imp_typ), Some(exp_typ)) => {
            // if imp_typ != exp_typ {
            // Err(format!(
            // "Implied type ({imp_typ:?}) doesn't match Constant type ({exp_typ:?})"
            // ))
            // } else {
            // Ok(Some(exp_typ))
            // }
            // }
        }
        Expression::IDENTIFIER { ident } => {
            let ident_typ = symbol_table
                .get(ident)
                .copied()
                .ok_or(format!("Ident: ({ident}) doesn't exist"))?;

            match (imp_typ, ident_typ) {
                (None, None) => Ok(None), // leave it for now
                (_, Some(_)) => Ok(ident_typ),
                (Some(_), None) => Ok(imp_typ),
            }
        }
        Expression::BINOP { lhs, op, rhs } => {
            // get expected definate types
            let lhs_typ1 = analyse_expression(lhs, symbol_table, imp_typ)?;
            let rhs_typ1 = analyse_expression(rhs, symbol_table, imp_typ)?;

            // set definate types
            let lhs_typ2 = analyse_expression(lhs, symbol_table, rhs_typ1)?;
            // .ok_or(format!("operad on lhs does not have a type"))?;
            let rhs_typ2 = analyse_expression(rhs, symbol_table, lhs_typ1)?;
            // .ok_or(format!("operad on rhs does not have a type"))?;

            println!("{lhs_typ2:?} {rhs_typ2:?}");
            // its fine of both sides are none
            if lhs_typ2 == rhs_typ2 {
                // Ok(Some(lhs_typ2))
                Ok(lhs_typ2)
            } else {
                Err(format!(
                    "types dont match on operation: ({lhs_typ2:?}) {op:?} ({rhs_typ2:?})"
                ))
            }
        }
        Expression::REFRENCE { ident: _ } => Ok(Some(Type::Uint(NonZeroU8::new(1).unwrap()))),
    }
}
fn analyse_instruction(
    instruction: &Instruction,
    symbol_table: &mut SymbolTable,
) -> Result<(), String> {
    match instruction {
        Instruction::ASSIGN { ident, expr } => {
            let ident_typ = symbol_table
                .get(ident)
                .copied()
                .ok_or(format!("`{ident}` doesn't exist"))?;
            let expr_typ = analyse_expression(expr, symbol_table, ident_typ)?;
            match (ident_typ, expr_typ) {
                (None, None) => {
                    let typ = Some(Type::Uint(NonZeroU8::new(1).unwrap()));
                    *symbol_table.get_mut(ident).unwrap() = typ;
                    analyse_expression(expr, symbol_table, typ)?;
                    Ok(())
                }
                (None, Some(typ)) => {
                    *symbol_table.get_mut(ident).unwrap() = Some(typ);
                    Ok(())
                }
                (Some(_), None) => Err(format!("assign expression must have a type")),
                (Some(ident_typ), Some(expr_typ)) => {
                    if ident_typ != expr_typ {
                        Err(format!(
                            "assign expression type does not match var type: `{ident}: {ident_typ:?} = ({expr_typ:?})`"
                        ))
                    } else {
                        Ok(())
                    }
                }
            }
        }
        Instruction::LOOP { block } => {
            if let Some(instructions) = block {
                analyse_instruction(instructions, symbol_table)
            } else {
                Ok(())
            }
        }
        Instruction::BIN { lhs, rhs } => {
            analyse_instruction(lhs, symbol_table)?;
            analyse_instruction(rhs, symbol_table)
        }
    }
}

struct Ram {
    data: [u8; 64 * 1024],
    ptr: usize,
}
impl Ram {
    fn new() -> Self {
        let mut data = [0u8; 64 * 1024];
        data[0xFFFC] = 0x00;
        data[0xFFFD] = 0x01;
        let ptr = 0x0100;
        Self { data, ptr }
    }
    fn insert_u8(&mut self, data: u8) {
        self.data[self.ptr] = data;
        self.ptr += 1;
    }
    fn insert_u16(&mut self, data: u16) {
        let b = data.to_le_bytes();
        self.insert_u8(b[0]);
        self.insert_u8(b[1]);
    }
}

#[derive(Debug, Clone, Copy)]
enum State {
    Normal,
    Add,
    Sub,
}

fn gen_expression(expr: &Expression, ram: &mut Ram, mem_table: &MemTable, state: State) {
    match expr {
        Expression::CONSTANT { value, typ } => {
            if let Some(typ) = typ.get() {
                let mut bytes = value.to_le_bytes().to_vec();
                let size = match typ {
                    Type::Uint(size) => size.get(),
                };
                bytes.resize(size as usize, 0);

                // set carry bit (if needed)
                match state {
                    State::Normal => {}
                    State::Add => {
                        ram.insert_u8(0x18); // CLC_IMP
                    }
                    State::Sub => {
                        ram.insert_u8(0x38); // SEC_IMP
                    }
                }

                // loop bytes
                for i in 0..size {
                    // load
                    match state {
                        State::Normal => {
                            // load const byte
                            ram.insert_u8(0xA9); // LDA_IMM
                            ram.insert_u8(bytes[i as usize]);
                        }
                        _ => {
                            // load byte
                            ram.insert_u8(0xA5); // LDA_ZP0
                            ram.insert_u8(i);
                        }
                    }
                    // op
                    match state {
                        State::Normal => {}
                        State::Add => {
                            // add const byte
                            ram.insert_u8(0x69); // ADC_IMM
                            ram.insert_u8(bytes[i as usize]);
                        }
                        State::Sub => {
                            // sub const byte
                            ram.insert_u8(0xE9); // SBC_IMM
                            ram.insert_u8(bytes[i as usize]);
                        }
                    }
                    // save byte
                    ram.insert_u8(0x85); // STA_ZP0
                    ram.insert_u8(i);
                }
            } else {
                panic!("couldn't generate code");
            }
        }
        Expression::IDENTIFIER { ident } => {
            let var = &mem_table[ident];
            let size = match var.typ {
                Type::Uint(size) => size.get(),
            };

            // set carry bit (if needed)
            match state {
                State::Normal => {}
                State::Add => {
                    ram.insert_u8(0x18); // CLC_IMP
                }
                State::Sub => {
                    ram.insert_u8(0x38); // SEC_IMP
                }
            }

            // loop bytes
            for i in 0..size {
                // load
                match state {
                    State::Normal => {
                        // load ident byte
                        match var.addr {
                            AddrType::Abs(p) => {
                                let p = p + i as u16;
                                ram.insert_u8(0xAD); // LDA_ABS
                                ram.insert_u16(p);
                            }
                            AddrType::Zp(p) => {
                                let p = p + i;
                                ram.insert_u8(0xA5); // LDA_ZP0
                                ram.insert_u8(p);
                            }
                        };
                    }
                    _ => {
                        // load byte
                        ram.insert_u8(0xA5); // LDA_ZP0
                        ram.insert_u8(i);
                    }
                }
                // op
                match state {
                    State::Normal => {}
                    State::Add => {
                        // add ident byte
                        match var.addr {
                            AddrType::Abs(p) => {
                                let p = p + i as u16;
                                ram.insert_u8(0x6D); // ADC_ABS
                                ram.insert_u16(p);
                            }
                            AddrType::Zp(p) => {
                                let p = p + i;
                                ram.insert_u8(0x65); // ADC_ZP0
                                ram.insert_u8(p);
                            }
                        }
                    }
                    State::Sub => {
                        // sub ident byte
                        match var.addr {
                            AddrType::Abs(p) => {
                                let p = p + i as u16;
                                ram.insert_u8(0xED); // SBC_ABS
                                ram.insert_u16(p);
                            }
                            AddrType::Zp(p) => {
                                let p = p + i;
                                ram.insert_u8(0xE5); // SBC_ZP0
                                ram.insert_u8(p);
                            }
                        }
                    }
                }

                // save byte
                ram.insert_u8(0x85); // STA_ZP0
                ram.insert_u8(i);
            }
        }
        Expression::BINOP { lhs, op, rhs } => {
            gen_expression(lhs, ram, mem_table, state);
            let state = match op {
                Operator::ADD => State::Add,
                Operator::SUB => State::Sub,
            };
            gen_expression(rhs, ram, mem_table, state);
        }
        Expression::REFRENCE { ident } => {
            let p = match mem_table[ident].addr {
                AddrType::Abs(_) => panic!("absolute references aren't supported"),
                AddrType::Zp(p) => p,
            };
            gen_expression(
                &Expression::CONSTANT {
                    value: p as usize,
                    typ: Cell::new(Some(Type::Uint(NonZeroU8::new(1).unwrap()))),
                },
                ram,
                mem_table,
                state,
            );
        }
    }
}
fn gen_instruction(instruction: &Instruction, ram: &mut Ram, mem_table: &MemTable) {
    match instruction {
        Instruction::ASSIGN { ident, expr } => {
            gen_expression(expr, ram, mem_table, State::Normal);
            let var = &mem_table[ident];
            let size = match var.typ {
                Type::Uint(size) => size.get(),
            };
            for i in 0..size {
                ram.insert_u8(0xA5); // LDA_ZP0
                ram.insert_u8(i);
                match var.addr {
                    AddrType::Abs(p) => {
                        let p = p + i as u16;
                        ram.insert_u8(0x8D); // STA_ABS
                        ram.insert_u16(p);
                    }
                    AddrType::Zp(p) => {
                        let p = p + i;
                        ram.insert_u8(0x85); // STA_ZP0
                        ram.insert_u8(p);
                    }
                };
            }
        }
        Instruction::LOOP { block } => {
            let ptr = ram.ptr;
            if let Some(instructions) = block {
                gen_instruction(instructions, ram, mem_table);
            } else {
                ram.insert_u8(0xEA); // NOP
            }
            ram.insert_u8(0x4C); // JMP_ABS
            ram.insert_u16(ptr as u16);
        }
        Instruction::BIN { lhs, rhs } => {
            gen_instruction(lhs, ram, mem_table);
            gen_instruction(rhs, ram, mem_table);
        }
    }
}

fn main() {
    let file = std::env::args().nth(1).expect("No file given");
    let path = std::path::Path::new(&file);
    let code = std::fs::read_to_string(path).expect("Could not read file");

    let (instruction, mut symbol_table) = parse(&code);
    symbol_table.insert(
        Rc::from("out"),
        Some(Type::Uint(NonZeroU8::new(1).unwrap())),
    );
    analyse_instruction(&instruction, &mut symbol_table).unwrap();
    println!("{instruction:#?}");

    // get max var size for working memory
    let mut i: u8 = 0;
    for (ident, typ) in symbol_table.iter() {
        match typ.expect(&format!("`{ident}` deos not have type")) {
            Type::Uint(size) => i = i.max(size.get()),
        }
    }
    println!("Working memory size: {i} bytes");

    let mut mem_table = HashMap::new();

    for (ident, typ) in symbol_table.iter() {
        match typ.expect(&format!("`{ident}` deos not have type")) {
            Type::Uint(size) => {
                println!("ident: {ident }, addr: {i:#04X}, size: {size}");

                let var = if &**ident == "out" {
                    Var {
                        addr: AddrType::Abs(0x5000),
                        typ: typ.unwrap(),
                    }
                } else {
                    let var = Var {
                        addr: AddrType::Zp(i),
                        typ: typ.unwrap(),
                    };
                    i += size.get();
                    var
                };
                mem_table.insert(ident.clone(), var);
            }
        }
    }

    let mut ram = Ram::new();

    gen_instruction(&instruction, &mut ram, &mem_table);

    let ptr = ram.ptr as u16;
    ram.insert_u8(0xEA); // NOP
    ram.insert_u8(0x4C); // JMP_ABS
    ram.insert_u16(ptr);

    std::fs::write(
        format!(
            "{}.bin",
            path.file_stem()
                .expect("Could not get file name")
                .to_str()
                .unwrap()
        ),
        ram.data,
    )
    .unwrap();
}
