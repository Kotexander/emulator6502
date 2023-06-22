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
}
enum AddrType {
    Abs(u16),
    Zp(u8),
}
struct Var {
    addr: AddrType,
    typ: Type,
}

type SymbolTable = HashMap<Rc<str>, Type>;
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

                let typ = Type::Uint(NonZeroU8::new(n as u8).expect("size can't be zero"));

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

                symbol_table.insert(ident.clone(), Type::Uint(NonZeroU8::new(1).unwrap()));
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

fn analyse_expression(expr: &Expression, symbol_table: &SymbolTable, imp_typ: Type) -> Type {
    match expr {
        Expression::CONSTANT { value: _, typ } => {
            if let Some(typ) = typ.get() {
                typ
            } else {
                // if let Some(imp_typ) = imp_typ {
                typ.set(Some(imp_typ));
                imp_typ
                // } else {
                // panic!("Could not determin type of constant");
                // }
            }
        }
        Expression::IDENTIFIER { ident } => symbol_table[ident],
        Expression::BINOP { lhs, op: _, rhs } => {
            let lhs_typ = analyse_expression(lhs, symbol_table, imp_typ);
            let rhs_typ = analyse_expression(rhs, symbol_table, imp_typ);
            if lhs_typ == rhs_typ {
                lhs_typ
            } else {
                panic!("bin op types don't match");
            }
        }
        Expression::REFRENCE { ident } => Type::Uint(NonZeroU8::new(1).unwrap()),
    }
}
fn analyse_instruction(instruction: &Instruction, symbol_table: &SymbolTable) {
    match instruction {
        Instruction::ASSIGN { ident, expr } => {
            analyse_expression(expr, symbol_table, symbol_table[ident]);
        }
        Instruction::LOOP { block } => {
            if let Some(instructions) = block {
                analyse_instruction(instructions, symbol_table);
            }
        }
        Instruction::BIN { lhs, rhs } => {
            analyse_instruction(lhs, symbol_table);
            analyse_instruction(rhs, symbol_table);
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

fn gen_expression(
    expr: &Expression,
    ram: &mut Ram,
    mem_table: &MemTable,
    state: State,
    // typ: Type,
) {
    match expr {
        Expression::CONSTANT { value, typ } => {
            if let Some(typ) = typ.get() {
                let mut b = value.to_le_bytes().to_vec();
                let size = match typ {
                    Type::Uint(size) => size.get(),
                };
                b.resize(size as usize, 0);
                match state {
                    State::Normal => {
                        for i in 0..size {
                            ram.insert_u8(0xA9); // LDA_IMM
                            ram.insert_u8(b[i as usize]);
                            ram.insert_u8(0x85); // STA_ZP0
                            ram.insert_u8(i as u8);
                        }
                        // ram.insert_u8(0x85); // STA_ZP0
                        // ram.insert_u8(i as u8);
                    }
                    State::Add => {
                        ram.insert_u8(0x18); // CLC_IMP
                        for i in 0..size {
                            ram.insert_u8(0xA5); // LDA_ZP0
                            ram.insert_u8(i as u8);
                            ram.insert_u8(0x69); // ADC_IMM
                            ram.insert_u8(b[i as usize]);
                            ram.insert_u8(0x85); // STA_ZP0
                            ram.insert_u8(i as u8);
                        }

                        // ram.insert_u8(0x18); // CLC_IMP
                        // ram.insert_u8(0x69); // ADC_IMM
                        // ram.insert_u8(*n as u8);
                    }
                    State::Sub => {
                        ram.insert_u8(0x38); // SEC_IMP
                        for i in 0..size {
                            ram.insert_u8(0xA5); // LDA_ZP0
                            ram.insert_u8(i as u8);
                            ram.insert_u8(0xE9); // SBC_IMM
                            ram.insert_u8(b[i as usize]);
                            ram.insert_u8(0x85); // STA_ZP0
                            ram.insert_u8(i as u8);
                        }

                        // ram.insert_u8(0x38); // SEC_IMP
                        // ram.insert_u8(0xE9); // SBC_IMM
                        // ram.insert_u8(*n as u8);
                    }
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
            match state {
                State::Normal => {
                    for i in 0..size {
                        match var.addr {
                            AddrType::Abs(p) => {
                                let p = p + i as u16;
                                ram.insert_u8(0xAD); // LDA_ABS
                                ram.insert_u16(p);
                                ram.insert_u8(0x8D); // STA_ABS
                                ram.insert_u16(i as u16);
                            }
                            AddrType::Zp(p) => {
                                let p = p + i;
                                ram.insert_u8(0xA5); // LDA_ZP0
                                ram.insert_u8(p);
                                ram.insert_u8(0x85); // STA_ZP0
                                ram.insert_u8(i as u8);
                            }
                        };
                    }
                    // match mem_table[x] {
                    //     VarType::Abs(x) => {
                    //         ram.insert_u8(0xAD); // LDA_ABS
                    //         ram.insert_u16(x);
                    //     }
                    //     VarType::Zp(x) => {
                    //         ram.insert_u8(0xA5); // LDA_ZP0
                    //         ram.insert_u8(x);
                    //     }
                    // };
                }
                State::Add => {
                    ram.insert_u8(0x18); // CLC_IMP
                    for i in 0..size {
                        match var.addr {
                            AddrType::Abs(p) => {
                                let p = p + i as u16;
                                ram.insert_u8(0xA5); // LDA_ZP0
                                ram.insert_u8(i as u8);
                                ram.insert_u8(0x6D); // ADC_ABS
                                ram.insert_u16(p);
                                ram.insert_u8(0x85); // STA_ZP0
                                ram.insert_u8(i as u8);
                            }
                            AddrType::Zp(p) => {
                                let p = p + i;
                                ram.insert_u8(0xA5); // LDA_ZP0
                                ram.insert_u8(i as u8);
                                ram.insert_u8(0x65); // ADC_ZP0
                                ram.insert_u8(p);
                                ram.insert_u8(0x85); // STA_ZP0
                                ram.insert_u8(i as u8);
                            }
                        };
                    }
                    // ram.insert_u8(0x18); // CLC_IMP
                    // match mem_table[x] {
                    //     VarType::Abs(x) => {
                    //         ram.insert_u8(0x6D); // ADC_ABS
                    //         ram.insert_u16(x);
                    //     }
                    //     VarType::Zp(x) => {
                    //         ram.insert_u8(0x65); // ADC_ZP0
                    //         ram.insert_u8(x);
                    //     }
                    // };
                }
                State::Sub => {
                    ram.insert_u8(0x38); // SEC_IMP
                    for i in 0..size {
                        match var.addr {
                            AddrType::Abs(p) => {
                                let p = p + i as u16;
                                ram.insert_u8(0xA5); // LDA_ZP0
                                ram.insert_u8(i as u8);
                                ram.insert_u8(0xED); // SBC_ABS
                                ram.insert_u16(p);
                                ram.insert_u8(0x85); // STA_ZP0
                                ram.insert_u8(i as u8);
                            }
                            AddrType::Zp(p) => {
                                let p = p + i;
                                ram.insert_u8(0xA5); // LDA_ZP0
                                ram.insert_u8(i as u8);
                                ram.insert_u8(0xE5); // SBC_ZP0
                                ram.insert_u8(p);
                                ram.insert_u8(0x85); // STA_ZP0
                                ram.insert_u8(i as u8);
                            }
                        };
                    }
                    // ram.insert_u8(0x38); // SEC_IMP
                    // match mem_table[x] {
                    //     VarType::Abs(x) => {
                    //         ram.insert_u8(0xED); // SBC_ABS
                    //         ram.insert_u16(x);
                    //     }
                    //     VarType::Zp(x) => {
                    //         ram.insert_u8(0xE5); // SBC_ZP0
                    //         ram.insert_u8(x);
                    //     }
                    // };
                }
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
                match var.addr {
                    AddrType::Abs(p) => {
                        let p = p + i as u16;
                        ram.insert_u8(0xA5); // LDA_ZP0
                        ram.insert_u8(i);
                        ram.insert_u8(0x8D); // STA_ABS
                        ram.insert_u16(p);
                    }
                    AddrType::Zp(p) => {
                        let p = p + i;
                        ram.insert_u8(0xA5); // LDA_ZP0
                        ram.insert_u8(i);
                        ram.insert_u8(0x85); // STA_ZP0
                        ram.insert_u8(p);
                    }
                };
            }

            // match mem_table[ident] {
            // VarType::Abs(x) => {
            // ram.insert_u8(0x8D); // STA_ABS
            // ram.insert_u16(x);
            // }
            // VarType::Zp(x) => {
            // ram.insert_u8(0x85); // STA_ZP0
            // ram.insert_u8(x);
            // }
            // };
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

    let mut mem_table = HashMap::new();
    mem_table.insert(
        Rc::from("out"),
        Var {
            addr: AddrType::Abs(0x5000),
            typ: Type::Uint(NonZeroU8::new(2).unwrap()),
        },
    );

    let mut i: u8 = 0;
    for (id, typ) in symbol_table.iter() {
        match typ {
            Type::Uint(size) => i = i.max(size.get()),
        }
    }
    println!("Temp size: {i}");

    for (ident, typ) in symbol_table.iter() {
        println!("id: {ident } addr: {i}");
        match typ {
            Type::Uint(size) => {
                mem_table.insert(
                    ident.clone(),
                    Var {
                        addr: AddrType::Zp(i),
                        typ: *typ,
                    },
                );
                i += size.get();
            }
        }
    }
    symbol_table.insert(Rc::from("out"), Type::Uint(NonZeroU8::new(1).unwrap()));

    let mut ram = Ram::new();
    analyse_instruction(&instruction, &symbol_table);
    println!("{instruction:#?}");
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
