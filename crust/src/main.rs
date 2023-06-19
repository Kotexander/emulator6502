use std::collections::HashMap;
use std::rc::Rc;
use std::{
    iter::Peekable,
    str::{CharIndices, FromStr},
};

// #[derive(Debug, Clone)]
// struct Identifier {
// ident: String,
// }

#[derive(Debug, Clone, Copy)]
enum Operator {
    ADD,
    SUB,
    // MUL,
    // DIV,
}

#[derive(Debug, Clone)]
enum Token {
    NUMBER(i32),
    OPERATOR(Operator),
    IDENTIFIER(Rc<str>),
    VAR,
    ASSIGN,
    SEMICOLON,
    OPEN,
    CLOSE,
    LOOP,
    BOPEN,
    BCLOSE,
}

fn symbol(c: char) -> Option<Token> {
    match c {
        '(' => Some(Token::OPEN),
        ')' => Some(Token::CLOSE),
        '{' => Some(Token::BOPEN),
        '}' => Some(Token::BCLOSE),
        ';' => Some(Token::SEMICOLON),
        '=' => Some(Token::ASSIGN),
        '+' => Some(Token::OPERATOR(Operator::ADD)),
        '-' => Some(Token::OPERATOR(Operator::SUB)),
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

        while let Some((i, c)) = self.code_chars.peek() {
            if c.is_whitespace() || symbol(*c).is_some() {
                break;
            }
            let (i, c) = self.code_chars.next().unwrap();
            end = i;
        }

        let token = &self.code[start..=end];

        if let Ok(num) = i32::from_str(token) {
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
enum AST {
    CONSTANT(i32),
    IDENTIFIER(Rc<str>),
    EXPRESSION(Rc<AST>),
    BINOP {
        lhs: Rc<AST>,
        op: Operator,
        rhs: Rc<AST>,
    },
    // DECLARE {
    // ident: Rc<str>,
    // expr: Rc<AST>,
    // },
    ASSIGN {
        ident: Rc<str>,
        expr: Rc<AST>,
    },
    LOOP {
        instructions: Rc<AST>,
    },
    INSTRUCTION {
        instruction: Rc<AST>,
    },
    BININSTRUCTION {
        instruction1: Rc<AST>,
        instruction2: Rc<AST>,
    }, // FUNCTION {
       // ident: Rc<str>,
       // args: Rc<AST>,
       // },
}

#[derive(Debug)]
enum Node {
    Token(Token),
    AST(AST),
}

// E <- E +/- T | T
// T <- T * F   | F
// F <- E       | id

fn parse(code: &str) -> (AST, HashMap<Rc<str>, ()>) {
    let mut stack = Vec::new();
    let mut tokenizer = Tokenizer::new(code).peekable();

    let mut symbol_table = HashMap::new();

    while let Some(token) = tokenizer.next() {
        stack.push(Node::Token(token));

        while match &stack[..] {
            [.., Node::AST(
                instruction @ (AST::ASSIGN { .. } | AST::LOOP { .. } | AST::BININSTRUCTION { .. }),
            )] => {
                let instruction = Rc::new(instruction.clone());
                stack.pop();
                stack.push(Node::AST(AST::INSTRUCTION { instruction }));
                true
            }
            [.., Node::AST(instruction1 @ AST::INSTRUCTION { .. }), Node::AST(instruction2 @ AST::INSTRUCTION { .. })] =>
            {
                let instruction1 = Rc::new(instruction1.clone());
                let instruction2 = Rc::new(instruction2.clone());
                stack.pop();
                stack.pop();
                stack.push(Node::AST(AST::BININSTRUCTION {
                    instruction1,
                    instruction2,
                }));
                true
            }
            [.., Node::Token(Token::NUMBER(n))] => {
                let n = *n;
                stack.pop();
                stack.push(Node::AST(AST::CONSTANT(n)));
                true
            }
            [.., Node::Token(Token::IDENTIFIER(x))] => {
                let x = x.clone();
                stack.pop();
                stack.push(Node::AST(AST::IDENTIFIER(x)));
                true
            }

            [.., Node::AST(expr @ AST::CONSTANT(_))] => {
                let expr = Rc::new(expr.clone());
                stack.pop();
                stack.push(Node::AST(AST::EXPRESSION(expr)));
                true
            }
            [.., Node::AST(expr @ AST::IDENTIFIER(_))] => {
                if let Some(Token::ASSIGN | Token::OPEN) = tokenizer.peek() {
                    false
                } else {
                    let expr = Rc::new(expr.clone());
                    stack.pop();
                    stack.push(Node::AST(AST::EXPRESSION(expr)));
                    true
                }
            }
            [.., Node::AST(binop @ AST::BINOP { .. })] => {
                let binop = Rc::new(binop.clone());
                stack.pop();
                stack.push(Node::AST(AST::EXPRESSION(binop)));
                true
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

            [.., Node::AST(lhs @ AST::EXPRESSION(_)), Node::Token(Token::OPERATOR(op)), Node::AST(rhs @ AST::EXPRESSION(_))] =>
            {
                let lhs = Rc::new(lhs.clone());
                let rhs = Rc::new(rhs.clone());
                let op = *op;
                stack.pop();
                stack.pop();
                stack.pop();

                stack.push(Node::AST(AST::BINOP { lhs, op, rhs }));
                true
            }

            [.., Node::Token(Token::VAR), Node::AST(AST::IDENTIFIER(ident)), Node::Token(Token::ASSIGN), Node::AST(expr @ AST::EXPRESSION(_)), Node::Token(Token::SEMICOLON)] =>
            {
                let ident = ident.clone();
                let expr = Rc::new(expr.clone());
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();

                symbol_table.insert(ident.clone(), ());
                stack.push(Node::AST(AST::ASSIGN { ident, expr }));
                true
            }
            [.., Node::AST(AST::IDENTIFIER(ident)), Node::Token(Token::ASSIGN), Node::AST(expr @ AST::EXPRESSION(_)), Node::Token(Token::SEMICOLON)] =>
            {
                let ident = ident.clone();
                let expr = Rc::new(expr.clone());
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();

                stack.push(Node::AST(AST::ASSIGN { ident, expr }));
                true
            }
            [.., Node::Token(Token::LOOP), Node::Token(Token::BOPEN), Node::AST(instructions @ AST::INSTRUCTION { .. }), Node::Token(Token::BCLOSE)] =>
            {
                let instructions = Rc::new(instructions.clone());
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();

                stack.push(Node::AST(AST::LOOP { instructions }));
                true
            }

            // [.., Node::AST(AST::FUNCTION { .. })]
            _ => false,
        } {}
    }

    if stack.len() == 1 {
        if let Node::AST(ast) = stack.pop().unwrap() {
            println!("{ast:#?}");
            (ast, symbol_table)
        } else {
            panic!("node is not ast")
        }
    } else {
        panic!("stack is bigger then 1");
    }
}

#[derive(Debug, Clone, Copy)]
enum State {
    Normal,
    Add,
    Sub,
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

fn gen_code(ast: &AST, ram: &mut Ram, mem_table: &HashMap<Rc<str>, VarType>, state: State) {
    match ast {
        AST::CONSTANT(n) => {
            match state {
                State::Normal => {
                    ram.insert_u8(0xA9); // LDA_IMM
                    ram.insert_u8(*n as u8);
                }
                State::Add => {
                    ram.insert_u8(0x18); // CLC_IMP
                    ram.insert_u8(0x69); // ADC_IMM
                    ram.insert_u8(*n as u8);
                }
                State::Sub => {
                    ram.insert_u8(0x38); // SEC_IMP
                    ram.insert_u8(0xE9); // SBC_IMM
                    ram.insert_u8(*n as u8);
                }
            }
        }
        AST::IDENTIFIER(x) => {
            match state {
                State::Normal => {
                    match mem_table[x] {
                        VarType::Abs(x) => {
                            ram.insert_u8(0xAD); // LDA_ABS
                            ram.insert_u16(x);
                        }
                        VarType::Zp(x) => {
                            ram.insert_u8(0xA5); // LDA_ZP0
                            ram.insert_u8(x);
                        }
                    };
                }
                State::Add => {
                    ram.insert_u8(0x18); // CLC_IMP
                    match mem_table[x] {
                        VarType::Abs(x) => {
                            ram.insert_u8(0x6D); // ADC_ABS
                            ram.insert_u16(x);
                        }
                        VarType::Zp(x) => {
                            ram.insert_u8(0x65); // ADC_ZP0
                            ram.insert_u8(x);
                        }
                    };
                }
                State::Sub => {
                    ram.insert_u8(0x38); // SEC_IMP
                    match mem_table[x] {
                        VarType::Abs(x) => {
                            ram.insert_u8(0xED); // SBC_ABS
                            ram.insert_u16(x);
                        }
                        VarType::Zp(x) => {
                            ram.insert_u8(0xE5); // SBC_ZP0
                            ram.insert_u8(x);
                        }
                    };
                }
            }
        }
        AST::EXPRESSION(expr) => {
            gen_code(expr, ram, mem_table, state);
        }
        AST::BINOP { lhs, op, rhs } => {
            gen_code(lhs, ram, mem_table, state);
            let state = match op {
                Operator::ADD => State::Add,
                Operator::SUB => State::Sub,
            };
            gen_code(rhs, ram, mem_table, state);
        }
        AST::ASSIGN { ident, expr } => {
            // assert_eq!(state, State::Normal);
            gen_code(expr, ram, mem_table, State::Normal);
            match mem_table[ident] {
                VarType::Abs(x) => {
                    ram.insert_u8(0x8D); // STA_ABS

                    ram.insert_u16(x);
                }
                VarType::Zp(x) => {
                    ram.insert_u8(0x85); // STA_ZP0
                    ram.insert_u8(x);
                }
            };
        }
        AST::LOOP { instructions } => {
            let ptr = ram.ptr;
            gen_code(instructions, ram, mem_table, state);
            ram.insert_u8(0x4C); // JMP_ABS
            ram.insert_u16(ptr as u16);
        }
        AST::INSTRUCTION { instruction } => {
            gen_code(instruction, ram, mem_table, state);
        }
        AST::BININSTRUCTION {
            instruction1,
            instruction2,
        } => {
            gen_code(instruction1, ram, mem_table, state);
            gen_code(instruction2, ram, mem_table, state);
        }
    }
}

enum VarType {
    Abs(u16),
    Zp(u8),
}

fn main() {
    let code = r#"
var x=43-1;
var y = 30-7;

x=x+(23   +    y +(24-     1)         );

out = x;
out = y;
"#;

    //     let fibinachi = r#"
    // var a = 1;
    // var b = 0;
    // var c = 0;

    // loop {
    //     c = a + b;
    //     a = b;
    //     b = c;

    //     out = c;
    // }
    // "#;

    let fibinachi = r#"
a = 1;
b = 0;
c = 0;

loop {
    c = a + b;
    a = b;
    b = c;

    out = c;
}
var a=0;
var b=0;
var c=0;
"#;

    // println(x)
    // loop {
    //     y = y + 1;
    //     println(y);
    // }

    let (tree, symbol_table) = parse(fibinachi);
    let mut mem_table = HashMap::new();
    mem_table.insert(Rc::from("out"), VarType::Abs(0x5000));
    let mut i = 0u8;
    for (ident, _) in symbol_table.iter() {
        mem_table.insert(ident.clone(), VarType::Zp(i));
        i += 1;
    }
    let mut ram = Ram::new();

    // for instruction in tree {
    gen_code(&tree, &mut ram, &mem_table, State::Normal);
    // }
    let ptr = ram.ptr as u16;
    ram.insert_u8(0xEA); // NOP
    ram.insert_u8(0x4C); // JMP_ABS
    ram.insert_u16(ptr);

    std::fs::write("a.bin", ram.data).unwrap();

    // for token in Tokenizer::new(code) {
    // println!("{:?}", token);
    // }
    // println!("{:#?}", parse(code));
}
