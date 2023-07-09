#![allow(clippy::upper_case_acronyms)]

use std::{cell::RefCell, rc::Rc};

mod tokenizer;
pub use tokenizer::*;

// #[derive(Debug, Clone)]
// struct Expression {
//     typ: Option<Type>,
//     expr: ExpressionType,
// }

#[derive(Debug, Clone)]
pub enum Expression {
    CONSTANT {
        value: usize,
        typ: RefCell<Option<Type>>,
        // typ: Option<Type>,
    },
    IDENTIFIER {
        id: Rc<str>,
    },
    REFERENCE {
        id: Rc<str>,
    },
    // DEREFERENCE {
    // id: Rc<str>,
    // },
    BINOP {
        lhs: Rc<Self>,
        op: Operator,
        rhs: Rc<Self>,
    },
}
#[derive(Debug, Clone)]
pub enum Instruction {
    ASSIGN {
        id: Rc<str>,
        expr: Expression,
    },
    DECLARE {
        id: Rc<str>,
        expr: Expression,
        typ: Option<Type>,
        addr_mode: AddrMode,
    },
    LOOP {
        block: Option<Rc<Self>>,
    },
    BIN {
        lhs: Rc<Self>,
        rhs: Rc<Self>,
    },
}

#[derive(Debug, Clone)]
pub enum AST {
    EXPRESSION(Expression),
    INSTRUCTION(Instruction),
    BLOCK(Option<Instruction>),
}

#[derive(Debug, Clone)]
pub enum Node {
    Token(Token),
    AST(AST),
    Type(Type),
}

pub fn parse(code: &str) -> Instruction {
    let mut stack = Vec::new();
    let mut tokenizer = Tokenizer::new(code).peekable();

    // let mut symbol_table = SymbolTable::new();

    while let Some(token) = tokenizer.next() {
        stack.push(Node::Token(token));

        loop {
            let stack_len = stack.len();
            if !match &stack[..] {
                // combine instructions
                [.., Node::AST(AST::INSTRUCTION(lhs)), Node::AST(AST::INSTRUCTION(rhs))] => {
                    let lhs = Rc::new(lhs.clone());
                    let rhs = Rc::new(rhs.clone());

                    stack.truncate(stack_len - 2);

                    let instructions = Instruction::BIN { lhs, rhs };
                    stack.push(Node::AST(AST::INSTRUCTION(instructions)));

                    true
                }
                // token number -> ast constant
                [.., Node::Token(Token::NUMBER(n))] => {
                    let n = *n;

                    stack.pop();

                    let typ = RefCell::new(None);
                    let expr = Expression::CONSTANT { value: n, typ };
                    stack.push(Node::AST(AST::EXPRESSION(expr)));

                    true
                }
                // token type -> ast type
                [.., Node::Token(Token::TYPE(typ))] => {
                    let typ = typ.clone();

                    stack.pop();

                    stack.push(Node::Type(typ));

                    true
                }
                // constructing reference types
                [.., Node::Token(Token::POINTER(addr)), Node::Type(typ)] => {
                    let typ = Rc::new(typ.clone());
                    let addr = *addr;

                    stack.truncate(stack_len - 2);

                    let typ = Type::Ref { typ, addr };
                    stack.push(Node::Type(typ));

                    true
                }
                // reference
                [.., Node::Token(Token::REFERENCE), Node::Token(Token::IDENTIFIER(id))] => {
                    let id = id.clone();

                    stack.truncate(stack_len - 2);

                    // let typ = Some(symbol_table.get(&id)?.typ);

                    let expr = Expression::REFERENCE { id };
                    stack.push(Node::AST(AST::EXPRESSION(expr)));

                    true
                }
                // // dereference
                // [.., Node::Token(Token::DEREFERENCE), Node::Token(Token::IDENTIFIER(id))] => {
                //     let id = id.clone();

                //     stack.truncate(stack_len - 2);

                //     stack.push(Node::AST(AST::EXPRESSION(Rc::new(
                //         ExpressionType::DEREFERENCE { id },
                //     ))));
                //     true
                // }

                // token id -> ast id
                [.., Node::Token(Token::IDENTIFIER(id))] => {
                    if let Some(Token::ASSIGN | Token::COLON) = tokenizer.peek() {
                        false
                    } else {
                        let id = id.clone();

                        stack.pop();

                        // let typ = Some(symbol_table.get(&id)?.typ);
                        let expr = Expression::IDENTIFIER { id };
                        stack.push(Node::AST(AST::EXPRESSION(expr)));
                        true
                    }
                }
                // (expr)
                [.., Node::Token(Token::OPEN), Node::AST(expr @ AST::EXPRESSION(_)), Node::Token(Token::CLOSE)] =>
                {
                    let expr = expr.clone();

                    stack.truncate(stack_len - 3);

                    stack.push(Node::AST(expr));

                    true
                }
                // expr op expr
                [.., Node::AST(AST::EXPRESSION(lhs)), Node::Token(Token::OPERATOR(op)), Node::AST(AST::EXPRESSION(rhs))] =>
                {
                    let lhs = Rc::new(lhs.clone());
                    let rhs = Rc::new(rhs.clone());
                    let op = *op;

                    stack.truncate(stack_len - 3);

                    let expr = Expression::BINOP { lhs, op, rhs };
                    stack.push(Node::AST(AST::EXPRESSION(expr)));

                    true
                }
                // `adr` var `id`: `type` = expr;
                [.., Node::Token(Token::ADDRTYPE(addr)), Node::Token(Token::VAR), Node::Token(Token::IDENTIFIER(id)), Node::Token(Token::COLON), Node::Type(typ), Node::Token(Token::ASSIGN), Node::AST(AST::EXPRESSION(expr)), Node::Token(Token::SEMICOLON)] =>
                {
                    let id = id.clone();
                    let expr = expr.clone();
                    let typ = Some(typ.clone());
                    let addr = *addr;

                    stack.truncate(stack_len - 8);

                    let instruction = Instruction::DECLARE {
                        id,
                        expr,
                        typ,
                        addr_mode: addr,
                    };
                    stack.push(Node::AST(AST::INSTRUCTION(instruction)));

                    true
                }
                // `adr` var `id` = expr;
                [.., Node::Token(Token::ADDRTYPE(addr)), Node::Token(Token::VAR), Node::Token(Token::IDENTIFIER(id)), Node::Token(Token::ASSIGN), Node::AST(AST::EXPRESSION(expr)), Node::Token(Token::SEMICOLON)] =>
                {
                    let id = id.clone();
                    let expr = expr.clone();
                    let addr = *addr;

                    stack.truncate(stack_len - 6);

                    let typ = None;
                    let instruction = Instruction::DECLARE {
                        id,
                        expr,
                        typ,
                        addr_mode: addr,
                    };
                    stack.push(Node::AST(AST::INSTRUCTION(instruction)));

                    true
                }
                // var `id`: `type` = expr;
                [.., Node::Token(Token::VAR), Node::Token(Token::IDENTIFIER(id)), Node::Token(Token::COLON), Node::Type(typ), Node::Token(Token::ASSIGN), Node::AST(AST::EXPRESSION(expr)), Node::Token(Token::SEMICOLON)] =>
                {
                    let id = id.clone();
                    let expr = expr.clone();
                    let typ = Some(typ.clone());

                    stack.truncate(stack_len - 7);

                    let addr = AddrMode::ZPG;
                    let instruction = Instruction::DECLARE {
                        id,
                        expr,
                        typ,
                        addr_mode: addr,
                    };
                    stack.push(Node::AST(AST::INSTRUCTION(instruction)));

                    true
                }
                // var `id` = expr;
                [.., Node::Token(Token::VAR), Node::Token(Token::IDENTIFIER(id)), Node::Token(Token::ASSIGN), Node::AST(AST::EXPRESSION(expr)), Node::Token(Token::SEMICOLON)] =>
                {
                    let id = id.clone();
                    let expr = expr.clone();

                    stack.truncate(stack_len - 5);

                    let typ = None;
                    let addr = AddrMode::ZPG;
                    let instruction = Instruction::DECLARE {
                        id,
                        expr,
                        typ,
                        addr_mode: addr,
                    };
                    stack.push(Node::AST(AST::INSTRUCTION(instruction)));

                    true
                }
                // `id` = expr
                [.., Node::Token(Token::IDENTIFIER(id)), Node::Token(Token::ASSIGN), Node::AST(AST::EXPRESSION(expr)), Node::Token(Token::SEMICOLON)] =>
                {
                    let id = id.clone();
                    let expr = expr.clone();

                    stack.truncate(stack_len - 4);

                    let instruction = Instruction::ASSIGN { id, expr };
                    stack.push(Node::AST(AST::INSTRUCTION(instruction)));

                    true
                }
                // {}
                [.., Node::Token(Token::BOPEN), Node::Token(Token::BCLOSE)] => {
                    stack.truncate(stack_len - 2);

                    stack.push(Node::AST(AST::BLOCK(None)));

                    true
                }
                // {instructions}
                [.., Node::Token(Token::BOPEN), Node::AST(AST::INSTRUCTION(instructions)), Node::Token(Token::BCLOSE)] =>
                {
                    let instructions = instructions.clone();

                    stack.truncate(stack_len - 3);

                    stack.push(Node::AST(AST::BLOCK(Some(instructions))));

                    true
                }
                // loop {block}
                [.., Node::Token(Token::LOOP), Node::AST(AST::BLOCK(block))] => {
                    let block = block.clone().map(Rc::new);

                    stack.truncate(stack_len - 2);

                    let instruction = Instruction::LOOP { block };
                    stack.push(Node::AST(AST::INSTRUCTION(instruction)));

                    true
                }
                _ => false,
            } {
                break;
            }
        }
    }

    for node in &stack {
        println!("{node:#?}");
    }

    if stack.len() == 1 {
        if let Node::AST(AST::INSTRUCTION(ast)) = stack.pop().unwrap() {
            ast
        } else {
            panic!("node is not ast or instruction")
        }
    } else {
        panic!("stack is bigger then 1");
    }
}
