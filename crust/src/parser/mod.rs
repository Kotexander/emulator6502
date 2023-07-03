#![allow(clippy::upper_case_acronyms)]

use std::{cell::RefCell, rc::Rc};

mod tokenizer;
pub use tokenizer::*;

#[derive(Debug, Clone)]
pub enum Expression {
    CONSTANT {
        value: usize,
        typ: RefCell<Option<Type>>,
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
pub enum Instruction {
    ASSIGN {
        ident: Rc<str>,
        expr: Rc<Expression>,
    },
    DECLARE {
        ident: Rc<str>,
        expr: Rc<Expression>,
        typ: Option<Type>,
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
    EXPRESSION(Rc<Expression>),
    INSTRUCTION(Rc<Instruction>),
    BLOCK(Option<Rc<Instruction>>),
}

#[derive(Debug, Clone)]
pub enum Node {
    Token(Token),
    AST(AST),
    Type(Type),
}

pub fn parse(code: &str) -> Rc<Instruction> {
    let mut stack = Vec::new();
    let mut tokenizer = Tokenizer::new(code).peekable();

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
                        typ: RefCell::new(None),
                    }))));
                    true
                }
            }
            [.., Node::Token(Token::TYPE(typ))] => {
                let typ = typ.clone();
                stack.pop();
                stack.push(Node::Type(typ));
                true
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
            [.., Node::Token(Token::VAR), Node::Token(Token::IDENTIFIER(ident)), Node::Token(Token::COLON), Node::Type(typ), Node::Token(Token::ASSIGN), Node::AST(AST::EXPRESSION(expr)), Node::Token(Token::SEMICOLON)] =>
            {
                let ident = ident.clone();
                let expr = expr.clone();
                let typ = Some(typ.clone());
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();
                stack.pop();

                stack.push(Node::AST(AST::INSTRUCTION(Rc::new(Instruction::DECLARE {
                    ident,
                    expr,
                    typ,
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

                let typ = None;

                stack.push(Node::AST(AST::INSTRUCTION(Rc::new(Instruction::DECLARE {
                    ident,
                    expr,
                    typ,
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
