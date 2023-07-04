use std::cell::RefCell;
use std::collections::HashMap;
use std::num::NonZeroU8;
use std::rc::Rc;

mod parser;
use parser::*;

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

/// `imp_typ` is used for implied type incase type is not defined
///
/// Returns the type of expression
fn analyse_expression(
    expr: &Expression,
    symbol_table: &SymbolTable,
    imp_typ: Option<Type>,
) -> Result<Option<Type>, String> {
    match expr {
        Expression::CONSTANT { value: _, typ } => {
            let expr_typ = typ.borrow().clone();

            match (imp_typ, expr_typ) {
                (None, None) => Ok(None), // leave it for now (tried again later)
                (_, Some(exp_typ)) => Ok(Some(exp_typ)), // ignore implied
                (Some(imp_typ), None) => {
                    *typ.borrow_mut() = Some(imp_typ.clone());
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
                // .copied()
                .ok_or(format!("undeclared variable: {ident}"))?
                .clone();

            Ok(Some(ident_typ))

            // match (imp_typ, ident_typ) {
            // (None, None) => Ok(None), // leave it for now
            // (_, _) => Ok(Some(ident_typ)),
            // (Some(_), _) => Ok(imp_typ),
            // }
        }
        Expression::BINOP { lhs, op, rhs } => {
            // get expected definate types
            let lhs_typ1 = analyse_expression(lhs, symbol_table, imp_typ.clone())?;
            let rhs_typ1 = analyse_expression(rhs, symbol_table, imp_typ)?;

            // set definate types
            let lhs_typ2 = analyse_expression(lhs, symbol_table, rhs_typ1)?;
            // .ok_or(format!("operad on lhs does not have a type"))?;
            let rhs_typ2 = analyse_expression(rhs, symbol_table, lhs_typ1)?;
            // .ok_or(format!("operad on rhs does not have a type"))?;

            // its fine of both sides are none
            if lhs_typ2 == rhs_typ2 {
                // Ok(Some(lhs_typ2))
                Ok(lhs_typ2)
            } else {
                let lhs = lhs_typ2.map_or(format!("(Unknown)"), |v| format!("{v}"));
                let rhs = rhs_typ2.map_or(format!("(Unknown)"), |v| format!("{v}"));
                Err(format!("types dont match on operation: `{lhs} {op} {rhs}`"))
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
        Instruction::DECLARE { ident, expr, typ } => {
            if symbol_table.get(ident).is_some() {
                Err(format!("variable: ({ident}) already declared"))
            } else {
                let expr_typ = analyse_expression(expr, symbol_table, typ.clone())?;

                match (typ, expr_typ) {
                    (None, None) => {
                        let typ = Type::Uint(NonZeroU8::new(1).unwrap());
                        // *symbol_table.get_mut(ident).unwrap() = typ;
                        analyse_expression(expr, symbol_table, Some(typ.clone()))?;
                        symbol_table.insert(ident.clone(), typ);
                        Ok(())
                    }
                    (None, Some(expr_typ)) => {
                        symbol_table.insert(ident.clone(), expr_typ);
                        // *symbol_table.get_mut(ident).unwrap() = expr_typ;
                        Ok(())
                    }
                    (Some(_typ), None) => {
                        // dont know about this one
                        Err(format!("could not determine type of variable: ({ident})"))
                    }
                    (Some(typ), Some(expr_typ)) => {
                        if *typ != expr_typ {
                            Err(format!("variable: ({ident}) is set of type of: `{typ}` but got type `{expr_typ}`\n`var {ident}: {typ} = {expr_typ}`"))
                        } else {
                            symbol_table.insert(ident.clone(), typ.clone());
                            Ok(())
                        }
                    }
                }
            }
        }
        Instruction::ASSIGN { ident, expr } => {
            let ident_typ = symbol_table
                .get(ident)
                .ok_or(format!("assignment of ({ident}) before declaration"))?
                .clone();
            let expr_typ = analyse_expression(expr, symbol_table, Some(ident_typ.clone()))?;
            match (ident_typ, expr_typ) {
                // (None, None) => {
                //     let typ = Some(Type::Uint(NonZeroU8::new(1).unwrap()));
                //     *symbol_table.get_mut(ident).unwrap() = typ;
                //     analyse_expression(expr, symbol_table, typ)?;
                //     Ok(())
                // }
                // (None, Some(typ)) => {
                // *symbol_table.get_mut(ident).unwrap() = Some(typ);
                // Ok(())
                // }
                (_, None) => Err(format!("assign expression must have a type")),
                (ident_typ, Some(expr_typ)) => {
                    if ident_typ != expr_typ {
                        Err(format!(
                            "assign expression type does not match variable's type\n`{ident}:({ident_typ}) = ({expr_typ});`"
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

fn const_anyint(ram: &mut Ram, bytes: &[u8], state: &State) {
    let size = bytes.len() as u8;
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
}
fn ident_anyint(ram: &mut Ram, var: &Var, state: &State) {
    let size = match var.typ {
        Type::Uint(size) | Type::Int(size) => size.get(),
        Type::Ref(_) => todo!(),
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

fn gen_expression(expr: &Expression, ram: &mut Ram, mem_table: &MemTable, state: State) {
    match expr {
        Expression::CONSTANT { value, typ } => {
            if let Some(typ) = typ.borrow().clone() {
                let mut bytes = value.to_le_bytes().to_vec();
                match typ {
                    Type::Uint(size) | Type::Int(size) => {
                        let size = size.get();
                        bytes.resize(size as usize, 0);
                        const_anyint(ram, &bytes, &state);
                    }
                    Type::Ref(_) => todo!(),
                };
            } else {
                panic!("couldn't generate code");
            }
        }
        Expression::IDENTIFIER { ident } => {
            let var = &mem_table[ident];
            ident_anyint(ram, var, &state);
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
                    typ: RefCell::new(Some(Type::Uint(NonZeroU8::new(1).unwrap()))),
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
        Instruction::ASSIGN { ident, expr } | Instruction::DECLARE { ident, expr, .. } => {
            gen_expression(expr, ram, mem_table, State::Normal);
            let var = &mem_table[ident];
            let size = match var.typ {
                Type::Uint(size) | Type::Int(size) => size.get(),
                Type::Ref(_) => todo!(),
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

    let instruction = parse(&code);
    let mut symbol_table = SymbolTable::new();
    // symbol_table.insert(Rc::from("out"), Type::Uint(NonZeroU8::new(1).unwrap()));
    symbol_table.insert(
        Rc::from("out"),
        Type::Ref(Rc::new(Type::Uint(NonZeroU8::new(4).unwrap()))),
    );
    match analyse_instruction(&instruction, &mut symbol_table) {
        Ok(_) => {}
        Err(e) => {
            println!("{e}");
            return;
        }
    };
    println!("{instruction:#?}");

    // get max var size for working memory
    let mut i: u8 = 0;
    for (_ident, typ) in symbol_table.iter() {
        match typ {
            Type::Uint(size) | Type::Int(size) => i = i.max(size.get()),
            Type::Ref(_) => todo!(),
        }
    }
    println!("Working memory size: {i} bytes");

    let mut mem_table = HashMap::new();

    for (ident, typ) in symbol_table.iter() {
        match typ {
            Type::Uint(size) | Type::Int(size) => {
                let var = if &**ident == "out" {
                    Var {
                        addr: AddrType::Abs(0x5000),
                        typ: typ.clone(),
                    }
                } else {
                    let var = Var {
                        addr: AddrType::Zp(i),
                        typ: typ.clone(),
                    };
                    i += size.get();
                    var
                };
                let addr = match var.addr {
                    AddrType::Abs(addr) => format!("{addr:#06X}"),
                    AddrType::Zp(addr) => format!("{addr:#04X}"),
                };
                println!("ident: {ident}, addr: {addr}, size: {size} type: {typ}");
                mem_table.insert(ident.clone(), var);
            }
            Type::Ref(_) => todo!(),
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
