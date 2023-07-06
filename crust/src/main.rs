use std::cell::RefCell;
use std::collections::HashMap;
use std::num::NonZeroU8;
use std::rc::Rc;

mod parser;
use parser::*;

#[derive(Debug, Clone, Copy)]
enum Addr {
    Abs(u16),
    Zp(u8),
}
impl Addr {
    fn get_type(&self) -> AddrType {
        match self {
            Addr::Abs(_) => AddrType::ABS,
            Addr::Zp(_) => AddrType::ZPG,
        }
    }
}
struct Var {
    addr: Addr,
    typ: Type,
}

type SymbolTable = HashMap<Rc<str>, (Type, AddrType)>;
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
        }
        Expression::IDENTIFIER { id } => {
            let (id_typ, _) = symbol_table
                .get(id)
                // .copied()
                .ok_or(format!("undeclared variable: {id}"))?
                .clone();

            Ok(Some(id_typ))
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
        Expression::REFRENCE { id } => {
            let (id_typ, addr) = symbol_table
                .get(id)
                .ok_or(format!("undeclared variable: {id}"))?
                .clone();

            Ok(Some(Type::Ref {
                typ: Rc::new(id_typ),
                addr,
            }))
        }
    }
}
fn analyse_instruction(
    instruction: &Instruction,
    symbol_table: &mut SymbolTable,
) -> Result<(), String> {
    match instruction {
        Instruction::DECLARE {
            id,
            expr,
            typ,
            addr,
        } => {
            if symbol_table.get(id).is_some() {
                Err(format!("variable: ({id}) already declared"))
            } else {
                let expr_typ = analyse_expression(expr, symbol_table, typ.clone())?;

                match (typ, expr_typ) {
                    (None, None) => {
                        let typ = Type::Uint(NonZeroU8::new(1).unwrap());
                        analyse_expression(expr, symbol_table, Some(typ.clone()))?;
                        symbol_table.insert(id.clone(), (typ, *addr));
                        Ok(())
                    }
                    (None, Some(expr_typ)) => {
                        symbol_table.insert(id.clone(), (expr_typ, *addr));
                        Ok(())
                    }
                    (Some(_typ), None) => {
                        // dont know about this one
                        Err(format!("could not determine type of variable: ({id})"))
                    }
                    (Some(typ), Some(expr_typ)) => {
                        if *typ != expr_typ {
                            Err(format!("variable: ({id}) is set of type of: `{typ}` but got type `{expr_typ}`\n`var {id}: {typ} = {expr_typ}`"))
                        } else {
                            symbol_table.insert(id.clone(), (typ.clone(), *addr));
                            Ok(())
                        }
                    }
                }
            }
        }
        Instruction::ASSIGN { id, expr } => {
            let (id_typ, addr) = symbol_table
                .get(id)
                .ok_or(format!("assignment of ({id}) before declaration"))?
                .clone();
            let expr_typ = analyse_expression(expr, symbol_table, Some(id_typ.clone()))?;
            match (id_typ, expr_typ) {
                (_, None) => Err(format!("assign expression must have a type")),
                (id_typ, Some(expr_typ)) => {
                    if id_typ != expr_typ {
                        Err(format!(
                            "assign expression type does not match variable's type\n`{id}:({id_typ}) = ({expr_typ});`"
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

fn load_bytes(ram: &mut Ram, bytes: &[u8]) {
    let size = bytes.len() as u8;

    for i in 0..size {
        // load
        ram.insert_u8(0xA9); // LDA_IMM
        ram.insert_u8(bytes[i as usize]);

        // save
        ram.insert_u8(0x85); // STA_ZP0
        ram.insert_u8(i);
    }
}
fn add_bytes(ram: &mut Ram, bytes: &[u8]) {
    let size = bytes.len() as u8;

    ram.insert_u8(0x18); // CLC_IMP
    for i in 0..size {
        // load
        ram.insert_u8(0xA5); // LDA_ZP0
        ram.insert_u8(i);

        // add
        ram.insert_u8(0x69); // ADC_IMM
        ram.insert_u8(bytes[i as usize]);

        // save
        ram.insert_u8(0x85); // STA_ZP0
        ram.insert_u8(i);
    }
}
fn sub_bytes(ram: &mut Ram, bytes: &[u8]) {
    let size = bytes.len() as u8;

    ram.insert_u8(0x38); // SEC_IMP
    for i in 0..size {
        // load
        ram.insert_u8(0xA5); // LDA_ZP0
        ram.insert_u8(i);

        // sub
        ram.insert_u8(0xE9); // SBC_IMM
        ram.insert_u8(bytes[i as usize]);

        // save
        ram.insert_u8(0x85); // STA_ZP0
        ram.insert_u8(i);
    }
}

fn load_id(ram: &mut Ram, size: u8, addr: Addr) {
    for i in 0..size {
        // load
        match addr {
            Addr::Abs(p) => {
                let p = p + i as u16;
                ram.insert_u8(0xAD); // LDA_ABS
                ram.insert_u16(p);
            }
            Addr::Zp(p) => {
                let p = p + i;
                ram.insert_u8(0xA5); // LDA_ZP0
                ram.insert_u8(p);
            }
        };

        // save
        ram.insert_u8(0x85); // STA_ZP0
        ram.insert_u8(i);
    }
}
fn add_id(ram: &mut Ram, size: u8, addr: Addr) {
    ram.insert_u8(0x18); // CLC_IMP
    for i in 0..size {
        // load
        ram.insert_u8(0xA5); // LDA_ZP0
        ram.insert_u8(i);

        // add
        match addr {
            Addr::Abs(p) => {
                let p = p + i as u16;
                ram.insert_u8(0x6D); // ADC_ABS
                ram.insert_u16(p);
            }
            Addr::Zp(p) => {
                let p = p + i;
                ram.insert_u8(0x65); // ADC_ZP0
                ram.insert_u8(p);
            }
        }

        // save
        ram.insert_u8(0x85); // STA_ZP0
        ram.insert_u8(i);
    }
}
fn sub_id(ram: &mut Ram, size: u8, addr: Addr) {
    ram.insert_u8(0x38); // SEC_IMP
    for i in 0..size {
        // load
        ram.insert_u8(0xA5); // LDA_ZP0
        ram.insert_u8(i);

        match addr {
            Addr::Abs(p) => {
                let p = p + i as u16;
                ram.insert_u8(0xED); // SBC_ABS
                ram.insert_u16(p);
            }
            Addr::Zp(p) => {
                let p = p + i;
                ram.insert_u8(0xE5); // SBC_ZP0
                ram.insert_u8(p);
            }
        }

        // save
        ram.insert_u8(0x85); // STA_ZP0
        ram.insert_u8(i);
    }
}

fn gen_expression(expr: &Expression, ram: &mut Ram, mem_table: &MemTable, state: State) {
    match expr {
        Expression::CONSTANT { value, typ } => {
            let typ = typ.borrow();
            let typ = typ.as_ref().unwrap();
            let size = typ.get_size().get();
            let mut bytes = value.to_le_bytes().to_vec();
            bytes.resize(size as usize, 0);

            match state {
                State::Normal => load_bytes(ram, &bytes),
                State::Add => add_bytes(ram, &bytes),
                State::Sub => sub_bytes(ram, &bytes),
            }
        }
        Expression::IDENTIFIER { id } => {
            let var = &mem_table[id];
            let size = var.typ.get_size().get();
            let addr = var.addr.clone();
            match state {
                State::Normal => load_id(ram, size, addr),
                State::Add => add_id(ram, size, addr),
                State::Sub => sub_id(ram, size, addr),
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
        Expression::REFRENCE { id: ident } => {
            let var = mem_table.get(ident).expect(&format!(
                "all identifiers should be defined during code generation but ({ident}) was not"
            ));

            let p = match var.addr {
                Addr::Abs(_) => todo!("haven't figured out how to check if references lengths match yet; therefore, only zero point references are supported"),
                Addr::Zp(p) => p,
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
        Instruction::ASSIGN { id: ident, expr }
        | Instruction::DECLARE {
            id: ident, expr, ..
        } => {
            gen_expression(expr, ram, mem_table, State::Normal);
            let var = &mem_table[ident];
            let size = var.typ.get_size().get();

            for i in 0..size {
                ram.insert_u8(0xA5); // LDA_ZP0
                ram.insert_u8(i);
                match var.addr {
                    Addr::Abs(p) => {
                        let p = p + i as u16;
                        ram.insert_u8(0x8D); // STA_ABS
                        ram.insert_u16(p);
                    }
                    Addr::Zp(p) => {
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

fn compile(code: &str) -> Result<Ram, String> {
    let mut global_vars = MemTable::new();
    global_vars.insert(
        Rc::from("zp_out_u32"),
        Var {
            addr: Addr::Abs(0x5000),
            typ: Type::Ref {
                typ: Rc::new(Type::Uint(NonZeroU8::new(4).unwrap())),
                addr: AddrType::ZPG,
            },
        },
    );

    let instruction = parse(&code);
    let mut symbol_table = SymbolTable::new();

    for (id, var) in &global_vars {
        symbol_table.insert(id.clone(), (var.typ.clone(), var.addr.get_type()));
    }

    analyse_instruction(&instruction, &mut symbol_table)?;
    println!("{instruction:#?}");

    // get max var size for working memory
    let mut i: u8 = 2;
    for (_ident, (typ, _addr)) in symbol_table.iter() {
        i = i.max(typ.get_size().get());
    }
    println!("Memory register size: {i} bytes");

    let mut mem_table = global_vars;
    for (id, _var) in &mem_table {
        symbol_table.remove(id);
    }

    for (id, (typ, addr)) in symbol_table.iter() {
        assert_eq!(*addr, AddrType::ZPG);
        let size = typ.get_size();
        let var = Var {
            addr: Addr::Zp(i),
            typ: typ.clone(),
        };
        i += size.get();
        mem_table.insert(id.clone(), var);
    }

    for (id, var) in &mem_table {
        let addr = match var.addr {
            Addr::Abs(addr) => format!("{addr:#06X}"),
            Addr::Zp(addr) => format!("{addr:#04X}"),
        };
        let typ = &var.typ;
        let size = typ.get_size();
        println!("ident: {id}, addr: {addr} type: {typ} size: {size} byte");
    }

    let mut ram = Ram::new();

    gen_instruction(&instruction, &mut ram, &mem_table);

    let ptr = ram.ptr as u16;
    ram.insert_u8(0xEA); // NOP
    ram.insert_u8(0x4C); // JMP_ABS
    ram.insert_u16(ptr);

    Ok(ram)
}

fn main() {
    let file = std::env::args().nth(1).expect("No file given");
    let path = std::path::Path::new(&file);
    let code = std::fs::read_to_string(path).expect("Could not read file");

    match compile(&code) {
        Ok(ram) => {
            std::fs::write(
                format!(
                    "{}.hex",
                    path.file_stem()
                        .expect("Could not get file name")
                        .to_str()
                        .unwrap()
                ),
                ram.data,
            )
            .unwrap();
        }
        Err(err) => println!("{err}"),
    }
}
