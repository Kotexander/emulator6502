use std::cell::RefCell;
use std::collections::HashMap;
use std::num::NonZeroU8;
use std::rc::Rc;

mod analyser;
use analyser::*;

use cpu::OpCode::*;

#[derive(Debug, Clone, Copy)]
enum Addr {
    Abs(u16),
    Zp(u8),
}
impl Addr {
    fn get_type(&self) -> AddrMode {
        match self {
            Addr::Abs(_) => AddrMode::ABS,
            Addr::Zp(_) => AddrMode::ZPG,
        }
    }
}
struct Var {
    addr: Addr,
    typ: Type,
}

type MemTable = HashMap<Rc<str>, Var>;

struct Ram {
    data: [u8; 64 * 1024],
    ptr: usize,
}
impl Ram {
    fn new(start: u16) -> Self {
        let mut data = [0u8; 64 * 1024];
        let start_bytes = start.to_le_bytes();
        data[0xFFFC] = start_bytes[0];
        data[0xFFFD] = start_bytes[1];
        let ptr = start as usize;
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
        ram.insert_u8(LDA_IMM as u8);
        ram.insert_u8(bytes[i as usize]);

        // save
        ram.insert_u8(STA_ZPG as u8);
        ram.insert_u8(i);
    }
}
fn add_bytes(ram: &mut Ram, bytes: &[u8]) {
    let size = bytes.len() as u8;

    ram.insert_u8(CLC_IMP as u8);
    for i in 0..size {
        // load
        ram.insert_u8(LDA_ZPG as u8);
        ram.insert_u8(i);

        // add
        ram.insert_u8(ADC_IMM as u8);
        ram.insert_u8(bytes[i as usize]);

        // save
        ram.insert_u8(STA_ZPG as u8);
        ram.insert_u8(i);
    }
}
fn sub_bytes(ram: &mut Ram, bytes: &[u8]) {
    let size = bytes.len() as u8;

    ram.insert_u8(SEC_IMP as u8);
    for i in 0..size {
        // load
        ram.insert_u8(LDA_ZPG as u8);
        ram.insert_u8(i);

        // sub
        ram.insert_u8(SBC_IMM as u8);
        ram.insert_u8(bytes[i as usize]);

        // save
        ram.insert_u8(STA_ZPG as u8);
        ram.insert_u8(i);
    }
}

fn load_id(ram: &mut Ram, size: u8, addr: Addr) {
    for i in 0..size {
        // load
        match addr {
            Addr::Abs(p) => {
                let p = p + i as u16;
                ram.insert_u8(LDA_ABS as u8);
                ram.insert_u16(p);
            }
            Addr::Zp(p) => {
                let p = p + i;
                ram.insert_u8(LDA_ZPG as u8);
                ram.insert_u8(p);
            }
        };

        // save
        ram.insert_u8(STA_ZPG as u8);
        ram.insert_u8(i);
    }
}
fn add_id(ram: &mut Ram, size: u8, addr: Addr) {
    ram.insert_u8(CLC_IMP as u8);
    for i in 0..size {
        // load
        ram.insert_u8(LDA_ZPG as u8);
        ram.insert_u8(i);

        // add
        match addr {
            Addr::Abs(p) => {
                let p = p + i as u16;
                ram.insert_u8(ADC_ABS as u8);
                ram.insert_u16(p);
            }
            Addr::Zp(p) => {
                let p = p + i;
                ram.insert_u8(ADC_ZPG as u8);
                ram.insert_u8(p);
            }
        }

        // save
        ram.insert_u8(STA_ZPG as u8);
        ram.insert_u8(i);
    }
}
fn sub_id(ram: &mut Ram, size: u8, addr: Addr) {
    ram.insert_u8(SEC_IMP as u8);
    for i in 0..size {
        // load
        ram.insert_u8(LDA_ZPG as u8);
        ram.insert_u8(i);

        match addr {
            Addr::Abs(p) => {
                let p = p + i as u16;
                ram.insert_u8(SBC_ABS as u8);
                ram.insert_u16(p);
            }
            Addr::Zp(p) => {
                let p = p + i;
                ram.insert_u8(SBC_ZPG as u8);
                ram.insert_u8(p);
            }
        }

        // save
        ram.insert_u8(STA_ZPG as u8);
        ram.insert_u8(i);
    }
}

// fn load_ind(ram: &mut Ram, size: u8, addr: Addr, typ: AddrMode) {
//     match typ {
//         AddrMode::ABS => {
//             todo!();
//             // ram.insert_u8(LDA_ZPG as u8);
//             // ram.insert_u8(0);
//             // ram.insert_u8(PHA_IMP as u8);

//             // ram.insert_u8(LDA_ZPG as u8);
//             // ram.insert_u8(1);
//             // ram.insert_u8(PHA_IMP as u8);
//         }
//         AddrMode::ZPG => {
//             ram.insert_u8(LDX_IMM as u8);
//             ram.insert_u8(0);
//         }
//     }
//     for i in 0..size {
//         // match addr {
//         //     Addr::Abs(p) => {}
//         //     Addr::Zp(p) => {
//         //         ram.insert_u8(LDA_IZX as u8);
//         //         ram.insert_u8(p);
//         //         ram.insert_u8(STA_ZPG as u8);
//         //         ram.insert_u8(i);
//         //     }
//         // }
//         match typ {
//             AddrMode::ABS => match addr {
//                 Addr::Abs(_) => todo!(),
//                 Addr::Zp(_) => todo!(),
//             },
//             AddrMode::ZPG => match addr {
//                 Addr::Abs(p) => {
//                     todo!();
//                 }
//                 Addr::Zp(_) => {
//                     ram.insert_u8(LDA_IZX as u8);
//                     ram.insert_u8(STA_ZPG as u8);
//                     ram.insert_u8(i);
//                     ram.insert_u8(INX_IMP as u8);
//                 }
//             },
//         }
//     }
//     // match typ {
//     //     AddrMode::ABS => {
//     //         ram.insert_u8(PLA_IMP as u8);
//     //         ram.insert_u8(STA_ZPG as u8);
//     //         ram.insert_u8(1);

//     //         ram.insert_u8(PLA_IMP as u8);
//     //         ram.insert_u8(STA_ZPG as u8);
//     //         ram.insert_u8(0);
//     //     }
//     //     AddrMode::ZPG => {}
//     // }

//     // for i in 0..size {
//     // load
//     // match addr {
//     // Addr::Abs(p) => {
//     // let p = p + i as u16;
//     // ram.insert_u8(LDA_i as u8);
//     // ram.insert_u16(p);
//     // }
//     // Addr::Zp(p) => {
//     // let p = p + i;
//     // ram.insert_u8(LDA_ZPG as u8);
//     // ram.insert_u8(p);
//     // }
//     // };

//     // save
//     // ram.insert_u8(STA_ZPG as u8);
//     // ram.insert_u8(i);
//     // }
// }

fn gen_expression(expr: &Expression, ram: &mut Ram, mem_table: &MemTable, state: Option<Operator>) {
    match expr {
        Expression::CONSTANT { value, typ } => {
            let typ = typ.borrow();
            let typ = typ.as_ref().unwrap();
            let size = typ.get_size().get();
            let mut bytes = value.to_le_bytes().to_vec();
            bytes.resize(size as usize, 0);

            match state {
                Some(op) => match op {
                    Operator::ADD => add_bytes(ram, &bytes),
                    Operator::SUB => sub_bytes(ram, &bytes),
                    _ => {
                        panic!()
                    }
                },
                None => load_bytes(ram, &bytes),
            }
        }
        Expression::IDENTIFIER { id } => {
            let var = &mem_table[id];
            let size = var.typ.get_size().get();
            let addr = var.addr;

            match state {
                Some(op) => match op {
                    Operator::ADD => add_id(ram, size, addr),
                    Operator::SUB => sub_id(ram, size, addr),
                    _ => {}
                },
                None => load_id(ram, size, addr),
            }
        }
        Expression::BINOP { lhs, op, rhs } => {
            gen_expression(lhs, ram, mem_table, state);

            gen_expression(rhs, ram, mem_table, Some(*op));
        }
        Expression::REFERENCE { id } => {
            let var = &mem_table[id];

            let expr = match var.addr {
                Addr::Abs(p) => Expression::CONSTANT {
                    value: p as usize,
                    typ: RefCell::new(Some(Type::Uint(NonZeroU8::new(2).unwrap()))),
                },
                Addr::Zp(p) => Expression::CONSTANT {
                    value: p as usize,
                    typ: RefCell::new(Some(Type::Uint(NonZeroU8::new(1).unwrap()))),
                },
            };

            gen_expression(&expr, ram, mem_table, state);
        }
        Expression::DEREFERENCE { id } => todo!(),
        Expression::BOOL { value } => todo!(),
    }
}
fn gen_instruction(instruction: &Instruction, ram: &mut Ram, mem_table: &MemTable) {
    match instruction {
        Instruction::ASSIGN { id: ident, expr }
        | Instruction::DECLARE {
            id: ident, expr, ..
        } => {
            gen_expression(expr, ram, mem_table, None);
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
        Instruction::IF { condition, block } => {
            gen_expression(condition, ram, mem_table, None);
            todo!();
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
                addr: AddrMode::ZPG,
            },
        },
    );

    let instruction = parse(code);
    let mut symbol_table = SymbolTable::new();

    for (id, var) in &global_vars {
        let symbol = Symbol {
            typ: var.typ.clone(),
            addr_mode: var.addr.get_type(),
        };
        symbol_table.insert(id.clone(), symbol).unwrap();
    }

    analyse_instruction(&instruction, &mut symbol_table)?;
    println!("{instruction:#?}");

    // get max var size for working memory
    let mut zp_ptr: u8 = 2;
    for (_id, symbol) in symbol_table.table.iter() {
        zp_ptr = zp_ptr.max(symbol.typ.get_size().get());
    }
    println!("Memory register size: {zp_ptr} bytes");

    let mut start = 0x0100;
    let mut mem_table = global_vars;
    for id in mem_table.keys() {
        symbol_table.table.remove(id);
    }

    for (id, symbol) in symbol_table.table.iter() {
        let typ = &symbol.typ;
        let addr_mode = &symbol.addr_mode;
        // assert_eq!(*addr, AddrType::ZPG);
        let var = match addr_mode {
            AddrMode::ABS => {
                let size = typ.get_size().get();
                let var = Var {
                    addr: Addr::Abs(start),
                    typ: typ.clone(),
                };
                start += size as u16;
                var
            }
            AddrMode::ZPG => {
                let size = typ.get_size().get();
                let var = Var {
                    addr: Addr::Zp(zp_ptr),
                    typ: typ.clone(),
                };
                zp_ptr += size;
                var
            }
        };
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

    let mut ram = Ram::new(start);

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
