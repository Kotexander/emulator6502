mod parser;
use std::{collections::HashMap, num::NonZeroU8, rc::Rc};

pub use parser::*;

// pub type SymbolTable = HashMap<Rc<str>, (Type, AddrMode)>;
#[derive(Debug, Clone)]
pub struct Symbol {
    pub typ: Type,
    pub addr_mode: AddrMode,
}
pub struct SymbolTable {
    pub table: HashMap<Rc<str>, Symbol>,
}
impl SymbolTable {
    pub fn new() -> Self {
        let table = HashMap::new();
        Self { table }
    }
    pub fn get(&self, id: &Rc<str>) -> Result<&Symbol, String> {
        self.table
            .get(id)
            .ok_or(format!("variable {id} is not defined"))
    }
    pub fn insert(&mut self, id: Rc<str>, symbol: Symbol) -> Result<(), String> {
        if self.table.contains_key(&id) {
            Err(format!("{id} is already defined"))
        } else {
            self.table.insert(id, symbol);
            Ok(())
        }
    }
}

/// `imp_typ` is used for implied type incase type is not defined
///
/// Returns the type of expression
pub fn analyse_expression(
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
            let symbol = symbol_table.get(id)?.clone();

            Ok(Some(symbol.typ))
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
                let lhs = lhs_typ2.map_or(String::from("(Unknown)"), |v| format!("{v}"));
                let rhs = rhs_typ2.map_or(String::from("(Unknown)"), |v| format!("{v}"));
                Err(format!("types dont match on operation: `{lhs} {op} {rhs}`"))
            }
        }
        Expression::REFERENCE { id } => {
            let symbol = symbol_table.get(id)?.clone();

            Ok(Some(Type::Ref {
                typ: Rc::new(symbol.typ),
                addr: symbol.addr_mode,
            }))
        } // Expression::DEREFERENCE { id } => {
          //     let (id_typ, _addr) = symbol_table
          //         .get(id)
          //         .ok_or(format!("undeclared variable: {id}"))?
          //         .clone();
          //     let id_typ = match id_typ {
          //         Type::Ref { typ, addr: _ } => Ok(typ),
          //         _ => Err("can't dereference {id}: ({id_typ})"),
          //     }?;
          //     // let id_typ: &Type = &id_typ;
          //     Ok(Some(id_typ.deref().clone()))
          // }
    }
}
pub fn analyse_instruction(
    instruction: &Instruction,
    symbol_table: &mut SymbolTable,
) -> Result<(), String> {
    match instruction {
        Instruction::DECLARE {
            id,
            expr,
            typ,
            addr_mode,
        } => {
            let expr_typ = analyse_expression(expr, symbol_table, typ.clone())?;

            match (typ, expr_typ) {
                (None, None) => {
                    let typ = Type::Uint(NonZeroU8::new(1).unwrap());
                    analyse_expression(expr, symbol_table, Some(typ.clone()))?;
                    let symbol = Symbol {
                        typ,
                        addr_mode: *addr_mode,
                    };
                    symbol_table.insert(id.clone(), symbol)?;
                    Ok(())
                }
                (None, Some(expr_typ)) => {
                    let symbol = Symbol {
                        typ: expr_typ,
                        addr_mode: *addr_mode,
                    };
                    symbol_table.insert(id.clone(), symbol)?;
                    Ok(())
                }
                (Some(_typ), None) => {
                    // dont know about this one
                    Err(format!("could not determine type of variable: ({id})"))
                }
                (Some(typ), Some(expr_typ)) => {
                    if *typ != expr_typ {
                        Err(format!("variable: ({id}) is set of type of: `{typ}` but got type `{expr_typ}`\n`var {id}: ({typ}) = ({expr_typ})`"))
                    } else {
                        let symbol = Symbol {
                            typ: typ.clone(),
                            addr_mode: *addr_mode,
                        };
                        symbol_table.insert(id.clone(), symbol)?;

                        Ok(())
                    }
                }
            }
        }
        Instruction::ASSIGN { id, expr } => {
            let symbol = symbol_table.get(id)?.clone();
            let expr_typ = analyse_expression(expr, symbol_table, Some(symbol.typ.clone()))?;
            match (symbol.typ, expr_typ) {
                (_, None) => Err(String::from("assign expression must have a type")),
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
