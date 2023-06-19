// enum Instruction {
//     ADC,
//     AND,
//     ASL,
//     // BBR, // *
//     // BBS, // *
//     BCC,
//     BCS,
//     BEQ,
//     BIT,
//     BMI,
//     BNE,
//     BPL,
//     // BRA, // *
//     BRK,
//     BVC,
//     BVS,
//     CLC,
//     CLD,
//     CLI,
//     CLV,
//     CMP,
//     CPX,
//     CPY,
//     DEC,
//     DEX,
//     DEY,
//     EOR,
//     INC,
//     INX,
//     INY,
//     JMP,
//     JSR,
//     LDA,
//     LDX,
//     LDY,
//     LSR,
//     NOP,
//     ORA,
//     PHA,
//     PHP,
//     // PHX, // *
//     // PHY, // *
//     PLA,
//     PLP,
//     // PLX, // *
//     // PLY, // *
//     // RMB, // *
//     ROL,
//     ROR,
//     RTI,
//     RTS,
//     SBC,
//     SEC,
//     SED,
//     SEI,
//     // SMB, // *
//     STA,
//     // STP, // *
//     STX,
//     STY,
//     // STZ, // *
//     TAX,
//     TAY,
//     // TRB, // *
//     // TSB, // *
//     TSX,
//     TXA,
//     TXS,
//     TYA,
//     // WAI, // *
// }
// enum AddressMode {
//     ABS,
//     // AIX, // *
//     ABX,
//     ABY,
//     ABI,
//     ACC,
//     IMM,
//     IMP,
//     REL,
//     // STK, // *
//     ZP0,
//     ZIX,
//     ZPX,
//     ZPY,
//     // ZPI, // *
//     ZIY,
//     IND,
// }

// use AddressMode::*;
// use Instruction::*;

// const Opcodes: [[Option<(Instruction, AddressMode)>; 16]; 16] = [
//     [
//         Some((BRK, IMP)),
//         Some((ORA, ZIX)),
//         None,
//         None,
//         None,
//         Some((ORA, ZP0)),
//         Some((ASL, ZP0)),
//         None,
//         Some((PHP, IMP)),
//         Some((ORA, IMM)),
//         Some((ASL, ACC)),
//         None,
//         None,
//         Some((ORA, ABS)),
//         Some((ASL, ABS)),
//         None,
//     ],
//     [
//         Some((BPL, REL)),
//         Some((ORA, ZIY)),
//         None,
//         None,
//         None,
//         Some((ORA, ZPX)),
//         Some((ASL, ZPX)),
//         None,
//         Some((CLC, IMP)),
//         Some((ORA, ABY)),
//         None,
//         None,
//         None,
//         Some((ORA, ABX)),
//         Some((ASL, ABX)),
//         None,
//     ],
//     [
//         Some((JSR, ABS)),
//         Some((AND, ZIX)),
//         None,
//         None,
//         Some((BIT, ZP0)),
//         Some((AND, ZP0)),
//         Some((ROL, ZP0)),
//         None,
//         Some((PLP, IMP)),
//         Some((AND, IMM)),
//         Some((ROL, ACC)),
//         None,
//         Some((BIT, ABS)),
//         Some((AND, ABS)),
//         Some((ROL, ABS)),
//         None,
//     ],
//     [
//         Some((BMI, REL)),
//         Some((AND, ZIY)),
//         None,
//         None,
//         None,
//         Some((AND, ZPX)),
//         Some((ROL, ZPX)),
//         None,
//         Some((SEC, IMP)),
//         Some((AND, ABY)),
//         None,
//         None,
//         None,
//         Some((AND, ABX)),
//         Some((ROL, ABX)),
//         None,
//     ],
//     [
//         Some((RTI, IMP)),
//         Some((EOR, ZIX)),
//         None,
//         None,
//         None,
//         Some((EOR, ZP0)),
//         Some((LSR, ZP0)),
//         None,
//         Some((PHA, IMP)),
//         Some((EOR, IMM)),
//         Some((LSR, ACC)),
//         None,
//         Some((JMP, ABS)),
//         Some((EOR, ABS)),
//         Some((LSR, ABS)),
//         None,
//     ],
//     [
//         Some((BVC, REL)),
//         Some((EOR, ZIY)),
//         None,
//         None,
//         None,
//         Some((EOR, ZPX)),
//         Some((LSR, ZPX)),
//         None,
//         Some((CLI, IMP)),
//         Some((EOR, ABY)),
//         None,
//         None,
//         None,
//         Some((EOR, ABX)),
//         Some((LSR, ABX)),
//         None,
//     ],
//     [
//         Some((RTS, IMP)),
//         Some((ADC, ZIX)),
//         None,
//         None,
//         None,
//         Some((ADC, ZP0)),
//         Some((ROR, ZP0)),
//         None,
//         Some((PLA, IMP)),
//         Some((ADC, IMM)),
//         Some((ROR, ACC)),
//         None,
//         Some((JMP, IND)),
//         Some((ADC, ABS)),
//         Some((ROR, ABS)),
//         None,
//     ],
//     [
//         Some((BVS, REL)),
//         Some((ADC, ZIY)),
//         None,
//         None,
//         None,
//         Some((ADC, ZPX)),
//         Some((ROR, ZPX)),
//         None,
//         Some((SEI, IMP)),
//         Some((ADC, ABY)),
//         None,
//         None,
//         None,
//         Some((ADC, ABX)),
//         Some((ROR, ABX)),
//         None,
//     ],
//     [
//         None,
//         Some((STA, ZIX)),
//         None,
//         None,
//         Some((STY, ZP0)),
//         Some((STA, ZP0)),
//         Some((STX, ZP0)),
//         None,
//         Some((DEY, IMP)),
//         None,
//         Some((TXA, IMP)),
//         None,
//         Some((STY, ABS)),
//         Some((STA, ABS)),
//         Some((STX, ABS)),
//         None,
//     ],
//     [
//         Some((BCC, REL)),
//         Some((STA, ZIY)),
//         None,
//         None,
//         Some((STY, ZPX)),
//         Some((STA, ZPX)),
//         Some((STX, ZPY)),
//         None,
//         Some((TYA, IMP)),
//         Some((STA, ABY)),
//         Some((TXS, IMP)),
//         None,
//         None,
//         Some((STA, ABX)),
//         None,
//         None,
//     ],
//     [
//         Some((LDY, IMM)),
//         Some((LDA, ZIX)),
//         Some((LDX, IMM)),
//         None,
//         Some((LDY, ZP0)),
//         Some((LDA, ZP0)),
//         Some((LDX, ZP0)),
//         None,
//         Some((TAY, IMP)),
//         Some((LDA, IMM)),
//         Some((TAX, IMP)),
//         None,
//         Some((LDY, ABS)),
//         Some((LDA, ABS)),
//         Some((LDX, ABS)),
//         None,
//     ],
//     [
//         Some((BCS, REL)),
//         Some((LDA, ZIY)),
//         None,
//         None,
//         Some((LDY, ZPX)),
//         Some((LDA, ZPX)),
//         Some((LDX, ZPY)),
//         None,
//         Some((CLV, IMP)),
//         Some((LDA, ABY)),
//         Some((TSX, IMP)),
//         None,
//         Some((LDY, ABX)),
//         Some((LDA, ABX)),
//         Some((LDX, ABY)),
//         None,
//     ],
//     [
//         Some((CPY, IMM)),
//         Some((CMP, ZIX)),
//         None,
//         None,
//         Some((CPY, ZP0)),
//         Some((CMP, ZP0)),
//         Some((DEC, ZP0)),
//         None,
//         Some((INY, IMP)),
//         Some((CMP, IMM)),
//         Some((DEX, IMP)),
//         None,
//         Some((CPY, ABS)),
//         Some((CMP, ABS)),
//         Some((DEC, ABS)),
//         None,
//     ],
//     [
//         Some((BNE, REL)),
//         Some((CMP, ZIY)),
//         None,
//         None,
//         None,
//         Some((CMP, ZPX)),
//         Some((DEC, ZPX)),
//         None,
//         Some((CLD, IMP)),
//         Some((CMP, ABY)),
//         None,
//         None,
//         None,
//         Some((CMP, ABX)),
//         Some((DEC, ABX)),
//         None,
//     ],
//     [],
//     [],
// ];

// #[cfg(test)]
// mod test {
//     use super::*;

//     #[test]
//     fn lda_imm() {
//         let a1 = 42;
//         let a2 = -100i8 as u8;
//         let a3 = 0;

//         let mut bus = Bus::new();
//         // lda #
//         bus.ram[0] = 0xA9;
//         bus.ram[1] = a1;
//         bus.ram[2] = 0xA9;
//         bus.ram[3] = a2;
//         bus.ram[4] = 0xA9;
//         bus.ram[5] = a3;

//         let mut cpu = CPU6502::new(bus);
//         assert!(cpu.a == 0);

//         cpu.execute();
//         assert!(cpu.a == a1);
//         assert!(
//             !cpu.get_ps(Flags6502::Z),
//             "Z bit should not be set for {}",
//             a1
//         );
//         assert!(
//             !cpu.get_ps(Flags6502::N),
//             "N bit should not be set for {}",
//             a1
//         );

//         cpu.execute();
//         assert!(cpu.a == a2);
//         assert!(
//             !cpu.get_ps(Flags6502::Z),
//             "Z bit should not be set for {}",
//             a2
//         );
//         assert!(cpu.get_ps(Flags6502::N), "N bit should be set for {}", a2);

//         cpu.execute();
//         assert!(cpu.a == a3);
//         assert!(
//             cpu.get_ps(Flags6502::Z),
//             "Z bit should not be set for {}",
//             a3
//         );
//         assert!(
//             !cpu.get_ps(Flags6502::N),
//             "N bit should not be set for {}",
//             a3
//         );
//     }
//     #[test]
//     fn lda_zp() {
//         let addr1 = 0x42;
//         let addr2 = 0x69;
//         let addr3 = 0x10;

//         let a1 = 42;
//         let a2 = -100i8 as u8;
//         let a3 = 0;

//         let mut bus = Bus::new();
//         bus.ram[0] = 0xA5;
//         bus.ram[1] = addr1;
//         bus.ram[2] = 0xA5;
//         bus.ram[3] = addr2;
//         bus.ram[4] = 0xA5;
//         bus.ram[5] = addr3;
//         bus.ram[addr1 as usize] = a1;
//         bus.ram[addr2 as usize] = a2;
//         bus.ram[addr3 as usize] = a3;

//         let mut cpu = CPU6502::new(bus);
//         assert!(cpu.a == 0);

//         cpu.execute();
//         assert!(cpu.a == a1);
//         assert!(
//             !cpu.get_ps(Flags6502::Z),
//             "Z bit should not be set for {}",
//             a1
//         );
//         assert!(
//             !cpu.get_ps(Flags6502::N),
//             "N bit should not be set for {}",
//             a1
//         );

//         cpu.execute();
//         assert!(cpu.a == a2);
//         assert!(
//             !cpu.get_ps(Flags6502::Z),
//             "Z bit should not be set for {}",
//             a2
//         );
//         assert!(cpu.get_ps(Flags6502::N), "N bit should be set for {}", a2);

//         cpu.execute();
//         assert!(cpu.a == a3);
//         assert!(
//             cpu.get_ps(Flags6502::Z),
//             "Z bit should not be set for {}",
//             a3
//         );
//         assert!(
//             !cpu.get_ps(Flags6502::N),
//             "N bit should not be set for {}",
//             a3
//         );
//     }
//     #[test]
//     fn lda_zpx() {
//         let addr = 0x42;
//         let addr2 = 0x12;

//         let x1 = 2;
//         let x2 = 10;
//         let x3 = 42;

//         let a1 = 42;
//         let a2 = -100i8 as u8;
//         let a3 = 0;

//         let mut bus = Bus::new();
//         bus.ram[0] = 0xB5;
//         bus.ram[1] = addr;
//         bus.ram[2] = 0xB5;
//         bus.ram[3] = addr;
//         bus.ram[4] = 0xB5;
//         bus.ram[5] = addr2;
//         bus.ram[(addr + x1) as usize] = a1;
//         bus.ram[(addr + x2) as usize] = a2;
//         bus.ram[(addr2 + x3) as usize] = a3;

//         let mut cpu = CPU6502::new(bus);
//         assert!(cpu.a == 0);

//         cpu.x = x1;
//         cpu.execute();
//         assert!(cpu.a == a1);
//         assert!(
//             !cpu.get_ps(Flags6502::Z),
//             "Z bit should not be set for {}",
//             a1
//         );
//         assert!(
//             !cpu.get_ps(Flags6502::N),
//             "N bit should not be set for {}",
//             a1
//         );

//         cpu.x = x2;
//         cpu.execute();
//         assert!(cpu.a == a2);
//         assert!(
//             !cpu.get_ps(Flags6502::Z),
//             "Z bit should not be set for {}",
//             a2
//         );
//         assert!(cpu.get_ps(Flags6502::N), "N bit should be set for {}", a2);

//         cpu.x = x3;
//         cpu.execute();
//         assert!(cpu.a == a3);
//         assert!(
//             cpu.get_ps(Flags6502::Z),
//             "Z bit should not be set for {}",
//             a3
//         );
//         assert!(
//             !cpu.get_ps(Flags6502::N),
//             "N bit should not be set for {}",
//             a3
//         );
//     }
//     #[test]
//     fn lda_abs() {
//         let addr1 = 0x4232;
//         let addr2 = 0x1292;
//         let addr3 = 0x5312;

//         let a1 = 42;
//         let a2 = -100i8 as u8;
//         let a3 = 0;

//         let mut bus = Bus::new();
//         bus.ram[0] = 0xAD;
//         bus.write_u16(1, addr1);
//         bus.ram[3] = 0xAD;
//         bus.write_u16(4, addr2);
//         bus.ram[6] = 0xAD;
//         bus.write_u16(7, addr3);
//         bus.ram[addr1 as usize] = a1;
//         bus.ram[addr2 as usize] = a2;
//         bus.ram[addr3 as usize] = a3;

//         let mut cpu = CPU6502::new(bus);
//         assert!(cpu.a == 0);

//         cpu.execute();
//         assert!(cpu.a == a1);
//         assert!(
//             !cpu.get_ps(Flags6502::Z),
//             "Z bit should not be set for {}",
//             a1
//         );
//         assert!(
//             !cpu.get_ps(Flags6502::N),
//             "N bit should not be set for {}",
//             a1
//         );

//         cpu.execute();
//         assert!(cpu.a == a2);
//         assert!(
//             !cpu.get_ps(Flags6502::Z),
//             "Z bit should not be set for {}",
//             a2
//         );
//         assert!(cpu.get_ps(Flags6502::N), "N bit should be set for {}", a2);

//         cpu.execute();
//         assert!(cpu.a == a3);
//         assert!(
//             cpu.get_ps(Flags6502::Z),
//             "Z bit should not be set for {}",
//             a3
//         );
//         assert!(
//             !cpu.get_ps(Flags6502::N),
//             "N bit should not be set for {}",
//             a3
//         );
//     }
// }

// match op_code {
//     // nop
//     0xEA => {}

//     // jsr
//     0x20 => {
//         let addr = self.fetch_u16();
//         self.jsr(addr);
//     }
//     // rts
//     0x60 => {
//         self.rts();
//     }

//     // adc immediate
//     0x69 => {
//         self.adc(None);
//     }
//     // adc zp
//     0x65 => {
//         let addr = self.zp0_addrmode();
//         self.adc(Some(addr));
//     }
//     // adc zp x
//     0x75 => {
//         let addr = self.zpx_addrmode();
//         self.adc(Some(addr));
//     }
//     // adc abs
//     0x6D => {
//         let addr = self.abs_addrmode();
//         self.adc(Some(addr));
//     }
//     // adc abs x
//     0x7D => {
//         let addr = self.abx_addrmode();
//         self.adc(Some(addr));
//     }
//     // adc abs y
//     0x79 => {
//         let addr = self.aby_addrmode();
//         self.adc(Some(addr));
//     }
//     // adc ind zp x
//     0x61 => {
//         let addr = self.zpx_addrmode();
//         self.adc(Some(addr));
//     }
//     // adc ind zp y
//     0x71 => {
//         let addr = self.zpy_addrmode();
//         self.adc(Some(addr));
//     }

//     // and immediate
//     0x29 => {
//         self.and(None);
//     }
//     // and zp
//     0x25 => {
//         let addr = self.zp0_addrmode();
//         self.and(Some(addr));
//     }
//     // and zp x
//     0x35 => {
//         let addr = self.zpx_addrmode();
//         self.and(Some(addr));
//     }
//     // and abs
//     0x2D => {
//         let addr = self.abs_addrmode();
//         self.and(Some(addr));
//     }
//     // and abs x
//     0x3D => {
//         let addr = self.abx_addrmode();
//         self.and(Some(addr));
//     }
//     // and abs y
//     0x39 => {
//         let addr = self.aby_addrmode();
//         self.and(Some(addr));
//     }
//     // and ind zp x
//     0x21 => {
//         let addr = self.izx_addrmode();
//         self.and(Some(addr));
//     }
//     // and ind zp y
//     0x31 => {
//         let addr = self.izy_addrmode();
//         self.and(Some(addr));
//     }

//     // asl accumulator
//     0x0A => {
//         self.asl(None);
//     }
//     // asl zp
//     0x06 => {
//         let addr = self.zp0_addrmode();
//         self.asl(Some(addr));
//     }
//     // asl zp x
//     0x16 => {
//         let addr = self.zpx_addrmode();
//         self.asl(Some(addr));
//     }
//     // asl abs
//     0x0E => {
//         let addr = self.abs_addrmode();
//         self.asl(Some(addr));
//     }
//     // asl abs x
//     0x1E => {
//         let addr = self.abx_addrmode();
//         self.asl(Some(addr));
//     }

//     // bcc
//     0x90 => {
//         self.bcc();
//     }

//     // lda immediate
//     0xA9 => {
//         self.lda(None);
//     }
//     // lda zp
//     0xA5 => {
//         let addr = self.zp0_addrmode();
//         self.lda(Some(addr));
//     }
//     // lda zp x
//     0xB5 => {
//         let addr = self.zpx_addrmode();
//         self.lda(Some(addr));
//     }
//     // lda abs
//     0xAD => {
//         let addr = self.abs_addrmode();
//         self.lda(Some(addr));
//     }
//     // lda abs x
//     0xBD => {
//         let addr = self.abx_addrmode();
//         self.lda(Some(addr));
//     }
//     // lda abs y
//     0xB9 => {
//         let addr = self.aby_addrmode();
//         self.lda(Some(addr));
//     }
//     // lda ind zp x
//     0xA1 => {
//         let addr = self.izx_addrmode();
//         self.lda(Some(addr));
//     }
//     // lda ind zp y
//     0xB1 => {
//         let addr = self.izy_addrmode();
//         self.lda(Some(addr));
//     }

//     // ldx immediate
//     0xA2 => {
//         self.ldx(None);
//     }
//     // ldx zp
//     0xA6 => {
//         let addr = self.zp0_addrmode();
//         self.ldx(Some(addr));
//     }
//     // ldx zp y
//     0xB6 => {
//         let addr = self.zpy_addrmode();
//         self.ldx(Some(addr));
//     }
//     // ldx abs
//     0xAE => {
//         let addr = self.abs_addrmode();
//         self.ldx(Some(addr));
//     }
//     // ldx abs y
//     0xBE => {
//         let addr = self.aby_addrmode();
//         self.ldx(Some(addr));
//     }

//     // ldy immediate
//     0xA0 => {
//         self.ldy(None);
//     }
//     // ldy zp
//     0xA4 => {
//         let addr = self.zp0_addrmode();
//         self.ldy(Some(addr));
//     }
//     // ldy zp x
//     0xB4 => {
//         let addr = self.zpx_addrmode();
//         self.ldy(Some(addr));
//     }
//     // ldy abs
//     0xAC => {
//         let addr = self.abs_addrmode();
//         self.ldy(Some(addr));
//     }
//     // ldy abs x
//     0xBC => {
//         let addr = self.abx_addrmode();
//         self.ldy(Some(addr));
//     }

//     _ => {
//         panic!("Unsupported instruction: {:#04X}", op_code);
//     }
// }

// use std::fmt::Display;

// // mod Flags6502 {
// //     pub const C: u8 = 1 << 0; // Carry            1 = true
// //     pub const Z: u8 = 1 << 1; // Zero             1 = true
// //     pub const I: u8 = 1 << 2; // IRQB ennable     1 = disable
// //     pub const D: u8 = 1 << 3; // Decimal mode     1 = true
// //     pub const B: u8 = 1 << 4; // BRK command      1 = BRK, 0 = IRQB
// //     pub const U: u8 = 1 << 5; // Unused
// //     pub const V: u8 = 1 << 6; // Overflow         1 = true
// //     pub const N: u8 = 1 << 7; // Negative         1 = neg
// // }

// enum Flags6502 {
//     C,
//     Z,
//     I,
//     D,
//     B,
//     U,
//     V,
//     N,
// }
// impl Flags6502 {
//     fn bit(self) -> u8 {
//         use Flags6502::*;
//         match self {
//             C => 1 << 0,
//             Z => 1 << 1,
//             I => 1 << 2,
//             D => 1 << 3,
//             B => 1 << 4,
//             U => 1 << 5,
//             V => 1 << 6,
//             N => 1 << 7,
//         }
//     }
// }
// struct StatusRegister {
//     status: u8,
// }
// impl StatusRegister {
//     fn new() -> Self {
//         let mut ps = Self { status: 0 };
//         ps.set(Flags6502::U, true);
//         ps
//     }
//     fn get(&self, flag: Flags6502) -> bool {
//         (self.status & flag.bit()) != 0
//     }
//     fn set(&mut self, flag: Flags6502, v: bool) {
//         if v {
//             self.status |= flag.bit();
//         } else {
//             self.status &= !flag.bit();
//         }
//     }
//     fn update(&mut self, value: u8) {
//         self.set(Flags6502::Z, value == 0);
//         self.set(Flags6502::N, (value & (1 << 7)) != 0);
//     }
// }

// mod instruction {
//     #![allow(dead_code)]
//     #![allow(non_camel_case_types)]

//     #[repr(u8)]
//     #[derive(Debug, macros::FromHexCode)]
//     pub enum Instruction {
//         BRK_IMP = 0x00,
//         ORA_IZX = 0x01,
//         ORA_ZP0 = 0x05,
//         ASL_ZP0 = 0x06,
//         PHP_IMP = 0x07,
//         ORA_IMM = 0x09,
//         ASL_ACC = 0x0A,
//         ORA_ABS = 0x0D,
//         ASL_ABS = 0x0E,

//         BPL_REL = 0x10,
//         ORA_IZY = 0x11,
//         ORA_ZPX = 0x15,
//         ASL_ZPX = 0x16,
//         CLC_IMP = 0x18,
//         ORA_ABY = 0x19,
//         ORA_ABX = 0x1D,
//         ASL_ABX = 0x1E,

//         JSR_ABS = 0x20,
//         AND_IZX = 0x21,
//         BIT_ZP0 = 0x24,
//         AND_ZP0 = 0x25,
//         ROL_ZP0 = 0x26,
//         PLP_IMP = 0x28,
//         AND_IMM = 0x29,
//         ROL_ACC = 0x2A,
//         BIT_ABS = 0x2C,
//         AND_ABS = 0x2D,
//         ROL_ABS = 0x2E,

//         BMI_REL = 0x30,
//         AND_IZY = 0x31,
//         AND_ZPX = 0x35,
//         ROL_ZPX = 0x36,
//         SEC_IMP = 0x38,
//         AND_ABY = 0x39,
//         AND_ABX = 0x3D,
//         ROL_ABX = 0x3E,

//         RTI_IMP = 0x40,
//         EOR_IZX = 0x41,
//         EOR_ZP0 = 0x45,
//         LSR_ZP0 = 0x46,
//         PHA_IMP = 0x48,
//         EOR_IMM = 0x49,
//         LSR_ACC = 0x4A,
//         JMP_ABS = 0x4C,
//         EOR_ABS = 0x4D,
//         LSR_ABS = 0x4E,

//         BVC_REL = 0x50,
//         EOR_IZY = 0x51,
//         EOR_ZPX = 0x55,
//         LSR_ZPX = 0x56,
//         CLI_IMP = 0x58,
//         EOR_ABY = 0x59,
//         EOR_ABX = 0x5D,
//         LSR_ABX = 0x5E,

//         RTS_IMP = 0x60,
//         ADC_IZX = 0x61,
//         ADC_ZP0 = 0x65,
//         ROR_ZP0 = 0x66,
//         PLA_IMP = 0x68,
//         ADC_IMM = 0x69,
//         ROR_ACC = 0x6A,
//         JMP_IND = 0x6C,
//         ADC_ABS = 0x6D,
//         ROR_ABS = 0x6E,

//         BVS_REL = 0x70,
//         ADC_IZY = 0x71,
//         ADC_ZPX = 0x75,
//         ROR_ZPX = 0x76,
//         SEI_IMP = 0x78,
//         ADC_ABY = 0x79,
//         ADC_ABX = 0x7D,
//         ROR_ABX = 0x7E,

//         STA_IZX = 0x81,
//         STY_ZP0 = 0x84,
//         STA_ZP0 = 0x85,
//         STX_ZP0 = 0x86,
//         DEY_IMP = 0x88,
//         TXA_IMP = 0x8A,
//         STY_ABS = 0x8C,
//         STA_ABS = 0x8D,
//         STX_ABS = 0x8E,

//         BCC_REL = 0x90,
//         STA_IZY = 0x91,
//         STY_ZPX = 0x94,
//         STA_ZPX = 0x95,
//         STX_ZPY = 0x96,
//         TYA_IMP = 0x98,
//         STA_ABY = 0x99,
//         TXS_IMP = 0x9A,
//         STA_ABX = 0x9D,

//         LDY_IMM = 0xA0,
//         LDA_IZX = 0xA1,
//         LDX_IMM = 0xA2,
//         LDY_ZP0 = 0xA4,
//         LDA_ZP0 = 0xA5,
//         LDX_ZP0 = 0xA6,
//         TAY_IMP = 0xA8,
//         LDA_IMM = 0xA9,
//         TAX_IMP = 0xAA,
//         LDY_ABS = 0xAC,
//         LDA_ABS = 0xAD,
//         LDX_ABS = 0xAE,

//         BCS_REL = 0xB0,
//         LDA_IZY = 0xB1,
//         LDY_ZPX = 0xB4,
//         LDA_ZPX = 0xB5,
//         LDX_ZPY = 0xB6,
//         CLV_IMP = 0xB8,
//         LDA_ABY = 0xB9,
//         TSX_IMP = 0xBA,
//         LDY_ABX = 0xBC,
//         LDA_ABX = 0xBD,
//         LDX_ABY = 0xBE,

//         CPY_IMM = 0xC0,
//         CMP_IZX = 0xC1,
//         CPY_ZP0 = 0xC4,
//         CMP_ZP0 = 0xC5,
//         DEC_ZP0 = 0xC6,
//         INY_IMP = 0xC8,
//         CMP_IMM = 0xC9,
//         DEX_IMP = 0xCA,
//         CPY_ABS = 0xCC,
//         CMP_ABS = 0xCD,
//         DEC_ABS = 0xCE,

//         BNE_REL = 0xD0,
//         CMP_IZY = 0xD1,
//         CMP_ZPX = 0xD5,
//         DEC_ZPX = 0xD6,
//         CLD_IMP = 0xD8,
//         CMP_ABY = 0xD9,
//         CMP_ABX = 0xDD,
//         DEC_ABX = 0xDE,

//         CPX_IMM = 0xE0,
//         SBC_IZX = 0xE1,
//         CPX_ZP0 = 0xE4,
//         SBC_ZP0 = 0xE5,
//         INC_ZP0 = 0xE6,
//         INX_IMP = 0xE8,
//         SBC_IMP = 0xE9,
//         NOP_IMP = 0xEA,
//         CPX_ABS = 0xEC,
//         SBC_ABS = 0xED,
//         INC_ABS = 0xEE,

//         BEQ_REL = 0xF0,
//         SBC_IZY = 0xF1,
//         SBC_ZPX = 0xF5,
//         INC_ZPX = 0xF6,
//         SED_IMP = 0xF8,
//         SBC_ABY = 0xF9,
//         SBC_ABX = 0xFD,
//         INC_ABX = 0xFE,
//     }
// }
// use instruction::*;

// pub struct CPU6502 {
//     a: u8,
//     x: u8,
//     y: u8,

//     pc: u16,
//     sp: u8,

//     sr: StatusRegister,

//     bus: Bus,
// }
// impl CPU6502 {
//     pub fn new(bus: Bus) -> Self {
//         let ps = StatusRegister::new();
//         let a = 0;
//         let x = 0;
//         let y = 0;
//         let sp = 0;
//         let pc = 0;

//         let mut cpu = Self {
//             sr: ps,
//             a,
//             x,
//             y,
//             sp,
//             pc,
//             bus,
//         };

//         cpu.reset();

//         cpu
//     }

//     fn fetch_u8(&mut self) -> u8 {
//         let data = self.bus.read_u8(self.pc);
//         self.pc += 1;
//         data
//     }
//     fn fetch_u16(&mut self) -> u16 {
//         let data = self.bus.read_u16(self.pc);
//         self.pc += 2;
//         data
//     }

//     fn reset(&mut self) {
//         self.a = 0;
//         self.x = 0;
//         self.y = 0;

//         self.sr = StatusRegister::new();
//         self.sp = 0xFF;
//         self.pc = self.bus.read_u16(0xFFFC);
//     }

//     fn stk_push_u8(&mut self, data: u8) {
//         self.bus.write_u8(self.stk_addrmode(), data);
//         self.sp -= 1;
//     }
//     fn stk_push_u16(&mut self, data: u16) {
//         self.sp -= 2;
//         self.bus.write_u16(self.stk_addrmode(), data);
//     }
//     fn stk_pull_u8(&mut self) -> u8 {
//         let data = self.bus.read_u8(self.stk_addrmode());
//         self.sp += 1;
//         data
//     }
//     fn stk_pull_u16(&mut self) -> u16 {
//         let data = self.bus.read_u16(self.stk_addrmode());
//         self.sp += 2;
//         data
//     }

//     fn rel_branch(&mut self, offset: u8) {
//         self.pc = self.pc.wrapping_add_signed(offset as i8 as i16);
//     }

//     fn adc(&mut self, num: u8) {
//         // TODO consider D flag
//         // TODO consider V flag

//         let carry = if self.sr.get(Flags6502::C) { 1 } else { 0 };

//         let (result, c) = self.a.overflowing_add(num);
//         let v = result + carry;
//         self.a = v;

//         self.sr.update(v);
//         self.sr.set(Flags6502::C, c);
//     }
//     fn and(&mut self, num: u8) {
//         let result = self.a & num;
//         self.a = result;

//         self.sr.update(result);
//     }
//     fn asl(&mut self, num: u8) -> u8 {
//         let c = (num & (1 << 7)) != 0;
//         let result = num << 1;
//         self.sr.update(result);
//         self.sr.set(Flags6502::C, c);
//         result
//     }
//     fn bcc(&mut self, offset: u8) {
//         if !self.sr.get(Flags6502::C) {
//             self.rel_branch(offset);
//         }
//     }
//     fn bcs(&mut self, offset: u8) {
//         if self.sr.get(Flags6502::C) {
//             self.rel_branch(offset);
//         }
//     }
//     fn beq(&mut self, offset: u8) {
//         if self.sr.get(Flags6502::Z) {
//             self.rel_branch(offset);
//         }
//     }
//     // fn bit(&mut self, ) {
//     // }
//     fn bmi(&mut self, offset: u8) {
//         if self.sr.get(Flags6502::N) {
//             self.rel_branch(offset);
//         }
//     }
//     fn bne(&mut self, offset: u8) {
//         if !self.sr.get(Flags6502::Z) {
//             self.rel_branch(offset);
//         }
//     }
//     fn bpl(&mut self, offset: u8) {
//         if !self.sr.get(Flags6502::N) {
//             self.rel_branch(offset);
//         }
//     }
//     // fn brk(&mut self) {
//     // }
//     fn bvc(&mut self, offset: u8) {
//         if !self.sr.get(Flags6502::V) {
//             self.rel_branch(offset);
//         }
//     }
//     fn bvs(&mut self, offset: u8) {
//         if self.sr.get(Flags6502::V) {
//             self.rel_branch(offset);
//         }
//     }
//     fn clc(&mut self) {
//         self.sr.set(Flags6502::C, false);
//     }
//     fn cld(&mut self) {
//         self.sr.set(Flags6502::D, false);
//     }
//     fn cli(&mut self) {
//         self.sr.set(Flags6502::I, false);
//     }
//     fn clv(&mut self) {
//         self.sr.set(Flags6502::V, false);
//     }
//     fn cmp(&mut self, num: u8) {
//         let c = self.a >= num;
//         let z = self.a == num;
//         let n = !c;
//         self.sr.set(Flags6502::C, c);
//         self.sr.set(Flags6502::Z, z);
//         self.sr.set(Flags6502::Z, n);
//     }
//     fn cpx(&mut self, num: u8) {
//         let c = self.x >= num;
//         let z = self.x == num;
//         let n = !c;
//         self.sr.set(Flags6502::C, c);
//         self.sr.set(Flags6502::Z, z);
//         self.sr.set(Flags6502::Z, n);
//     }
//     fn cpy(&mut self, num: u8) {
//         let c = self.y >= num;
//         let z = self.y == num;
//         let n = !c;
//         self.sr.set(Flags6502::C, c);
//         self.sr.set(Flags6502::Z, z);
//         self.sr.set(Flags6502::Z, n);
//     }
//     fn dec(&mut self, num: u8) -> u8 {
//         let result = num - 1;
//         self.sr.update(result);
//         result
//     }
//     fn dex(&mut self) {
//         self.x -= 1;
//         self.sr.update(self.x);
//     }
//     fn dey(&mut self) {
//         self.y -= 1;
//         self.sr.update(self.y);
//     }
//     fn eor(&mut self, num: u8) {
//         self.a ^= num;
//         self.sr.update(self.a);
//     }
//     fn inc(&mut self, num: u8) -> u8 {
//         let result = num + 1;
//         self.sr.update(result);
//         result
//     }
//     fn inx(&mut self) {
//         self.x += 1;
//         self.sr.update(self.x);
//     }
//     fn iny(&mut self) {
//         self.y += 1;
//         self.sr.update(self.y);
//     }
//     fn jmp(&mut self, addr: u16) {
//         self.pc = addr;
//     }
//     fn jsr(&mut self, addr: u16) {
//         self.stk_push_u16(self.pc);
//         self.pc = addr;
//     }
//     fn lda(&mut self, num: u8) {
//         self.a = num;
//         self.sr.update(num);
//     }
//     fn ldx(&mut self, num: u8) {
//         self.x = num;
//         self.sr.update(num);
//     }
//     fn ldy(&mut self, num: u8) {
//         self.y = num;
//         self.sr.update(num);
//     }
//     fn lsr(&mut self, num: u8) -> u8 {
//         let c = (num | 1) != 0;
//         let result = num >> 1;
//         self.sr.set(Flags6502::N, false);
//         self.sr.set(Flags6502::Z, result == 0);
//         self.sr.set(Flags6502::C, c);
//         result
//     }
//     // fn nop(&self) {}
//     fn ora(&mut self, num: u8) {
//         self.a |= num;
//         self.sr.update(self.a);
//     }
//     fn pha(&mut self) {
//         self.stk_push_u8(self.a);
//     }
//     fn php(&mut self) {
//         self.stk_push_u8(self.sr.status);
//     }
//     fn pla(&mut self) {
//         self.a = self.stk_pull_u8();
//     }
//     fn plp(&mut self) {
//         self.sr.status = self.stk_pull_u8();
//     }
//     fn rol(&mut self, num: u8) -> u8 {
//         let c = (num | (1 << 7)) != 0;
//         let carry = if self.sr.get(Flags6502::C) { 1 } else { 0 };
//         let result = (num << 1) | carry;
//         self.sr.set(Flags6502::C, c);
//         self.sr.update(result);
//         result
//     }
//     fn ror(&mut self, num: u8) -> u8 {
//         let c = (num | 1) != 0;
//         let carry = if self.sr.get(Flags6502::C) { 1 << 7 } else { 0 };
//         let result = (num >> 1) | carry;
//         self.sr.set(Flags6502::C, c);
//         self.sr.update(result);
//         result
//     }
//     // fn rti() {}
//     fn rts(&mut self) {
//         let addr = self.stk_pull_u16();
//         self.pc = addr;
//     }
//     // fn sbc(&mut self) {}
//     fn sec(&mut self) {
//         self.sr.set(Flags6502::C, true);
//     }
//     fn sed(&mut self) {
//         self.sr.set(Flags6502::D, true);
//     }
//     fn sei(&mut self) {
//         self.sr.set(Flags6502::I, true);
//     }
//     fn sta(&mut self) {}
//     // fn stx() {}
//     // fn sty() {}
//     // fn tax() {}
//     // fn tay() {}
//     // fn tsx() {}
//     // fn txa() {}
//     // fn txs() {}
//     // fn tya() {}

//     pub fn execute(&mut self) {
//         use Instruction::*;
//         if let Some(op_code) = Instruction::from_u8(self.fetch_u8()) {
//             match op_code {
//                 BRK_IMP => {}
//                 ORA_IZX => {
//                     let addr = self.izx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ORA_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ASL_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.asl(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 PHP_IMP => {
//                     self.php();
//                 }
//                 ORA_IMM => {
//                     let num = self.fetch_u8();
//                     self.ora(num);
//                 }
//                 ASL_ACC => {
//                     let num = self.asl(self.a);
//                     self.a = num;
//                 }
//                 ORA_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ASL_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.asl(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 BPL_REL => {
//                     let offset = self.fetch_u8();
//                     self.bpl(offset);
//                 }
//                 ORA_IZY => {
//                     let addr = self.izy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ORA_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ASL_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.asl(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 CLC_IMP => {
//                     self.clc();
//                 }
//                 ORA_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ORA_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ASL_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.asl(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 JSR_ABS => {
//                     let addr = self.abs_addrmode();
//                     self.jsr(addr);
//                 }
//                 AND_IZX => {
//                     let addr = self.izx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 BIT_ZP0 => {}
//                 AND_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 ROL_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.rol(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 PLP_IMP => {
//                     self.plp();
//                 }
//                 AND_IMM => {
//                     let num = self.fetch_u8();
//                     self.and(num);
//                 }
//                 ROL_ACC => {
//                     self.a = self.rol(self.a);
//                 }
//                 BIT_ABS => {}
//                 AND_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 ROL_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.rol(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 BMI_REL => {
//                     let offset = self.fetch_u8();
//                     self.bmi(offset);
//                 }
//                 AND_IZY => {
//                     let addr = self.izy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 AND_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 ROL_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.rol(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 SEC_IMP => {
//                     self.sec();
//                 }
//                 AND_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 AND_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 ROL_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.rol(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 RTI_IMP => {}
//                 EOR_IZX => {
//                     let addr = self.izx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 EOR_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 LSR_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.lsr(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 PHA_IMP => {
//                     self.pha();
//                 }
//                 EOR_IMM => {
//                     let num = self.fetch_u8();
//                     self.eor(num);
//                 }
//                 LSR_ACC => {
//                     self.a = self.lsr(self.a);
//                 }
//                 JMP_ABS => {
//                     let addr = self.abs_addrmode();
//                     self.jmp(addr);
//                 }
//                 EOR_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 LSR_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.lsr(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 BVC_REL => {
//                     let offset = self.fetch_u8();
//                     self.bvc(offset);
//                 }
//                 EOR_IZY => {
//                     let addr = self.izy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 EOR_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 LSR_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.lsr(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 CLI_IMP => {
//                     self.cli();
//                 }
//                 EOR_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 EOR_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 LSR_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.lsr(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 RTS_IMP => {
//                     self.rts();
//                 }
//                 ADC_IZX => {
//                     let addr = self.izx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ADC_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ROR_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.ror(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 PLA_IMP => {
//                     self.pla();
//                 }
//                 ADC_IMM => {
//                     let num = self.fetch_u8();
//                     self.adc(num);
//                 }
//                 ROR_ACC => {
//                     self.a = self.ror(self.a);
//                 }
//                 JMP_IND => {
//                     let addr = self.ind_addrmode();
//                     self.jmp(addr);
//                 }
//                 ADC_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ROR_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.ror(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 BVS_REL => {
//                     let offset = self.fetch_u8();
//                     self.bvs(offset);
//                 }
//                 ADC_IZY => {
//                     let addr = self.izy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ADC_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ROR_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.ror(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 SEI_IMP => {
//                     self.sei();
//                 }
//                 ADC_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ADC_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ROR_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.ror(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 STA_IZX => {}
//                 STY_ZP0 => {}
//                 STA_ZP0 => {}
//                 STX_ZP0 => {}
//                 DEY_IMP => {
//                     self.dey();
//                 }
//                 TXA_IMP => {}
//                 STY_ABS => {}
//                 STA_ABS => {}
//                 STX_ABS => {}
//                 BCC_REL => {
//                     let offset = self.fetch_u8();
//                     self.bcc(offset);
//                 }
//                 STA_IZY => {}
//                 STY_ZPX => {}
//                 STA_ZPX => {}
//                 STX_ZPY => {}
//                 TYA_IMP => {}
//                 STA_ABY => {}
//                 TXS_IMP => {}
//                 STA_ABX => {}
//                 LDY_IMM => {
//                     let num = self.fetch_u8();
//                     self.ldy(num);
//                 }
//                 LDA_IZX => {
//                     let addr = self.izx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 LDX_IMM => {
//                     let num = self.fetch_u8();
//                     self.ldx(num);
//                 }
//                 LDY_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldy(num);
//                 }
//                 LDA_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 LDX_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldx(num);
//                 }
//                 TAY_IMP => {}
//                 LDA_IMM => {
//                     let num = self.fetch_u8();
//                     self.lda(num);
//                 }
//                 TAX_IMP => {}
//                 LDY_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldy(num);
//                 }
//                 LDA_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 LDX_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldx(num);
//                 }
//                 BCS_REL => {
//                     let offset = self.fetch_u8();
//                     self.bcs(offset);
//                 }
//                 LDA_IZY => {
//                     let addr = self.izy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 LDY_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldy(num);
//                 }
//                 LDA_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 LDX_ZPY => {
//                     let addr = self.zpy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldx(num);
//                 }
//                 CLV_IMP => {
//                     self.clv();
//                 }
//                 LDA_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 TSX_IMP => {}
//                 LDY_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldy(num);
//                 }
//                 LDA_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 LDX_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldx(num);
//                 }
//                 CPY_IMM => {
//                     let num = self.fetch_u8();
//                     self.cpy(num);
//                 }
//                 CMP_IZX => {
//                     let addr = self.izx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 CPY_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cpy(num);
//                 }
//                 CMP_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 DEC_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.dec(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 INY_IMP => {
//                     self.iny();
//                 }
//                 CMP_IMM => {
//                     let num = self.fetch_u8();
//                     self.cmp(num);
//                 }
//                 DEX_IMP => {
//                     self.dex();
//                 }
//                 CPY_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cpy(num);
//                 }
//                 CMP_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 DEC_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.dec(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 BNE_REL => {
//                     let offset = self.fetch_u8();
//                     self.bne(offset);
//                 }
//                 CMP_IZY => {
//                     let addr = self.izy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 CMP_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 DEC_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.dec(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 CLD_IMP => {
//                     self.cld();
//                 }
//                 CMP_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 CMP_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 DEC_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.dec(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 CPX_IMM => {
//                     let num = self.fetch_u8();
//                     self.cpx(num);
//                 }
//                 SBC_IZX => {}
//                 CPX_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cpx(num);
//                 }
//                 SBC_ZP0 => {}
//                 INC_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.inc(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 INX_IMP => {
//                     self.inx();
//                 }
//                 SBC_IMP => {}
//                 NOP_IMP => {
//                     // NOP
//                 }
//                 CPX_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cpx(num);
//                 }
//                 SBC_ABS => {}
//                 INC_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.inc(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 BEQ_REL => {
//                     let offset = self.fetch_u8();
//                     self.beq(offset);
//                 }
//                 SBC_IZY => {}
//                 SBC_ZPX => {}
//                 INC_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.inc(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 SED_IMP => {
//                     self.sed();
//                 }
//                 SBC_ABY => {}
//                 SBC_ABX => {}
//                 INC_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.inc(num);
//                     self.bus.write_u8(addr, num);
//                 }
//             }
//         } else {
//             panic!("BAD OPCODE");
//         }
//     }

//     fn abs_addrmode(&mut self) -> u16 {
//         self.fetch_u16()
//     }
//     fn abx_addrmode(&mut self) -> u16 {
//         self.fetch_u16() + self.x as u16
//     }
//     fn aby_addrmode(&mut self) -> u16 {
//         self.fetch_u16() + self.y as u16
//     }
//     fn zp0_addrmode(&mut self) -> u16 {
//         u16::from_le_bytes([self.fetch_u8(), 0x00])
//     }
//     fn zpx_addrmode(&mut self) -> u16 {
//         u16::from_le_bytes([self.fetch_u8() + self.x, 0x00])
//     }
//     fn zpy_addrmode(&mut self) -> u16 {
//         u16::from_le_bytes([self.fetch_u8() + self.y, 0x00])
//     }
//     // fn izp_addrmode(&mut self) -> u16 {
//     //     let ptr = self.zp0_addrmode();
//     //     self.bus.read_u16(ptr)
//     // }
//     fn izx_addrmode(&mut self) -> u16 {
//         let ptr = self.zpx_addrmode();
//         self.bus.read_u16(ptr)
//     }
//     fn izy_addrmode(&mut self) -> u16 {
//         let ptr = self.zp0_addrmode() + self.y as u16;
//         self.bus.read_u16(ptr)
//     }
//     fn ind_addrmode(&mut self) -> u16 {
//         // TODO: impl page change bug
//         let ptr = self.fetch_u16();
//         self.bus.read_u16(ptr)
//     }
//     fn stk_addrmode(&self) -> u16 {
//         u16::from_le_bytes([self.sp, 0x01])
//     }
// }
// impl Display for CPU6502 {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let opcode = self.bus.read_u8(self.pc);
//         writeln!(f, "Program Counter: {:#06X}", self.pc)?;
//         writeln!(f, "Accumlator:       u:{} s:{}", self.a, self.a as i8)?;
//         writeln!(f, "Register X:       u:{} s:{}", self.x, self.x as i8)?;
//         writeln!(f, "Register Y:       u:{} s:{}", self.y, self.y as i8)?;
//         writeln!(f, "Stack Ptr:        {:#04X}", self.sp)?;
//         writeln!(f, "                  NV-BDIZC")?;
//         writeln!(f, "Process Status:   {:08b}", self.sr.status)?;
//         writeln!(
//             f,
//             "Next Instruction: {:#04X} ({:?})",
//             opcode,
//             Instruction::from_u8(opcode).unwrap_or(Instruction::NOP_IMP)
//         )
//     }
// }

// pub struct Bus {
//     pub ram: [u8; 64 * 1024],
// }
// impl Bus {
//     pub fn new() -> Self {
//         let ram = [0x00; 64 * 1024];
//         Self { ram }
//     }
//     fn write_u8(&mut self, addr: u16, data: u8) {
//         self.ram[addr as usize] = data;
//     }
//     fn write_u16(&mut self, addr: u16, data: u16) {
//         let addr = addr as usize;
//         let data = data.to_le_bytes();
//         self.ram[addr] = data[0];
//         self.ram[addr + 1] = data[1];
//     }
//     fn read_u8(&self, addr: u16) -> u8 {
//         self.ram[addr as usize]
//     }
//     fn read_u16(&self, addr: u16) -> u16 {
//         let addr = addr as usize;
//         u16::from_le_bytes([self.ram[addr], self.ram[addr + 1]])
//  use std::fmt::Display;

// // mod Flags6502 {
// //     pub const C: u8 = 1 << 0; // Carry            1 = true
// //     pub const Z: u8 = 1 << 1; // Zero             1 = true
// //     pub const I: u8 = 1 << 2; // IRQB ennable     1 = disable
// //     pub const D: u8 = 1 << 3; // Decimal mode     1 = true
// //     pub const B: u8 = 1 << 4; // BRK command      1 = BRK, 0 = IRQB
// //     pub const U: u8 = 1 << 5; // Unused
// //     pub const V: u8 = 1 << 6; // Overflow         1 = true
// //     pub const N: u8 = 1 << 7; // Negative         1 = neg
// // }

// enum Flags6502 {
//     C,
//     Z,
//     I,
//     D,
//     B,
//     U,
//     V,
//     N,
// }
// impl Flags6502 {
//     fn bit(self) -> u8 {
//         use Flags6502::*;
//         match self {
//             C => 1 << 0,
//             Z => 1 << 1,
//             I => 1 << 2,
//             D => 1 << 3,
//             B => 1 << 4,
//             U => 1 << 5,
//             V => 1 << 6,
//             N => 1 << 7,
//         }
//     }
// }
// struct StatusRegister {
//     status: u8,
// }
// impl StatusRegister {
//     fn new() -> Self {
//         let mut ps = Self { status: 0 };
//         ps.set(Flags6502::U, true);
//         ps
//     }
//     fn get(&self, flag: Flags6502) -> bool {
//         (self.status & flag.bit()) != 0
//     }
//     fn set(&mut self, flag: Flags6502, v: bool) {
//         if v {
//             self.status |= flag.bit();
//         } else {
//             self.status &= !flag.bit();
//         }
//     }
//     fn update(&mut self, value: u8) {
//         self.set(Flags6502::Z, value == 0);
//         self.set(Flags6502::N, (value & (1 << 7)) != 0);
//     }
// }

// mod instruction {
//     #![allow(dead_code)]
//     #![allow(non_camel_case_types)]

//     #[repr(u8)]
//     #[derive(Debug, macros::FromHexCode)]
//     pub enum Instruction {
//         BRK_IMP = 0x00,
//         ORA_IZX = 0x01,
//         ORA_ZP0 = 0x05,
//         ASL_ZP0 = 0x06,
//         PHP_IMP = 0x07,
//         ORA_IMM = 0x09,
//         ASL_ACC = 0x0A,
//         ORA_ABS = 0x0D,
//         ASL_ABS = 0x0E,

//         BPL_REL = 0x10,
//         ORA_IZY = 0x11,
//         ORA_ZPX = 0x15,
//         ASL_ZPX = 0x16,
//         CLC_IMP = 0x18,
//         ORA_ABY = 0x19,
//         ORA_ABX = 0x1D,
//         ASL_ABX = 0x1E,

//         JSR_ABS = 0x20,
//         AND_IZX = 0x21,
//         BIT_ZP0 = 0x24,
//         AND_ZP0 = 0x25,
//         ROL_ZP0 = 0x26,
//         PLP_IMP = 0x28,
//         AND_IMM = 0x29,
//         ROL_ACC = 0x2A,
//         BIT_ABS = 0x2C,
//         AND_ABS = 0x2D,
//         ROL_ABS = 0x2E,

//         BMI_REL = 0x30,
//         AND_IZY = 0x31,
//         AND_ZPX = 0x35,
//         ROL_ZPX = 0x36,
//         SEC_IMP = 0x38,
//         AND_ABY = 0x39,
//         AND_ABX = 0x3D,
//         ROL_ABX = 0x3E,

//         RTI_IMP = 0x40,
//         EOR_IZX = 0x41,
//         EOR_ZP0 = 0x45,
//         LSR_ZP0 = 0x46,
//         PHA_IMP = 0x48,
//         EOR_IMM = 0x49,
//         LSR_ACC = 0x4A,
//         JMP_ABS = 0x4C,
//         EOR_ABS = 0x4D,
//         LSR_ABS = 0x4E,

//         BVC_REL = 0x50,
//         EOR_IZY = 0x51,
//         EOR_ZPX = 0x55,
//         LSR_ZPX = 0x56,
//         CLI_IMP = 0x58,
//         EOR_ABY = 0x59,
//         EOR_ABX = 0x5D,
//         LSR_ABX = 0x5E,

//         RTS_IMP = 0x60,
//         ADC_IZX = 0x61,
//         ADC_ZP0 = 0x65,
//         ROR_ZP0 = 0x66,
//         PLA_IMP = 0x68,
//         ADC_IMM = 0x69,
//         ROR_ACC = 0x6A,
//         JMP_IND = 0x6C,
//         ADC_ABS = 0x6D,
//         ROR_ABS = 0x6E,

//         BVS_REL = 0x70,
//         ADC_IZY = 0x71,
//         ADC_ZPX = 0x75,
//         ROR_ZPX = 0x76,
//         SEI_IMP = 0x78,
//         ADC_ABY = 0x79,
//         ADC_ABX = 0x7D,
//         ROR_ABX = 0x7E,

//         STA_IZX = 0x81,
//         STY_ZP0 = 0x84,
//         STA_ZP0 = 0x85,
//         STX_ZP0 = 0x86,
//         DEY_IMP = 0x88,
//         TXA_IMP = 0x8A,
//         STY_ABS = 0x8C,
//         STA_ABS = 0x8D,
//         STX_ABS = 0x8E,

//         BCC_REL = 0x90,
//         STA_IZY = 0x91,
//         STY_ZPX = 0x94,
//         STA_ZPX = 0x95,
//         STX_ZPY = 0x96,
//         TYA_IMP = 0x98,
//         STA_ABY = 0x99,
//         TXS_IMP = 0x9A,
//         STA_ABX = 0x9D,

//         LDY_IMM = 0xA0,
//         LDA_IZX = 0xA1,
//         LDX_IMM = 0xA2,
//         LDY_ZP0 = 0xA4,
//         LDA_ZP0 = 0xA5,
//         LDX_ZP0 = 0xA6,
//         TAY_IMP = 0xA8,
//         LDA_IMM = 0xA9,
//         TAX_IMP = 0xAA,
//         LDY_ABS = 0xAC,
//         LDA_ABS = 0xAD,
//         LDX_ABS = 0xAE,

//         BCS_REL = 0xB0,
//         LDA_IZY = 0xB1,
//         LDY_ZPX = 0xB4,
//         LDA_ZPX = 0xB5,
//         LDX_ZPY = 0xB6,
//         CLV_IMP = 0xB8,
//         LDA_ABY = 0xB9,
//         TSX_IMP = 0xBA,
//         LDY_ABX = 0xBC,
//         LDA_ABX = 0xBD,
//         LDX_ABY = 0xBE,

//         CPY_IMM = 0xC0,
//         CMP_IZX = 0xC1,
//         CPY_ZP0 = 0xC4,
//         CMP_ZP0 = 0xC5,
//         DEC_ZP0 = 0xC6,
//         INY_IMP = 0xC8,
//         CMP_IMM = 0xC9,
//         DEX_IMP = 0xCA,
//         CPY_ABS = 0xCC,
//         CMP_ABS = 0xCD,
//         DEC_ABS = 0xCE,

//         BNE_REL = 0xD0,
//         CMP_IZY = 0xD1,
//         CMP_ZPX = 0xD5,
//         DEC_ZPX = 0xD6,
//         CLD_IMP = 0xD8,
//         CMP_ABY = 0xD9,
//         CMP_ABX = 0xDD,
//         DEC_ABX = 0xDE,

//         CPX_IMM = 0xE0,
//         SBC_IZX = 0xE1,
//         CPX_ZP0 = 0xE4,
//         SBC_ZP0 = 0xE5,
//         INC_ZP0 = 0xE6,
//         INX_IMP = 0xE8,
//         SBC_IMP = 0xE9,
//         NOP_IMP = 0xEA,
//         CPX_ABS = 0xEC,
//         SBC_ABS = 0xED,
//         INC_ABS = 0xEE,

//         BEQ_REL = 0xF0,
//         SBC_IZY = 0xF1,
//         SBC_ZPX = 0xF5,
//         INC_ZPX = 0xF6,
//         SED_IMP = 0xF8,
//         SBC_ABY = 0xF9,
//         SBC_ABX = 0xFD,
//         INC_ABX = 0xFE,
//     }
// }
// use instruction::*;

// pub struct CPU6502 {
//     a: u8,
//     x: u8,
//     y: u8,

//     pc: u16,
//     sp: u8,

//     sr: StatusRegister,

//     bus: Bus,
// }
// impl CPU6502 {
//     pub fn new(bus: Bus) -> Self {
//         let ps = StatusRegister::new();
//         let a = 0;
//         let x = 0;
//         let y = 0;
//         let sp = 0;
//         let pc = 0;

//         let mut cpu = Self {
//             sr: ps,
//             a,
//             x,
//             y,
//             sp,
//             pc,
//             bus,
//         };

//         cpu.reset();

//         cpu
//     }

//     fn fetch_u8(&mut self) -> u8 {
//         let data = self.bus.read_u8(self.pc);
//         self.pc += 1;
//         data
//     }
//     fn fetch_u16(&mut self) -> u16 {
//         let data = self.bus.read_u16(self.pc);
//         self.pc += 2;
//         data
//     }

//     fn reset(&mut self) {
//         self.a = 0;
//         self.x = 0;
//         self.y = 0;

//         self.sr = StatusRegister::new();
//         self.sp = 0xFF;
//         self.pc = self.bus.read_u16(0xFFFC);
//     }

//     fn stk_push_u8(&mut self, data: u8) {
//         self.bus.write_u8(self.stk_addrmode(), data);
//         self.sp -= 1;
//     }
//     fn stk_push_u16(&mut self, data: u16) {
//         self.sp -= 2;
//         self.bus.write_u16(self.stk_addrmode(), data);
//     }
//     fn stk_pull_u8(&mut self) -> u8 {
//         let data = self.bus.read_u8(self.stk_addrmode());
//         self.sp += 1;
//         data
//     }
//     fn stk_pull_u16(&mut self) -> u16 {
//         let data = self.bus.read_u16(self.stk_addrmode());
//         self.sp += 2;
//         data
//     }

//     fn rel_branch(&mut self, offset: u8) {
//         self.pc = self.pc.wrapping_add_signed(offset as i8 as i16);
//     }

//     fn adc(&mut self, num: u8) {
//         // TODO consider D flag
//         // TODO consider V flag

//         let carry = if self.sr.get(Flags6502::C) { 1 } else { 0 };

//         let (result, c) = self.a.overflowing_add(num);
//         let v = result + carry;
//         self.a = v;

//         self.sr.update(v);
//         self.sr.set(Flags6502::C, c);
//     }
//     fn and(&mut self, num: u8) {
//         let result = self.a & num;
//         self.a = result;

//         self.sr.update(result);
//     }
//     fn asl(&mut self, num: u8) -> u8 {
//         let c = (num & (1 << 7)) != 0;
//         let result = num << 1;
//         self.sr.update(result);
//         self.sr.set(Flags6502::C, c);
//         result
//     }
//     fn bcc(&mut self, offset: u8) {
//         if !self.sr.get(Flags6502::C) {
//             self.rel_branch(offset);
//         }
//     }
//     fn bcs(&mut self, offset: u8) {
//         if self.sr.get(Flags6502::C) {
//             self.rel_branch(offset);
//         }
//     }
//     fn beq(&mut self, offset: u8) {
//         if self.sr.get(Flags6502::Z) {
//             self.rel_branch(offset);
//         }
//     }
//     // fn bit(&mut self, ) {
//     // }
//     fn bmi(&mut self, offset: u8) {
//         if self.sr.get(Flags6502::N) {
//             self.rel_branch(offset);
//         }
//     }
//     fn bne(&mut self, offset: u8) {
//         if !self.sr.get(Flags6502::Z) {
//             self.rel_branch(offset);
//         }
//     }
//     fn bpl(&mut self, offset: u8) {
//         if !self.sr.get(Flags6502::N) {
//             self.rel_branch(offset);
//         }
//     }
//     // fn brk(&mut self) {
//     // }
//     fn bvc(&mut self, offset: u8) {
//         if !self.sr.get(Flags6502::V) {
//             self.rel_branch(offset);
//         }
//     }
//     fn bvs(&mut self, offset: u8) {
//         if self.sr.get(Flags6502::V) {
//             self.rel_branch(offset);
//         }
//     }
//     fn clc(&mut self) {
//         self.sr.set(Flags6502::C, false);
//     }
//     fn cld(&mut self) {
//         self.sr.set(Flags6502::D, false);
//     }
//     fn cli(&mut self) {
//         self.sr.set(Flags6502::I, false);
//     }
//     fn clv(&mut self) {
//         self.sr.set(Flags6502::V, false);
//     }
//     fn cmp(&mut self, num: u8) {
//         let c = self.a >= num;
//         let z = self.a == num;
//         let n = !c;
//         self.sr.set(Flags6502::C, c);
//         self.sr.set(Flags6502::Z, z);
//         self.sr.set(Flags6502::Z, n);
//     }
//     fn cpx(&mut self, num: u8) {
//         let c = self.x >= num;
//         let z = self.x == num;
//         let n = !c;
//         self.sr.set(Flags6502::C, c);
//         self.sr.set(Flags6502::Z, z);
//         self.sr.set(Flags6502::Z, n);
//     }
//     fn cpy(&mut self, num: u8) {
//         let c = self.y >= num;
//         let z = self.y == num;
//         let n = !c;
//         self.sr.set(Flags6502::C, c);
//         self.sr.set(Flags6502::Z, z);
//         self.sr.set(Flags6502::Z, n);
//     }
//     fn dec(&mut self, num: u8) -> u8 {
//         let result = num - 1;
//         self.sr.update(result);
//         result
//     }
//     fn dex(&mut self) {
//         self.x -= 1;
//         self.sr.update(self.x);
//     }
//     fn dey(&mut self) {
//         self.y -= 1;
//         self.sr.update(self.y);
//     }
//     fn eor(&mut self, num: u8) {
//         self.a ^= num;
//         self.sr.update(self.a);
//     }
//     fn inc(&mut self, num: u8) -> u8 {
//         let result = num + 1;
//         self.sr.update(result);
//         result
//     }
//     fn inx(&mut self) {
//         self.x += 1;
//         self.sr.update(self.x);
//     }
//     fn iny(&mut self) {
//         self.y += 1;
//         self.sr.update(self.y);
//     }
//     fn jmp(&mut self, addr: u16) {
//         self.pc = addr;
//     }
//     fn jsr(&mut self, addr: u16) {
//         self.stk_push_u16(self.pc);
//         self.pc = addr;
//     }
//     fn lda(&mut self, num: u8) {
//         self.a = num;
//         self.sr.update(num);
//     }
//     fn ldx(&mut self, num: u8) {
//         self.x = num;
//         self.sr.update(num);
//     }
//     fn ldy(&mut self, num: u8) {
//         self.y = num;
//         self.sr.update(num);
//     }
//     fn lsr(&mut self, num: u8) -> u8 {
//         let c = (num | 1) != 0;
//         let result = num >> 1;
//         self.sr.set(Flags6502::N, false);
//         self.sr.set(Flags6502::Z, result == 0);
//         self.sr.set(Flags6502::C, c);
//         result
//     }
//     // fn nop(&self) {}
//     fn ora(&mut self, num: u8) {
//         self.a |= num;
//         self.sr.update(self.a);
//     }
//     fn pha(&mut self) {
//         self.stk_push_u8(self.a);
//     }
//     fn php(&mut self) {
//         self.stk_push_u8(self.sr.status);
//     }
//     fn pla(&mut self) {
//         self.a = self.stk_pull_u8();
//     }
//     fn plp(&mut self) {
//         self.sr.status = self.stk_pull_u8();
//     }
//     fn rol(&mut self, num: u8) -> u8 {
//         let c = (num | (1 << 7)) != 0;
//         let carry = if self.sr.get(Flags6502::C) { 1 } else { 0 };
//         let result = (num << 1) | carry;
//         self.sr.set(Flags6502::C, c);
//         self.sr.update(result);
//         result
//     }
//     fn ror(&mut self, num: u8) -> u8 {
//         let c = (num | 1) != 0;
//         let carry = if self.sr.get(Flags6502::C) { 1 << 7 } else { 0 };
//         let result = (num >> 1) | carry;
//         self.sr.set(Flags6502::C, c);
//         self.sr.update(result);
//         result
//     }
//     // fn rti() {}
//     fn rts(&mut self) {
//         let addr = self.stk_pull_u16();
//         self.pc = addr;
//     }
//     // fn sbc(&mut self) {}
//     fn sec(&mut self) {
//         self.sr.set(Flags6502::C, true);
//     }
//     fn sed(&mut self) {
//         self.sr.set(Flags6502::D, true);
//     }
//     fn sei(&mut self) {
//         self.sr.set(Flags6502::I, true);
//     }
//     fn sta(&mut self) {}
//     // fn stx() {}
//     // fn sty() {}
//     // fn tax() {}
//     // fn tay() {}
//     // fn tsx() {}
//     // fn txa() {}
//     // fn txs() {}
//     // fn tya() {}

//     pub fn execute(&mut self) {
//         use Instruction::*;
//         if let Some(op_code) = Instruction::from_u8(self.fetch_u8()) {
//             match op_code {
//                 BRK_IMP => {}
//                 ORA_IZX => {
//                     let addr = self.izx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ORA_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ASL_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.asl(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 PHP_IMP => {
//                     self.php();
//                 }
//                 ORA_IMM => {
//                     let num = self.fetch_u8();
//                     self.ora(num);
//                 }
//                 ASL_ACC => {
//                     let num = self.asl(self.a);
//                     self.a = num;
//                 }
//                 ORA_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ASL_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.asl(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 BPL_REL => {
//                     let offset = self.fetch_u8();
//                     self.bpl(offset);
//                 }
//                 ORA_IZY => {
//                     let addr = self.izy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ORA_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ASL_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.asl(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 CLC_IMP => {
//                     self.clc();
//                 }
//                 ORA_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ORA_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ora(num);
//                 }
//                 ASL_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.asl(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 JSR_ABS => {
//                     let addr = self.abs_addrmode();
//                     self.jsr(addr);
//                 }
//                 AND_IZX => {
//                     let addr = self.izx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 BIT_ZP0 => {}
//                 AND_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 ROL_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.rol(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 PLP_IMP => {
//                     self.plp();
//                 }
//                 AND_IMM => {
//                     let num = self.fetch_u8();
//                     self.and(num);
//                 }
//                 ROL_ACC => {
//                     self.a = self.rol(self.a);
//                 }
//                 BIT_ABS => {}
//                 AND_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 ROL_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.rol(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 BMI_REL => {
//                     let offset = self.fetch_u8();
//                     self.bmi(offset);
//                 }
//                 AND_IZY => {
//                     let addr = self.izy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 AND_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 ROL_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.rol(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 SEC_IMP => {
//                     self.sec();
//                 }
//                 AND_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 AND_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.and(num);
//                 }
//                 ROL_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.rol(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 RTI_IMP => {}
//                 EOR_IZX => {
//                     let addr = self.izx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 EOR_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 LSR_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.lsr(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 PHA_IMP => {
//                     self.pha();
//                 }
//                 EOR_IMM => {
//                     let num = self.fetch_u8();
//                     self.eor(num);
//                 }
//                 LSR_ACC => {
//                     self.a = self.lsr(self.a);
//                 }
//                 JMP_ABS => {
//                     let addr = self.abs_addrmode();
//                     self.jmp(addr);
//                 }
//                 EOR_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 LSR_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.lsr(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 BVC_REL => {
//                     let offset = self.fetch_u8();
//                     self.bvc(offset);
//                 }
//                 EOR_IZY => {
//                     let addr = self.izy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 EOR_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 LSR_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.lsr(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 CLI_IMP => {
//                     self.cli();
//                 }
//                 EOR_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 EOR_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.eor(num);
//                 }
//                 LSR_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.lsr(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 RTS_IMP => {
//                     self.rts();
//                 }
//                 ADC_IZX => {
//                     let addr = self.izx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ADC_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ROR_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.ror(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 PLA_IMP => {
//                     self.pla();
//                 }
//                 ADC_IMM => {
//                     let num = self.fetch_u8();
//                     self.adc(num);
//                 }
//                 ROR_ACC => {
//                     self.a = self.ror(self.a);
//                 }
//                 JMP_IND => {
//                     let addr = self.ind_addrmode();
//                     self.jmp(addr);
//                 }
//                 ADC_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ROR_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.ror(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 BVS_REL => {
//                     let offset = self.fetch_u8();
//                     self.bvs(offset);
//                 }
//                 ADC_IZY => {
//                     let addr = self.izy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ADC_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ROR_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.ror(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 SEI_IMP => {
//                     self.sei();
//                 }
//                 ADC_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ADC_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.adc(num);
//                 }
//                 ROR_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.ror(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 STA_IZX => {}
//                 STY_ZP0 => {}
//                 STA_ZP0 => {}
//                 STX_ZP0 => {}
//                 DEY_IMP => {
//                     self.dey();
//                 }
//                 TXA_IMP => {}
//                 STY_ABS => {}
//                 STA_ABS => {}
//                 STX_ABS => {}
//                 BCC_REL => {
//                     let offset = self.fetch_u8();
//                     self.bcc(offset);
//                 }
//                 STA_IZY => {}
//                 STY_ZPX => {}
//                 STA_ZPX => {}
//                 STX_ZPY => {}
//                 TYA_IMP => {}
//                 STA_ABY => {}
//                 TXS_IMP => {}
//                 STA_ABX => {}
//                 LDY_IMM => {
//                     let num = self.fetch_u8();
//                     self.ldy(num);
//                 }
//                 LDA_IZX => {
//                     let addr = self.izx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 LDX_IMM => {
//                     let num = self.fetch_u8();
//                     self.ldx(num);
//                 }
//                 LDY_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldy(num);
//                 }
//                 LDA_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 LDX_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldx(num);
//                 }
//                 TAY_IMP => {}
//                 LDA_IMM => {
//                     let num = self.fetch_u8();
//                     self.lda(num);
//                 }
//                 TAX_IMP => {}
//                 LDY_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldy(num);
//                 }
//                 LDA_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 LDX_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldx(num);
//                 }
//                 BCS_REL => {
//                     let offset = self.fetch_u8();
//                     self.bcs(offset);
//                 }
//                 LDA_IZY => {
//                     let addr = self.izy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 LDY_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldy(num);
//                 }
//                 LDA_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 LDX_ZPY => {
//                     let addr = self.zpy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldx(num);
//                 }
//                 CLV_IMP => {
//                     self.clv();
//                 }
//                 LDA_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 TSX_IMP => {}
//                 LDY_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldy(num);
//                 }
//                 LDA_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.lda(num);
//                 }
//                 LDX_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.ldx(num);
//                 }
//                 CPY_IMM => {
//                     let num = self.fetch_u8();
//                     self.cpy(num);
//                 }
//                 CMP_IZX => {
//                     let addr = self.izx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 CPY_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cpy(num);
//                 }
//                 CMP_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 DEC_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.dec(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 INY_IMP => {
//                     self.iny();
//                 }
//                 CMP_IMM => {
//                     let num = self.fetch_u8();
//                     self.cmp(num);
//                 }
//                 DEX_IMP => {
//                     self.dex();
//                 }
//                 CPY_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cpy(num);
//                 }
//                 CMP_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 DEC_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.dec(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 BNE_REL => {
//                     let offset = self.fetch_u8();
//                     self.bne(offset);
//                 }
//                 CMP_IZY => {
//                     let addr = self.izy_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 CMP_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 DEC_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.dec(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 CLD_IMP => {
//                     self.cld();
//                 }
//                 CMP_ABY => {
//                     let addr = self.aby_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 CMP_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cmp(num);
//                 }
//                 DEC_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.dec(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 CPX_IMM => {
//                     let num = self.fetch_u8();
//                     self.cpx(num);
//                 }
//                 SBC_IZX => {}
//                 CPX_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cpx(num);
//                 }
//                 SBC_ZP0 => {}
//                 INC_ZP0 => {
//                     let addr = self.zp0_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.inc(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 INX_IMP => {
//                     self.inx();
//                 }
//                 SBC_IMP => {}
//                 NOP_IMP => {
//                     // NOP
//                 }
//                 CPX_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     self.cpx(num);
//                 }
//                 SBC_ABS => {}
//                 INC_ABS => {
//                     let addr = self.abs_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.inc(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 BEQ_REL => {
//                     let offset = self.fetch_u8();
//                     self.beq(offset);
//                 }
//                 SBC_IZY => {}
//                 SBC_ZPX => {}
//                 INC_ZPX => {
//                     let addr = self.zpx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.inc(num);
//                     self.bus.write_u8(addr, num);
//                 }
//                 SED_IMP => {
//                     self.sed();
//                 }
//                 SBC_ABY => {}
//                 SBC_ABX => {}
//                 INC_ABX => {
//                     let addr = self.abx_addrmode();
//                     let num = self.bus.read_u8(addr);
//                     let num = self.inc(num);
//                     self.bus.write_u8(addr, num);
//                 }
//             }
//         } else {
//             panic!("BAD OPCODE");
//         }
//     }

//     fn abs_addrmode(&mut self) -> u16 {
//         self.fetch_u16()
//     }
//     fn abx_addrmode(&mut self) -> u16 {
//         self.fetch_u16() + self.x as u16
//     }
//     fn aby_addrmode(&mut self) -> u16 {
//         self.fetch_u16() + self.y as u16
//     }
//     fn zp0_addrmode(&mut self) -> u16 {
//         u16::from_le_bytes([self.fetch_u8(), 0x00])
//     }
//     fn zpx_addrmode(&mut self) -> u16 {
//         u16::from_le_bytes([self.fetch_u8() + self.x, 0x00])
//     }
//     fn zpy_addrmode(&mut self) -> u16 {
//         u16::from_le_bytes([self.fetch_u8() + self.y, 0x00])
//     }
//     // fn izp_addrmode(&mut self) -> u16 {
//     //     let ptr = self.zp0_addrmode();
//     //     self.bus.read_u16(ptr)
//     // }
//     fn izx_addrmode(&mut self) -> u16 {
//         let ptr = self.zpx_addrmode();
//         self.bus.read_u16(ptr)
//     }
//     fn izy_addrmode(&mut self) -> u16 {
//         let ptr = self.zp0_addrmode() + self.y as u16;
//         self.bus.read_u16(ptr)
//     }
//     fn ind_addrmode(&mut self) -> u16 {
//         // TODO: impl page change bug
//         let ptr = self.fetch_u16();
//         self.bus.read_u16(ptr)
//     }
//     fn stk_addrmode(&self) -> u16 {
//         u16::from_le_bytes([self.sp, 0x01])
//     }
// }
// impl Display for CPU6502 {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let opcode = self.bus.read_u8(self.pc);
//         writeln!(f, "Program Counter: {:#06X}", self.pc)?;
//         writeln!(f, "Accumlator:       u:{} s:{}", self.a, self.a as i8)?;
//         writeln!(f, "Register X:       u:{} s:{}", self.x, self.x as i8)?;
//         writeln!(f, "Register Y:       u:{} s:{}", self.y, self.y as i8)?;
//         writeln!(f, "Stack Ptr:        {:#04X}", self.sp)?;
//         writeln!(f, "                  NV-BDIZC")?;
//         writeln!(f, "Process Status:   {:08b}", self.sr.status)?;
//         writeln!(
//             f,
//             "Next Instruction: {:#04X} ({:?})",
//             opcode,
//             Instruction::from_u8(opcode).unwrap_or(Instruction::NOP_IMP)
//         )
//     }
// }

// pub struct Bus {
//     pub ram: [u8; 64 * 1024],
// }
// impl Bus {
//     pub fn new() -> Self {
//         let ram = [0x00; 64 * 1024];
//         Self { ram }
//     }
//     fn write_u8(&mut self, addr: u16, data: u8) {
//         self.ram[addr as usize] = data;
//     }
//     fn write_u16(&mut self, addr: u16, data: u16) {
//         let addr = addr as usize;
//         let data = data.to_le_bytes();
//         self.ram[addr] = data[0];
//         self.ram[addr + 1] = data[1];
//     }
//     fn read_u8(&self, addr: u16) -> u8 {
//         self.ram[addr as usize]
//     }
//     fn read_u16(&self, addr: u16) -> u16 {
//         let addr = addr as usize;
//         u16::from_le_bytes([self.ram[addr], self.ram[addr + 1]])
//     }
// }
