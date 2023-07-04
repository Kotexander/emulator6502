enum Flags6502 {
    C,
    Z,
    I,
    D,
    B,
    U,
    V,
    N,
}
impl Flags6502 {
    fn bit(self) -> u8 {
        use Flags6502::*;
        match self {
            C => 1 << 0,
            Z => 1 << 1,
            I => 1 << 2,
            D => 1 << 3,
            B => 1 << 4,
            U => 1 << 5,
            V => 1 << 6,
            N => 1 << 7,
        }
    }
}

#[derive(Clone, Copy)]
struct StatusRegister {
    status: u8,
}
impl StatusRegister {
    fn new() -> Self {
        let mut ps = Self { status: 0 };
        ps.set(Flags6502::U, true);
        ps
    }
    fn get(&self, flag: Flags6502) -> bool {
        (self.status & flag.bit()) != 0
    }
    fn set(&mut self, flag: Flags6502, v: bool) {
        if v {
            self.status |= flag.bit();
        } else {
            self.status &= !flag.bit();
        }
    }
    fn update(&mut self, value: u8) {
        self.set(Flags6502::Z, value == 0);
        self.set(Flags6502::N, (value & (1 << 7)) != 0);
    }
}
impl From<StatusRegister> for u8 {
    fn from(value: StatusRegister) -> Self {
        value.status
    }
}
impl From<u8> for StatusRegister {
    fn from(value: u8) -> Self {
        Self { status: value }
    }
}

mod opcodes {
    #![allow(non_camel_case_types)]

    #[repr(u8)]
    #[derive(Debug, macros::FromHexCode)]
    pub enum Instruction {
        BRK_IMP = 0x00,
        ORA_IZX = 0x01,
        ORA_ZP0 = 0x05,
        ASL_ZP0 = 0x06,
        PHP_IMP = 0x07,
        ORA_IMM = 0x09,
        ASL_ACC = 0x0A,
        ORA_ABS = 0x0D,
        ASL_ABS = 0x0E,

        BPL_REL = 0x10,
        ORA_IZY = 0x11,
        ORA_ZPX = 0x15,
        ASL_ZPX = 0x16,
        CLC_IMP = 0x18,
        ORA_ABY = 0x19,
        ORA_ABX = 0x1D,
        ASL_ABX = 0x1E,

        JSR_ABS = 0x20,
        AND_IZX = 0x21,
        BIT_ZP0 = 0x24,
        AND_ZP0 = 0x25,
        ROL_ZP0 = 0x26,
        PLP_IMP = 0x28,
        AND_IMM = 0x29,
        ROL_ACC = 0x2A,
        BIT_ABS = 0x2C,
        AND_ABS = 0x2D,
        ROL_ABS = 0x2E,

        BMI_REL = 0x30,
        AND_IZY = 0x31,
        AND_ZPX = 0x35,
        ROL_ZPX = 0x36,
        SEC_IMP = 0x38,
        AND_ABY = 0x39,
        AND_ABX = 0x3D,
        ROL_ABX = 0x3E,

        RTI_IMP = 0x40,
        EOR_IZX = 0x41,
        EOR_ZP0 = 0x45,
        LSR_ZP0 = 0x46,
        PHA_IMP = 0x48,
        EOR_IMM = 0x49,
        LSR_ACC = 0x4A,
        JMP_ABS = 0x4C,
        EOR_ABS = 0x4D,
        LSR_ABS = 0x4E,

        BVC_REL = 0x50,
        EOR_IZY = 0x51,
        EOR_ZPX = 0x55,
        LSR_ZPX = 0x56,
        CLI_IMP = 0x58,
        EOR_ABY = 0x59,
        EOR_ABX = 0x5D,
        LSR_ABX = 0x5E,

        RTS_IMP = 0x60,
        ADC_IZX = 0x61,
        ADC_ZP0 = 0x65,
        ROR_ZP0 = 0x66,
        PLA_IMP = 0x68,
        ADC_IMM = 0x69,
        ROR_ACC = 0x6A,
        JMP_IND = 0x6C,
        ADC_ABS = 0x6D,
        ROR_ABS = 0x6E,

        BVS_REL = 0x70,
        ADC_IZY = 0x71,
        ADC_ZPX = 0x75,
        ROR_ZPX = 0x76,
        SEI_IMP = 0x78,
        ADC_ABY = 0x79,
        ADC_ABX = 0x7D,
        ROR_ABX = 0x7E,

        STA_IZX = 0x81,
        STY_ZP0 = 0x84,
        STA_ZP0 = 0x85,
        STX_ZP0 = 0x86,
        DEY_IMP = 0x88,
        TXA_IMP = 0x8A,
        STY_ABS = 0x8C,
        STA_ABS = 0x8D,
        STX_ABS = 0x8E,

        BCC_REL = 0x90,
        STA_IZY = 0x91,
        STY_ZPX = 0x94,
        STA_ZPX = 0x95,
        STX_ZPY = 0x96,
        TYA_IMP = 0x98,
        STA_ABY = 0x99,
        TXS_IMP = 0x9A,
        STA_ABX = 0x9D,

        LDY_IMM = 0xA0,
        LDA_IZX = 0xA1,
        LDX_IMM = 0xA2,
        LDY_ZP0 = 0xA4,
        LDA_ZP0 = 0xA5,
        LDX_ZP0 = 0xA6,
        TAY_IMP = 0xA8,
        LDA_IMM = 0xA9,
        TAX_IMP = 0xAA,
        LDY_ABS = 0xAC,
        LDA_ABS = 0xAD,
        LDX_ABS = 0xAE,

        BCS_REL = 0xB0,
        LDA_IZY = 0xB1,
        LDY_ZPX = 0xB4,
        LDA_ZPX = 0xB5,
        LDX_ZPY = 0xB6,
        CLV_IMP = 0xB8,
        LDA_ABY = 0xB9,
        TSX_IMP = 0xBA,
        LDY_ABX = 0xBC,
        LDA_ABX = 0xBD,
        LDX_ABY = 0xBE,

        CPY_IMM = 0xC0,
        CMP_IZX = 0xC1,
        CPY_ZP0 = 0xC4,
        CMP_ZP0 = 0xC5,
        DEC_ZP0 = 0xC6,
        INY_IMP = 0xC8,
        CMP_IMM = 0xC9,
        DEX_IMP = 0xCA,
        CPY_ABS = 0xCC,
        CMP_ABS = 0xCD,
        DEC_ABS = 0xCE,

        BNE_REL = 0xD0,
        CMP_IZY = 0xD1,
        CMP_ZPX = 0xD5,
        DEC_ZPX = 0xD6,
        CLD_IMP = 0xD8,
        CMP_ABY = 0xD9,
        CMP_ABX = 0xDD,
        DEC_ABX = 0xDE,

        CPX_IMM = 0xE0,
        SBC_IZX = 0xE1,
        CPX_ZP0 = 0xE4,
        SBC_ZP0 = 0xE5,
        INC_ZP0 = 0xE6,
        INX_IMP = 0xE8,
        SBC_IMM = 0xE9,
        NOP_IMP = 0xEA,
        CPX_ABS = 0xEC,
        SBC_ABS = 0xED,
        INC_ABS = 0xEE,

        BEQ_REL = 0xF0,
        SBC_IZY = 0xF1,
        SBC_ZPX = 0xF5,
        INC_ZPX = 0xF6,
        SED_IMP = 0xF8,
        SBC_ABY = 0xF9,
        SBC_ABX = 0xFD,
        INC_ABX = 0xFE,
    }
}
pub use opcodes::*;

pub struct CPU6502 {
    a: u8,
    x: u8,
    y: u8,

    pc: u16,
    sp: u8,

    sr: StatusRegister,
}
impl CPU6502 {
    pub fn new() -> Self {
        let sr = StatusRegister::new();
        let a = 0;
        let x = 0;
        let y = 0;
        let sp = 0;
        let pc = 0;

        Self {
            sr,
            a,
            x,
            y,
            sp,
            pc,
        }
    }

    pub fn reset(&mut self, bus: &Bus) {
        self.a = 0;
        self.x = 0;
        self.y = 0;

        self.sr = StatusRegister::new();
        self.sp = 0xFF;
        self.pc = bus.read_u16(0xFFFC);
    }

    fn compare(&mut self, lhs: u8, rhs: u8) {
        let c = lhs >= rhs;
        let z = lhs == rhs;
        let n = !c;
        self.sr.set(Flags6502::C, c);
        self.sr.set(Flags6502::Z, z);
        self.sr.set(Flags6502::Z, n);
    }
    fn asl_mem(&mut self, addr: u16, bus: &mut Bus) {
        let num = bus.read_u8(addr);
        let num = self.asl(num);
        bus.write_u8(addr, num);
    }
    fn dec_mem(&mut self, addr: u16, bus: &mut Bus) {
        let num = bus.read_u8(addr);
        let num = self.dec(num);
        bus.write_u8(addr, num);
    }
    fn inc_mem(&mut self, addr: u16, bus: &mut Bus) {
        let num = bus.read_u8(addr);
        let num = self.inc(num);
        bus.write_u8(addr, num);
    }
    fn lsr_mem(&mut self, addr: u16, bus: &mut Bus) {
        let num = bus.read_u8(addr);
        let num = self.lsr(num);
        bus.write_u8(addr, num);
    }
    fn rol_mem(&mut self, addr: u16, bus: &mut Bus) {
        let num = bus.read_u8(addr);
        let num = self.rol(num);
        bus.write_u8(addr, num);
    }
    fn ror_mem(&mut self, addr: u16, bus: &mut Bus) {
        let num = bus.read_u8(addr);
        let num = self.ror(num);
        bus.write_u8(addr, num);
    }

    fn adc(&mut self, num: u8) {
        // TODO consider D flag

        let carry = if self.sr.get(Flags6502::C) { 1 } else { 0 };

        let (sum, c1) = self.a.overflowing_add(num);
        let (sum, c2) = sum.overflowing_add(carry);
        let c = c1 | c2;

        self.sr.set(
            Flags6502::V,
            (((!(self.a ^ num)) & (self.a ^ sum)) & 1 << 7) != 0,
        );
        self.sr.update(self.a);
        self.sr.set(Flags6502::C, c);
        self.a = sum;
    }
    fn and(&mut self, num: u8) {
        self.a &= num;
        self.sr.update(self.a);
    }
    fn asl(&mut self, num: u8) -> u8 {
        let c = (num & (1 << 7)) != 0;
        let num = num << 1;
        self.sr.update(num);
        self.sr.set(Flags6502::C, c);
        num
    }
    fn bcc(&mut self, offset: u8) {
        if !self.sr.get(Flags6502::C) {
            self.rel_branch(offset);
        }
    }
    fn bcs(&mut self, offset: u8) {
        if self.sr.get(Flags6502::C) {
            self.rel_branch(offset);
        }
    }
    fn beq(&mut self, offset: u8) {
        if self.sr.get(Flags6502::Z) {
            self.rel_branch(offset);
        }
    }
    fn bit(&mut self, num: u8) {
        self.sr.set(Flags6502::Z, (self.a & num) != 0);
        self.sr.set(Flags6502::N, (num & (1 << 7)) != 0);
        self.sr.set(Flags6502::V, (num & (1 << 6)) != 0);
    }
    fn bmi(&mut self, offset: u8) {
        if self.sr.get(Flags6502::N) {
            self.rel_branch(offset);
        }
    }
    fn bne(&mut self, offset: u8) {
        if !self.sr.get(Flags6502::Z) {
            self.rel_branch(offset);
        }
    }
    fn bpl(&mut self, offset: u8) {
        if !self.sr.get(Flags6502::N) {
            self.rel_branch(offset);
        }
    }
    // fn brk(&mut self) {
    //
    // }
    fn bvc(&mut self, offset: u8) {
        if !self.sr.get(Flags6502::V) {
            self.rel_branch(offset);
        }
    }
    fn bvs(&mut self, offset: u8) {
        if self.sr.get(Flags6502::V) {
            self.rel_branch(offset);
        }
    }
    fn clc(&mut self) {
        self.sr.set(Flags6502::C, false);
    }
    fn cld(&mut self) {
        self.sr.set(Flags6502::D, false);
    }
    fn cli(&mut self) {
        self.sr.set(Flags6502::I, false);
    }
    fn clv(&mut self) {
        self.sr.set(Flags6502::V, false);
    }
    fn cmp(&mut self, num: u8) {
        self.compare(self.a, num);
    }
    fn cpx(&mut self, num: u8) {
        self.compare(self.x, num);
    }
    fn cpy(&mut self, num: u8) {
        self.compare(self.y, num);
    }
    fn dec(&mut self, num: u8) -> u8 {
        let num = num - 1;
        self.sr.update(num);
        num
    }
    fn dex(&mut self) {
        self.x -= 1;
        self.sr.update(self.x);
    }
    fn dey(&mut self) {
        self.y -= 1;
        self.sr.update(self.y);
    }
    fn eor(&mut self, num: u8) {
        self.a ^= num;
        self.sr.update(self.a);
    }
    fn inc(&mut self, num: u8) -> u8 {
        let num = num + 1;
        self.sr.update(num);
        num
    }
    fn inx(&mut self) {
        self.x += 1;
        self.sr.update(self.x);
    }
    fn iny(&mut self) {
        self.y += 1;
        self.sr.update(self.y);
    }
    fn jmp(&mut self, addr: u16) {
        self.pc = addr;
    }
    fn jsr(&mut self, addr: u16, bus: &mut Bus) {
        self.stk_push_u16(self.pc, bus);
        self.pc = addr;
    }
    fn lda(&mut self, num: u8) {
        self.a = num;
        self.sr.update(num);
    }
    fn ldx(&mut self, num: u8) {
        self.x = num;
        self.sr.update(num);
    }
    fn ldy(&mut self, num: u8) {
        self.y = num;
        self.sr.update(num);
    }
    fn lsr(&mut self, num: u8) -> u8 {
        let c = (num & 1) != 0;
        let result = num >> 1;
        self.sr.set(Flags6502::N, false);
        self.sr.set(Flags6502::Z, result == 0);
        self.sr.set(Flags6502::C, c);
        result
    }
    fn nop(&self) {
        // nop
    }
    fn ora(&mut self, num: u8) {
        self.a |= num;
        self.sr.update(self.a);
    }
    fn pha(&mut self, bus: &mut Bus) {
        self.stk_push_u8(self.a, bus);
    }
    fn php(&mut self, bus: &mut Bus) {
        self.stk_push_u8(self.sr.into(), bus);
    }
    fn pla(&mut self, bus: &mut Bus) {
        self.a = self.stk_pull_u8(bus);
    }
    fn plp(&mut self, bus: &Bus) {
        self.sr = self.stk_pull_u8(bus).into();
    }
    fn rol(&mut self, num: u8) -> u8 {
        let c = (num & (1 << 7)) != 0;
        let carry = if self.sr.get(Flags6502::C) { 1 } else { 0 };
        let num = (num << 1) | carry;
        self.sr.set(Flags6502::C, c);
        self.sr.update(num);
        num
    }
    fn ror(&mut self, num: u8) -> u8 {
        let c = (num & 1) != 0;
        let carry = if self.sr.get(Flags6502::C) { 1 << 7 } else { 0 };
        let num = (num >> 1) | carry;
        self.sr.set(Flags6502::C, c);
        self.sr.update(num);
        num
    }
    // fn rti() {
    //
    // }
    fn rts(&mut self, bus: &Bus) {
        let addr = self.stk_pull_u16(bus);
        self.pc = addr;
    }
    fn sbc(&mut self, num: u8) {
        self.adc(!num);
    }
    fn sec(&mut self) {
        self.sr.set(Flags6502::C, true);
    }
    fn sed(&mut self) {
        self.sr.set(Flags6502::D, true);
    }
    fn sei(&mut self) {
        self.sr.set(Flags6502::I, true);
    }
    fn sta(&self, addr: u16, bus: &mut Bus) {
        bus.write_u8(addr, self.a);
    }
    fn stx(&self, addr: u16, bus: &mut Bus) {
        bus.write_u8(addr, self.x);
    }
    fn sty(&self, addr: u16, bus: &mut Bus) {
        bus.write_u8(addr, self.y);
    }
    fn tax(&mut self) {
        self.x = self.a;
        self.sr.update(self.x);
    }
    fn tay(&mut self) {
        self.y = self.a;
        self.sr.update(self.y);
    }
    fn tsx(&mut self) {
        self.x = self.sp;
        self.sr.update(self.x);
    }
    fn txa(&mut self) {
        self.a = self.x;
        self.sr.update(self.a);
    }
    fn txs(&mut self) {
        self.sp = self.x;
        self.sr.update(self.sp);
    }
    fn tya(&mut self) {
        self.a = self.y;
        self.sr.update(self.a);
    }

    pub fn execute(&mut self, bus: &mut Bus) {
        use Instruction::*;
        if let Some(op_code) = Instruction::from_u8(self.fetch_u8(bus)) {
            match op_code {
                BRK_IMP => {
                    panic!("BRK");
                }
                ORA_IZX => {
                    let num = self.izx_read_u8(bus);
                    self.ora(num);
                }
                ORA_ZP0 => {
                    let num = self.zp0_read_u8(bus);
                    self.ora(num);
                }
                ASL_ZP0 => {
                    let addr = self.zp0_addrmode(bus);
                    self.asl_mem(addr, bus);
                }
                PHP_IMP => {
                    self.php(bus);
                }
                ORA_IMM => {
                    let num = self.fetch_u8(bus);
                    self.ora(num);
                }
                ASL_ACC => {
                    self.a = self.asl(self.a);
                }
                ORA_ABS => {
                    let num = self.abs_read_u8(bus);
                    self.ora(num);
                }
                ASL_ABS => {
                    let addr = self.abs_addrmode(bus);
                    self.asl_mem(addr, bus);
                }
                BPL_REL => {
                    let offset = self.fetch_u8(bus);
                    self.bpl(offset);
                }
                ORA_IZY => {
                    let num = self.izy_read_u8(bus);
                    self.ora(num);
                }
                ORA_ZPX => {
                    let num = self.zpx_read_u8(bus);
                    self.ora(num);
                }
                ASL_ZPX => {
                    let addr = self.zpx_addrmode(bus);
                    self.asl_mem(addr, bus);
                }
                CLC_IMP => {
                    self.clc();
                }
                ORA_ABY => {
                    let num = self.aby_read_u8(bus);
                    self.ora(num);
                }
                ORA_ABX => {
                    let num = self.abx_read_u8(bus);
                    self.ora(num);
                }
                ASL_ABX => {
                    let addr = self.abx_addrmode(bus);
                    self.asl_mem(addr, bus);
                }
                JSR_ABS => {
                    let addr = self.abs_addrmode(bus);
                    self.jsr(addr, bus);
                }
                AND_IZX => {
                    let num = self.izx_read_u8(bus);
                    self.and(num);
                }
                BIT_ZP0 => {
                    let num = self.zp0_read_u8(bus);
                    self.bit(num);
                }
                AND_ZP0 => {
                    let num = self.zp0_read_u8(bus);
                    self.and(num);
                }
                ROL_ZP0 => {
                    let addr = self.zp0_addrmode(bus);
                    self.rol_mem(addr, bus);
                }
                PLP_IMP => {
                    self.plp(bus);
                }
                AND_IMM => {
                    let num = self.fetch_u8(bus);
                    self.and(num);
                }
                ROL_ACC => {
                    self.a = self.rol(self.a);
                }
                BIT_ABS => {
                    let num = self.abs_read_u8(bus);
                    self.bit(num);
                }
                AND_ABS => {
                    let num = self.abs_read_u8(bus);
                    self.and(num);
                }
                ROL_ABS => {
                    let addr = self.abs_addrmode(bus);
                    self.rol_mem(addr, bus);
                }
                BMI_REL => {
                    let offset = self.fetch_u8(bus);
                    self.bmi(offset);
                }
                AND_IZY => {
                    let num = self.izy_read_u8(bus);
                    self.and(num);
                }
                AND_ZPX => {
                    let num = self.zpx_read_u8(bus);
                    self.and(num);
                }
                ROL_ZPX => {
                    let addr = self.zpx_addrmode(bus);
                    self.rol_mem(addr, bus);
                }
                SEC_IMP => {
                    self.sec();
                }
                AND_ABY => {
                    let num = self.aby_read_u8(bus);
                    self.and(num);
                }
                AND_ABX => {
                    let num = self.abx_read_u8(bus);
                    self.and(num);
                }
                ROL_ABX => {
                    let addr = self.abx_addrmode(bus);
                    self.rol_mem(addr, bus);
                }
                RTI_IMP => {}
                EOR_IZX => {
                    let num = self.izx_read_u8(bus);
                    self.eor(num);
                }
                EOR_ZP0 => {
                    let num = self.zp0_read_u8(bus);
                    self.eor(num);
                }
                LSR_ZP0 => {
                    let addr = self.zp0_addrmode(bus);
                    self.lsr_mem(addr, bus);
                }
                PHA_IMP => {
                    self.pha(bus);
                }
                EOR_IMM => {
                    let num = self.fetch_u8(bus);
                    self.eor(num);
                }
                LSR_ACC => {
                    self.a = self.lsr(self.a);
                }
                JMP_ABS => {
                    let addr = self.abs_addrmode(bus);
                    self.jmp(addr);
                }
                EOR_ABS => {
                    let num = self.abs_read_u8(bus);
                    self.eor(num);
                }
                LSR_ABS => {
                    let addr = self.abs_addrmode(bus);
                    self.lsr_mem(addr, bus);
                }
                BVC_REL => {
                    let offset = self.fetch_u8(bus);
                    self.bvc(offset);
                }
                EOR_IZY => {
                    let num = self.izy_read_u8(bus);
                    self.eor(num);
                }
                EOR_ZPX => {
                    let num = self.zpx_read_u8(bus);
                    self.eor(num);
                }
                LSR_ZPX => {
                    let addr = self.zpx_addrmode(bus);
                    self.lsr_mem(addr, bus);
                }
                CLI_IMP => {
                    self.cli();
                }
                EOR_ABY => {
                    let num = self.aby_read_u8(bus);
                    self.eor(num);
                }
                EOR_ABX => {
                    let num = self.abx_read_u8(bus);
                    self.eor(num);
                }
                LSR_ABX => {
                    let addr = self.abx_addrmode(bus);
                    self.lsr_mem(addr, bus);
                }
                RTS_IMP => {
                    self.rts(bus);
                }
                ADC_IZX => {
                    let num = self.izx_read_u8(bus);
                    self.adc(num);
                }
                ADC_ZP0 => {
                    let num = self.zp0_read_u8(bus);
                    self.adc(num);
                }
                ROR_ZP0 => {
                    let addr = self.zp0_addrmode(bus);
                    self.ror_mem(addr, bus);
                }
                PLA_IMP => {
                    self.pla(bus);
                }
                ADC_IMM => {
                    let num = self.fetch_u8(bus);
                    self.adc(num);
                }
                ROR_ACC => {
                    self.a = self.ror(self.a);
                }
                JMP_IND => {
                    let addr = self.ind_addrmode(bus);
                    self.jmp(addr);
                }
                ADC_ABS => {
                    let num = self.abs_read_u8(bus);
                    self.adc(num);
                }
                ROR_ABS => {
                    let addr = self.abs_addrmode(bus);
                    self.ror_mem(addr, bus);
                }
                BVS_REL => {
                    let offset = self.fetch_u8(bus);
                    self.bvs(offset);
                }
                ADC_IZY => {
                    let num = self.izy_read_u8(bus);
                    self.adc(num);
                }
                ADC_ZPX => {
                    let num = self.zpx_read_u8(bus);
                    self.adc(num);
                }
                ROR_ZPX => {
                    let addr = self.zpx_addrmode(bus);
                    self.ror_mem(addr, bus);
                }
                SEI_IMP => {
                    self.sei();
                }
                ADC_ABY => {
                    let num = self.aby_read_u8(bus);
                    self.adc(num);
                }
                ADC_ABX => {
                    let num = self.abx_read_u8(bus);
                    self.adc(num);
                }
                ROR_ABX => {
                    let addr = self.abx_addrmode(bus);
                    self.ror_mem(addr, bus);
                }
                STA_IZX => {
                    let addr = self.izx_addrmode(bus);
                    self.sta(addr, bus);
                }
                STY_ZP0 => {
                    let addr = self.zp0_addrmode(bus);
                    self.sty(addr, bus);
                }
                STA_ZP0 => {
                    let addr = self.zp0_addrmode(bus);
                    self.sta(addr, bus);
                }
                STX_ZP0 => {
                    let addr = self.zp0_addrmode(bus);
                    self.stx(addr, bus);
                }
                DEY_IMP => {
                    self.dey();
                }
                TXA_IMP => {
                    self.txa();
                }
                STY_ABS => {
                    let addr = self.abs_addrmode(bus);
                    self.sty(addr, bus);
                }
                STA_ABS => {
                    let addr = self.abs_addrmode(bus);
                    self.sta(addr, bus);
                }
                STX_ABS => {
                    let addr = self.abs_addrmode(bus);
                    self.stx(addr, bus);
                }
                BCC_REL => {
                    let offset = self.fetch_u8(bus);
                    self.bcc(offset);
                }
                STA_IZY => {
                    let addr = self.izy_addrmode(bus);
                    self.sta(addr, bus);
                }
                STY_ZPX => {
                    let addr = self.zpx_addrmode(bus);
                    self.sty(addr, bus);
                }
                STA_ZPX => {
                    let addr = self.zpx_addrmode(bus);
                    self.sta(addr, bus);
                }
                STX_ZPY => {
                    let addr = self.zpy_addrmode(bus);
                    self.stx(addr, bus);
                }
                TYA_IMP => {
                    self.tya();
                }
                STA_ABY => {
                    let addr = self.aby_addrmode(bus);
                    self.sta(addr, bus);
                }
                TXS_IMP => {
                    self.txs();
                }
                STA_ABX => {
                    let addr = self.abx_addrmode(bus);
                    self.sta(addr, bus);
                }
                LDY_IMM => {
                    let num = self.fetch_u8(bus);
                    self.ldy(num);
                }
                LDA_IZX => {
                    let num = self.izx_read_u8(bus);
                    self.lda(num);
                }
                LDX_IMM => {
                    let num = self.fetch_u8(bus);
                    self.ldx(num);
                }
                LDY_ZP0 => {
                    let num = self.zp0_read_u8(bus);
                    self.ldy(num);
                }
                LDA_ZP0 => {
                    let num = self.zp0_read_u8(bus);
                    self.lda(num);
                }
                LDX_ZP0 => {
                    let num = self.zp0_read_u8(bus);
                    self.ldx(num);
                }
                TAY_IMP => {
                    self.tay();
                }
                LDA_IMM => {
                    let num = self.fetch_u8(bus);
                    self.lda(num);
                }
                TAX_IMP => {
                    self.tax();
                }
                LDY_ABS => {
                    let num = self.abs_read_u8(bus);
                    self.ldy(num);
                }
                LDA_ABS => {
                    let num = self.abs_read_u8(bus);
                    self.lda(num);
                }
                LDX_ABS => {
                    let num = self.abs_read_u8(bus);
                    self.ldx(num);
                }
                BCS_REL => {
                    let offset = self.fetch_u8(bus);
                    self.bcs(offset);
                }
                LDA_IZY => {
                    let num = self.izy_read_u8(bus);
                    self.lda(num);
                }
                LDY_ZPX => {
                    let num = self.zpx_read_u8(bus);
                    self.ldy(num);
                }
                LDA_ZPX => {
                    let num = self.zpx_read_u8(bus);
                    self.lda(num);
                }
                LDX_ZPY => {
                    let num = self.zpy_read_u8(bus);
                    self.ldx(num);
                }
                CLV_IMP => {
                    self.clv();
                }
                LDA_ABY => {
                    let num = self.aby_read_u8(bus);
                    self.lda(num);
                }
                TSX_IMP => {
                    self.tsx();
                }
                LDY_ABX => {
                    let num = self.abx_read_u8(bus);
                    self.ldy(num);
                }
                LDA_ABX => {
                    let num = self.abx_read_u8(bus);
                    self.lda(num);
                }
                LDX_ABY => {
                    let num = self.aby_read_u8(bus);
                    self.ldx(num);
                }
                CPY_IMM => {
                    let num = self.fetch_u8(bus);
                    self.cpy(num);
                }
                CMP_IZX => {
                    let num = self.izx_read_u8(bus);
                    self.cmp(num);
                }
                CPY_ZP0 => {
                    let num = self.zp0_read_u8(bus);
                    self.cpy(num);
                }
                CMP_ZP0 => {
                    let num = self.zp0_read_u8(bus);
                    self.cmp(num);
                }
                DEC_ZP0 => {
                    let addr = self.zp0_addrmode(bus);
                    self.dec_mem(addr, bus);
                }
                INY_IMP => {
                    self.iny();
                }
                CMP_IMM => {
                    let num = self.fetch_u8(bus);
                    self.cmp(num);
                }
                DEX_IMP => {
                    self.dex();
                }
                CPY_ABS => {
                    let num = self.abs_read_u8(bus);
                    self.cpy(num);
                }
                CMP_ABS => {
                    let num = self.abs_read_u8(bus);
                    self.cmp(num);
                }
                DEC_ABS => {
                    let addr = self.abs_addrmode(bus);
                    self.dec_mem(addr, bus);
                }
                BNE_REL => {
                    let offset = self.fetch_u8(bus);
                    self.bne(offset);
                }
                CMP_IZY => {
                    let num = self.izy_read_u8(bus);
                    self.cmp(num);
                }
                CMP_ZPX => {
                    let num = self.zpx_read_u8(bus);
                    self.cmp(num);
                }
                DEC_ZPX => {
                    let addr = self.zpx_addrmode(bus);
                    self.dec_mem(addr, bus);
                }
                CLD_IMP => {
                    self.cld();
                }
                CMP_ABY => {
                    let num = self.aby_read_u8(bus);
                    self.cmp(num);
                }
                CMP_ABX => {
                    let num = self.abx_read_u8(bus);
                    self.cmp(num);
                }
                DEC_ABX => {
                    let addr = self.abx_addrmode(bus);
                    self.dec_mem(addr, bus);
                }
                CPX_IMM => {
                    let num = self.fetch_u8(bus);
                    self.cpx(num);
                }
                SBC_IZX => {
                    let num = self.izx_read_u8(bus);
                    self.sbc(num);
                }
                CPX_ZP0 => {
                    let num = self.zp0_read_u8(bus);
                    self.cpx(num);
                }
                SBC_ZP0 => {
                    let num = self.zp0_read_u8(bus);
                    self.sbc(num);
                }
                INC_ZP0 => {
                    let addr = self.zp0_addrmode(bus);
                    self.inc_mem(addr, bus);
                }
                INX_IMP => {
                    self.inx();
                }
                SBC_IMM => {
                    let num = self.fetch_u8(bus);
                    self.sbc(num);
                }
                NOP_IMP => {
                    self.nop();
                }
                CPX_ABS => {
                    let num = self.abs_read_u8(bus);
                    self.cpx(num);
                }
                SBC_ABS => {
                    let num = self.abs_read_u8(bus);
                    self.sbc(num);
                }
                INC_ABS => {
                    let addr = self.abs_addrmode(bus);
                    self.inc_mem(addr, bus);
                }
                BEQ_REL => {
                    let offset = self.fetch_u8(bus);
                    self.beq(offset);
                }
                SBC_IZY => {
                    let num = self.izy_read_u8(bus);
                    self.sbc(num);
                }
                SBC_ZPX => {
                    let num = self.zpx_read_u8(bus);
                    self.sbc(num);
                }
                INC_ZPX => {
                    let addr = self.zpx_addrmode(bus);
                    self.inc_mem(addr, bus);
                }
                SED_IMP => {
                    self.sed();
                }
                SBC_ABY => {
                    let num = self.aby_read_u8(bus);
                    self.sbc(num);
                }
                SBC_ABX => {
                    let num = self.abx_read_u8(bus);
                    self.sbc(num);
                }
                INC_ABX => {
                    let addr = self.abx_addrmode(bus);
                    self.inc_mem(addr, bus);
                }
            }
        } else {
            panic!("BAD OPCODE {:#06X}", self.pc);
        }
    }

    fn fetch_u8(&mut self, bus: &Bus) -> u8 {
        let data = bus.read_u8(self.pc);
        self.pc += 1;
        data
    }
    fn fetch_u16(&mut self, bus: &Bus) -> u16 {
        let data = bus.read_u16(self.pc);
        self.pc += 2;
        data
    }

    fn abs_addrmode(&mut self, bus: &Bus) -> u16 {
        self.fetch_u16(bus)
    }
    fn abx_addrmode(&mut self, bus: &Bus) -> u16 {
        self.fetch_u16(bus) + self.x as u16
    }
    fn aby_addrmode(&mut self, bus: &Bus) -> u16 {
        self.fetch_u16(bus) + self.y as u16
    }
    fn zp0_addrmode(&mut self, bus: &Bus) -> u16 {
        u16::from_le_bytes([self.fetch_u8(bus), 0x00])
    }
    fn zpx_addrmode(&mut self, bus: &Bus) -> u16 {
        u16::from_le_bytes([self.fetch_u8(bus) + self.x, 0x00])
    }
    fn zpy_addrmode(&mut self, bus: &Bus) -> u16 {
        u16::from_le_bytes([self.fetch_u8(bus) + self.y, 0x00])
    }
    fn izx_addrmode(&mut self, bus: &Bus) -> u16 {
        let ptr = self.zpx_addrmode(bus);
        bus.read_u16(ptr)
    }
    fn izy_addrmode(&mut self, bus: &Bus) -> u16 {
        let ptr = self.zp0_addrmode(bus) + self.y as u16;
        bus.read_u16(ptr)
    }
    fn ind_addrmode(&mut self, bus: &Bus) -> u16 {
        // TODO: impl page change bug
        let ptr = self.fetch_u16(bus);
        bus.read_u16(ptr)
    }
    fn stk_addrmode(&self) -> u16 {
        u16::from_le_bytes([self.sp, 0x01])
    }

    fn abs_read_u8(&mut self, bus: &Bus) -> u8 {
        let addr = self.abs_addrmode(bus);
        bus.read_u8(addr)
    }
    fn abx_read_u8(&mut self, bus: &Bus) -> u8 {
        let addr = self.abx_addrmode(bus);
        bus.read_u8(addr)
    }
    fn aby_read_u8(&mut self, bus: &Bus) -> u8 {
        let addr = self.aby_addrmode(bus);
        bus.read_u8(addr)
    }
    fn zp0_read_u8(&mut self, bus: &Bus) -> u8 {
        let addr = self.zp0_addrmode(bus);
        bus.read_u8(addr)
    }
    fn zpx_read_u8(&mut self, bus: &Bus) -> u8 {
        let addr = self.zpx_addrmode(bus);
        bus.read_u8(addr)
    }
    fn zpy_read_u8(&mut self, bus: &Bus) -> u8 {
        let addr = self.zpy_addrmode(bus);
        bus.read_u8(addr)
    }
    fn izx_read_u8(&mut self, bus: &Bus) -> u8 {
        let addr = self.izx_addrmode(bus);
        bus.read_u8(addr)
    }
    fn izy_read_u8(&mut self, bus: &Bus) -> u8 {
        let addr = self.izy_addrmode(bus);
        bus.read_u8(addr)
    }

    fn stk_push_u8(&mut self, data: u8, bus: &mut Bus) {
        bus.write_u8(self.stk_addrmode(), data);
        self.sp -= 1;
    }
    fn stk_push_u16(&mut self, data: u16, bus: &mut Bus) {
        bus.write_u16(self.stk_addrmode(), data);
        self.sp -= 2;
    }
    fn stk_pull_u8(&mut self, bus: &Bus) -> u8 {
        self.sp += 1;
        bus.read_u8(self.stk_addrmode())
    }
    fn stk_pull_u16(&mut self, bus: &Bus) -> u16 {
        self.sp += 2;
        bus.read_u16(self.stk_addrmode())
    }

    fn rel_branch(&mut self, offset: u8) {
        self.pc = self.pc.wrapping_add_signed(offset as i8 as i16);
    }
}

impl Default for CPU6502 {
    fn default() -> Self {
        Self::new()
    }
}
impl std::fmt::Display for CPU6502 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Program Counter:  {:#06X}", self.pc)?;
        writeln!(
            f,
            "Accumlator:       u:{} s:{} {:#X}",
            self.a, self.a as i8, self.a
        )?;
        writeln!(
            f,
            "Register X:       u:{} s:{} {:#X}",
            self.x, self.x as i8, self.x
        )?;
        writeln!(
            f,
            "Register Y:       u:{} s:{} {:#X}",
            self.y, self.y as i8, self.y
        )?;
        writeln!(f, "Stack Ptr:        {:#04X}", self.sp)?;
        writeln!(f, "                  NV-BDIZC")?;
        writeln!(f, "Process Status:   {:08b}", self.sr.status)
    }
}

pub struct Bus {
    pub ram: [u8; 64 * 1024],
}
impl Bus {
    pub fn new() -> Self {
        let ram = [0x00; 64 * 1024];
        Self { ram }
    }
    fn write_u8(&mut self, addr: u16, data: u8) {
        if addr == 0x5000 {
            let ptr = data as usize;

            // let data: [u8; 8] = (&self.ram[ptr..(ptr + 8)]).try_into().unwrap();
            // let idata = i64::from_le_bytes(data);
            // let udata = u64::from_le_bytes(data);
            // println!("u64: {udata} i64: {idata}");

            let data: [u8; 4] = (&self.ram[ptr..(ptr + 4)]).try_into().unwrap();
            let udata = u32::from_le_bytes(data);
            println!("{udata}");
        }

        self.ram[addr as usize] = data;
    }
    fn write_u16(&mut self, addr: u16, data: u16) {
        let addr = addr as usize;
        let data = data.to_le_bytes();
        self.ram[addr] = data[0];
        self.ram[addr + 1] = data[1];
    }
    fn read_u8(&self, addr: u16) -> u8 {
        self.ram[addr as usize]
    }
    fn read_u16(&self, addr: u16) -> u16 {
        let addr = addr as usize;
        u16::from_le_bytes([self.ram[addr], self.ram[addr + 1]])
    }
}
impl Default for Bus {
    fn default() -> Self {
        Self::new()
    }
}
