use emulator6502::*;
use Instruction::*;

fn main() {
    let mut bus = Bus::new();
    // bus.ram = &std::fs::read("test.bin").unwrap()[..];
    bus.ram
        .copy_from_slice(&std::fs::read("fibinachi.bin").unwrap()[..]);

    // bus.ram[0x00] = 0xEA;
    // bus.ram[0x01] = 0xEA;
    // bus.ram[0x02] = 0x20;
    // bus.ram[0x03] = 0x00;
    // bus.ram[0x04] = 0x80;
    // bus.ram[0x8000] = 0xA9;
    // bus.ram[0x8001] = 69;
    // bus.ram[0x8002] = 0x60;
    // bus.ram[0x05] = 0xA2;
    // bus.ram[0x06] = -42i8 as u8;

    // bus.ram[0] = 0xA9;
    // bus.ram[1] = 10; // lda #255
    // bus.ram[2] = 0x69;
    // bus.ram[3] = 5; // adc #255
    // bus.ram[4] = 0xA9;
    // bus.ram[5] = 7; // lda #255
    // bus.ram[6] = 0x69;
    // bus.ram[7] = 1; // adc #255
    // bus.ram[8] = 0xFF;

    // bus.ram[0] = 0xA9;
    // bus.ram[1] = 1; // lda #1
    // bus.ram[2] = 0x0A; // asl a
    // bus.ram[3] = 0x0A; // asl a
    // bus.ram[4] = 0x0A; // asl a
    // bus.ram[5] = 0x0A; // asl a
    // bus.ram[6] = 0x0A; // asl a
    // bus.ram[7] = 0x0A; // asl a
    // bus.ram[8] = 0x0A; // asl a
    // bus.ram[9] = 0x0A; // asl a
    // bus.ram[10] = 0x0A; // asl a
    // bus.ram[11] = 0xFF;

    // bus.ram[0] = 0xA9;
    // bus.ram[1] = 42;

    // bus.ram[2] = 0x90;
    // bus.ram[3] = -4i8 as u8;

    // bus.ram[4] = 0xA2;
    // bus.ram[5] = 69;

    // bus.ram[6] = 0xA0;
    // bus.ram[7] = 42;

    // bus.ram[0] = LDA_IMM as u8;
    // bus.ram[1] = 10;
    // bus.ram[2] = ADC_IMM as u8;
    // bus.ram[3] = 1;
    // bus.ram[4] = SBC_IMM as u8;
    // bus.ram[5] = 1;
    // bus.ram[6] = SEC_IMP as u8;
    // bus.ram[7] = SBC_IMM as u8;
    // bus.ram[8] = 2;
    // bus.ram[9] = 0xFF;

    // bus.ram[0] = LDA_IMM as u8;
    // bus.ram[1] = 69;
    // bus.ram[2] = STA_ABS as u8;
    // bus.ram[3] = 0x00;
    // bus.ram[4] = 0x50;
    // bus.ram[5] = 0xFF;

    // let i = 0x30;
    // let j = 0x31;
    // let k = 0x32;

    // bus.ram[0] = LDA_ZP0 as u8;
    // bus.ram[1] = i;
    // bus.ram[2] = ADC_ZP0 as u8;
    // bus.ram[3] = j;
    // bus.ram[4] = STA_ZP0 as u8;
    // bus.ram[5] = k;
    // bus.ram[6] = LDA_ZP0 as u8;
    // bus.ram[7] = j;
    // bus.ram[8] = STA_ZP0 as u8;
    // bus.ram[9] = i;
    // bus.ram[10] = LDA_ZP0 as u8;
    // bus.ram[11] = k;
    // bus.ram[12] = STA_ZP0 as u8;
    // bus.ram[13] = j;

    // bus.ram[14] = BCS_REL as u8;
    // bus.ram[15] = 21 - 15;

    // bus.ram[16] = STA_ABS as u8;
    // bus.ram[17] = 0x00;
    // bus.ram[18] = 0x50;

    // bus.ram[19] = JMP_ABS as u8;
    // bus.ram[20] = 0;
    // bus.ram[21] = 0;
    // bus.ram[22] = 0xFF;

    // bus.ram[i as usize] = 0;
    // bus.ram[j as usize] = 1;

    let mut cpu = CPU6502::new();
    // bus.ram[0xFFFC] = 0;
    // bus.ram[0xFFFD] = 0;
    cpu.reset(&bus);

    // panic!("{:?}", 0b0100_0000u8.ro(8));
    loop {
        // println!("{cpu}");
        cpu.execute(&mut bus);
        // std::thread::sleep_ms(100);
    }
}
