use emulator6502::*;

fn main() {
    let file = std::env::args().nth(1).expect("No file given");
    let path = std::path::Path::new(&file);
    let code = std::fs::read(path).expect("Could not read file");

    let mut bus = Bus::new();
    bus.ram.copy_from_slice(&code[..]);

    let mut cpu = CPU6502::new();
    cpu.reset(&bus);

    loop {
        // println!("{cpu}");
        cpu.execute(&mut bus);
        std::thread::sleep_ms(1);
    }
}
