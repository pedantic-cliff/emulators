mod instr;
mod cpu;
mod core;
mod mem;
mod procvalue;

fn main() {
    let mut cpu = cpu::cpu("./test/test.bin");

    println!("{}", cpu);
    while cpu.get_pc() != procvalue::ProcValue::from(0xC078u16) {
        core::step(&mut cpu);
        println!("{}", cpu);
    }
    println!("{}", cpu);
    
}
