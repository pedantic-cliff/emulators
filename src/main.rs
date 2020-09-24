mod instr;
mod cpu;
mod core;
mod mem;
mod procvalue;
mod control;
#[macro_use] extern crate scan_fmt;


fn main() {
    let mut cpu = cpu::cpu("./test/test.bin");
    let mut control = control::control();
    control.control_loop(&mut cpu);
    /*
    println!("{}", cpu);
    for _ in 1..18 {
        core::step(&mut cpu);
        println!("{}", cpu);
    }
    */
    
    return; 
    
}
