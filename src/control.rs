use crate::core::*;
use crate::cpu::Cpu;
use crate::procvalue::ProcValue;
use crate::instr::*;

use scan_fmt;

use std::*;
use std::io;
use std::io::Write;

pub struct Control {
    bps: Vec<ProcValue>
}

pub fn control() -> Control{
    Control {
        bps: Vec::new()
    }
}

impl Control {

    pub fn control_loop(&mut self, cpu: &mut Cpu) {
        let mut running = true;
        let mut cmd = ' ';
        while running { 
            print!("> "); io::stdout().flush().unwrap();
            let line = input();

            if line.len() > 0 {
                cmd = line.chars().next().unwrap();
            }

            match cmd {
                'q' => running = false,
                's' => self.step(cpu),
                'n' => self.next(cpu),
                'x' => self.print_mem(cpu, &line),
                'i' => self.print_info(cpu, &line),
                'c' => self.cont(cpu),
                'b' => self.breakpoint(&line),
                _ =>  println!("Unhandled Command")

            };
        }
        println!("Goodbye");
    }

    fn step(&self, cpu: &mut Cpu) {
        step(cpu);
        disassemble(cpu, cpu.get_pc() );
    }

    fn next(&self, cpu: &mut Cpu) {
        let addr =  cpu.get_pc();
        let bytes = cpu.read_memory(addr.value, SizeMode::Word);
        let instr = decode(cpu, bytes, addr);
        match instr.mnem {
            Mnem::Call(_) => {
                let next_addr = addr + instr.size;
                while cpu.get_pc() != next_addr {
                    step(cpu);

                    let addr = cpu.get_pc();
                    if self.bps.contains(&addr) {
                        println!("Breakpoint");
                        break;
                    }
                }
            },
            _ =>   step(cpu)
        };

        disassemble(cpu, cpu.get_pc());
    }
    
    fn cont(&self, cpu: &mut Cpu) {

        loop {
            
            step(cpu);
            
            let addr = cpu.get_pc(); 
            if self.bps.iter().any(|&a| a == addr) {
                println!("Break");
                disassemble(cpu, cpu.get_pc());
                return;
            }
        }   
    }

    fn breakpoint(&mut self, s: &String) {
        match scan_fmt!(s, "b {x}", [hex u16]) {
            Ok(addr) => {
                self.bps.push(ProcValue::from(addr))
            }
            _ => println!("Invalid Address")
        }
    }

    fn print_mem(&self, cpu : &Cpu, s: &String) {
        match scan_fmt!(s, "x {x}", [hex u16]) {
            Ok(addr) => {
                let value = cpu.read_memory(addr, SizeMode::Word);
                println!("{:04X} : {:04X}", addr,value);
            }
            _ => println!("Invalid Address")
        }
    }

    fn print_info(&self, cpu : &Cpu, s: &String) {
        match scan_fmt!(s, "i {}", char) {
            Ok('r') => println!("{}", cpu),
            Ok('b') => {
                for bp in self.bps.iter() {
                    println!("{:04X}", bp);
                }
            },
            Ok(v) => println!("{}", v),
            _ => println!("Invalid Sub Command")
        }
    }

}
fn input() -> String { 
    let mut ret = String::with_capacity(32);
    io::stdin().read_line(&mut ret).expect("error reading");
    while ret.ends_with('\n') || ret.ends_with('\n') {
        ret.pop();
    }
    ret
}