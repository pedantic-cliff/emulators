use std::io;
use std::fmt;

use crate::procvalue::ProcValue;
use crate::instr::*; 
use crate::mem::*;


pub struct Cpu {
    rs      : [ ProcValue ; 16],
    ram     : Ram,
    status  : u16,
    cycles  : usize,   
}

pub fn cpu(path: &str) -> Cpu {
    let mut cpu : Cpu = Cpu {
        rs      : [ ProcValue::from(0u16); 16 ],
        ram     : ram(),
        status  : 0x0,
        cycles  : 0,
    };
    cpu.load_file(path).unwrap();
    cpu.rs[0] = ProcValue::from(cpu.read_memory(0xFFFE, SizeMode::Word));
    return cpu
}

impl fmt::Display for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"CPU :").unwrap();
        
        write!(f, "\tPC :\t{:04X}", self.get_reg_value(0).value).unwrap();
        write!(f, "\tSP :\t{:04X}", self.get_reg_value(1).value).unwrap();
        
        writeln!(f, "\tFlags: {} {} {} {}", 
            if self.c_get() {"C"} else {"c"},
            if self.z_get() {"Z"} else {"z"},
            if self.n_get() {"N"} else {"n"},
            if self.v_get() {"V"} else {"v"}
        ).unwrap();

        for r in 4..16 {
            if r < 10 {
                write!(f, "\t{} :\t{:04X}", Op::Reg(r), 
                        self.get_reg_value(r).value).unwrap();
            } else {
                write!(f, "\t{}:\t{:04X}", Op::Reg(r), 
                        self.get_reg_value(r).value).unwrap();
            }
            if r % 4 == 3 {
                writeln!(f, "").unwrap();
            }
        }
        write!(f,"")
    }
}

/*
struct coreError;

impl fmt::Display for coreError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "coreError: Illegal Request")
    }
}
impl fmt::Debug for coreError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "coreError: Illegal Request {{ file: {}, line {} }}", file!(), line!())
    }
}
*/
impl Cpu { 
    pub fn read_memory(&self, addr : u16, size: SizeMode) -> u16 {
        match size { 
            SizeMode::Word => self.ram.read_word(addr),
            SizeMode::Byte => self.ram.read_byte(addr) as u16
        }
    }

    pub fn write_memory(&mut self, addr : u16, val : u16, size: SizeMode) -> () {
        match size {
            SizeMode::Word => &mut self.ram.write_word(addr, val),
            SizeMode::Byte => &mut self.ram.write_byte(addr, val as u8),
        };
    }

    pub fn get_reg_value(&self, idx : u8) -> ProcValue {
        let idx = (idx & 0xF) as usize;
        self.rs[idx]
    }
    
    pub fn set_reg_value(&mut self, idx : u8, val : ProcValue) -> () {
        let idx = (idx & 0xF) as usize;
        self.rs[idx] = val;
    }

    pub fn get_pc(&self) -> ProcValue {
        ProcValue::from(self.get_reg_value(0))
    }

    pub fn step(&mut self, instr : Instr) -> () {
        self.rs[0] = self.get_pc() + instr.size;
        self.cycles += 1;
    }

    pub fn jmp(&mut self, offset : ProcValue) -> (){
        self.rs[0] = self.get_pc() + offset * 2 + 2;
        self.cycles += 2;
    }

    pub fn load_file(&mut self, path: &str) -> io::Result<usize>  {
        self.ram.load_file(path)
    }

    pub fn c_get(&self) -> bool {
        self.status & 0x1 != 0
    }

    pub fn z_get(&self) -> bool {
        self.status & 0x2 != 0
    }

    pub fn n_get(&self) -> bool {
        self.status & 0x4 != 0
    }

    pub fn v_get(&self) -> bool {
        self.status & 0x100 != 0
    }

    pub fn c_set(&mut self, val:bool ) {
        match val {
            true  => self.status = self.status | 0x0001,
            false => self.status = self.status & 0xFFFE,
        }
    }
    pub fn z_set(&mut self, val:bool ) {
        match val {
            true  => self.status = self.status | 0x0002,
            false => self.status = self.status & 0xFFFD,
        }
    }

    pub fn n_set(&mut self, val:bool ) {
        match val {
            true  => self.status = self.status | 0x0004,
            false => self.status = self.status & 0xFFFB,
        }
    }

    pub fn v_set(&mut self, val:bool ) {
        match val {
            true  => self.status = self.status | 0x0100,
            false => self.status = self.status & 0xFEFF,
        }
    }

    pub fn set_flags(&mut self, carry: bool, zero: bool, negative: bool, overflow: bool) {
        self.v_set(overflow);
        self.z_set(zero);
        self.n_set(negative);
        self.c_set(carry)
    }
}