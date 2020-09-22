use std::io; 
use std::io::prelude::*;
use std::fs::File;

pub struct Ram {
    buffer : [ u8 ; 0x1_0000 ]
}

pub fn ram() -> Ram { 
    Ram {
        buffer : [0 ; 0x1_0000 ]
    }
}

impl Ram { 
    pub fn load_file(&mut self, path: &str) -> io::Result<usize> {
        let f = File::open(path)?;
        let mut ii : usize = 0;
        for byte in f.bytes() {
            self.buffer[0xC000 + ii] = byte.unwrap(); 
            //  print!("{:02x}", self.buffer[0xC000 + ii]);
            ii = ii + 1;      
        };
        Ok(ii)
    } 

    pub fn read_byte(&self, addr: u16) -> u8 {
        let addr = addr as usize;
        let val = self.buffer[addr];
        //println!("Read Byte {:02x} @ {:04x}", val, addr);
        return val;
    }
    
    pub fn read_word(&self, addr: u16) -> u16 {
        let addr = addr as usize;
        let val = (self.buffer[addr+1] as u16) << 8 
        | (self.buffer[(addr)] as u16) ; 
        //println!("Read Word {:04x} @ {:04x}", val, addr);
        return val;
    }

    pub fn write_byte(&mut self, addr: u16, value : u8) {
        let addr = addr as usize;
        self.buffer[addr] = value;
    }

    pub fn write_word(&mut self, addr: u16, value : u16) {
        let addr = addr as usize;
        self.buffer[addr+1]   = (value >> 8 & 0xFF) as u8;
        self.buffer[addr] = (value & 0xFF) as u8;
    }
}