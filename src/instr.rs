use std::fmt;

use crate::procvalue::ProcValue;

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum SizeMode {
    Word,
    Byte,
}
impl fmt::Display for SizeMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SizeMode::Word => write!(f, ".W"),
            SizeMode::Byte => write!(f, ".B")
        }
    }
}

// Page 48
#[derive(Copy,Clone,Debug,PartialEq)]
pub enum Op {
    Reg(u8),                        // Just a Reg index
    Index(Option<u8>, ProcValue),   // Optional Reg index, Const Offset X
    Indirect(u8),                   // Reg index defines address
    IndirectAutoIncr(u8),           // Reg index defines address, increment based on size
    Constant(ProcValue),            // Used by Constant generator type ops
    Immediate(ProcValue),           // Used for immediate ops
    PcOffset(ProcValue, ProcValue)  // Addr, Offset
}
impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Reg(idx) => match idx {
                0 => write!(f, "PC" ),
                1 => write!(f, "SP" ),
                2 => write!(f, "CG" ),
                3 => write!(f, "SR" ),
                _ => write!(f, "R{}", idx)
            },

            // ALl Constants
            Op::Constant(val) => write!(f, "{:X}h", val),
            
            // Jump
            Op::PcOffset(addr, offset) => {
                    write!(f, "{:X}h", *addr + (*offset * 2) + 2)
            },

            // Symbolic X(PC)
            // Indirect X(Rn)
            Op::Index(Some(r), offset) => {
                if (*offset & 0x8000) != ProcValue::from(0u16) {
                    write!(f, "-{:X}h({})", -*offset, Op::Reg(*r))
                } else {
                    write!(f, "{:X}h({})", offset, Op::Reg(*r))
                }
            },
            // Absolute &Addr
            Op::Index(None, offset)    => write!(f, "&{:X}h", offset),

            // Indirect
            Op::Indirect(r) => write!(f, "@{}", Op::Reg(*r)),

            Op::IndirectAutoIncr(r) => write!(f, "@{}+", Op::Reg(*r)),

            // Immediate
            Op::Immediate(imm) => write!(f, "#{:X}h", imm)
        }
    } 
}

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum Condition {
    Eq, Ne,
    C, Nc,
    N,
    Ge, L,
    A,
}
impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self { 
            Condition::Eq =>  write!(f, "EQ"), 
            Condition::Ne =>  write!(f, "NE"), 
            Condition::C  =>  write!(f, "C"),               
            Condition::Nc =>  write!(f, "NC"),
            Condition::N  =>  write!(f, "N"),                
            Condition::Ge =>  write!(f, "GE"),
            Condition::L  =>  write!(f, "L"),                
            Condition::A  =>  write!(f, "A"),
        }
    }
}


#[derive(Copy,Clone,Debug,PartialEq)]
pub enum Mnem {
    Mov( Op, Op, SizeMode),
    Add( Op, Op, SizeMode),
    Addc(Op, Op, SizeMode),
    Sub( Op, Op, SizeMode),
    Subc(Op, Op, SizeMode),
    Cmp( Op, Op, SizeMode),
    Dadd(Op, Op, SizeMode),
    Bit( Op, Op, SizeMode),
    Bic( Op, Op, SizeMode),
    Bis( Op, Op, SizeMode),
    Xor( Op, Op, SizeMode),
    And( Op, Op, SizeMode),

    Jmp( Op, Condition),
    
    Rrc( Op, SizeMode),
    Raa( Op, SizeMode),
    Push(Op, SizeMode),

    Sxt( Op),
    Swpb(Op),
    Call(Op),
    Reti(),

    Unimpl(),
}

impl fmt::Display for Mnem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Mnem::Mov(src,dst,size)  =>  write!(f, "MOV{}\t{}, {}",  size, src, dst),
            Mnem::Add(src,dst,size)  =>  write!(f, "ADD{}\t{}, {}",  size, src, dst),
            Mnem::Addc(src,dst,size) =>  write!(f, "ADDC{}\t{}, {}", size, src, dst),
            Mnem::Sub(src,dst,size)  =>  write!(f, "SUB{}\t{}, {}",  size, src, dst),
            Mnem::Subc(src,dst,size) =>  write!(f, "SUBC{}\t{}, {}", size, src, dst),
            Mnem::Dadd(src,dst,size) =>  write!(f, "DADD{}\t{}, {}", size, src, dst),
            Mnem::Bit(src,dst,size)  =>  write!(f, "BIT{}\t{}, {}",  size, src, dst),
            Mnem::Bis(src,dst,size)  =>  write!(f, "BIS{}\t{}, {}",  size, src, dst),
            Mnem::Bic(src,dst,size)  =>  write!(f, "BIS{}\t{}, {}",  size, src, dst),
            Mnem::Xor(src,dst,size)  =>  write!(f, "XOR{}\t{}, {}",  size, src, dst),
            Mnem::And(src,dst,size)  =>  write!(f, "AND{}\t{}, {}",  size, src, dst),
            Mnem::Cmp(src,dst,size)  =>  write!(f, "CMP{}\t{}, {}",  size, src, dst),

            Mnem::Jmp(offset, cond)  => match cond {
                Condition::A =>  write!(f, "JMP\t{}", offset),
                _            =>  write!(f, "J{}\t{}", cond, offset),           
            },

            Mnem::Rrc(dst,size)     => write!(f, "RCC{}\t{}", size, dst),
            Mnem::Raa(dst,size)     => write!(f, "RRA{}\t{}", size, dst),
            Mnem::Push(dst,size)    => write!(f, "PUSH{}\t{}", size, dst),
            
            Mnem::Swpb(dst)         => write!(f, "SWPB\t{}", dst),
            Mnem::Call(dst)         => write!(f, "CALL\t{}", dst),
            Mnem::Sxt(dst)          => write!(f, "SXT\t{}", dst),
            Mnem::Reti()            => write!(f, "RETI"),

            Mnem::Unimpl() => write!(f, "UNIMPL")       
        }
    }
}



#[derive(Copy,Clone,Debug)]
pub struct Instr {
    pub mnem : Mnem,
    pub bytes: u16,
    pub addr : ProcValue,
    pub size : u16,
}

pub fn instr(mnem : Mnem, bytes : u16, addr : ProcValue, size:u16) -> Instr 
{
    Instr{ 
        mnem : mnem,
        bytes: bytes,
        addr : addr,
        size : size,
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        { 
            write!(f, "{:04X}:\t{}", self.addr, self.mnem)
        }
    }
}
