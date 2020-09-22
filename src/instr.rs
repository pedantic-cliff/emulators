use std::fmt;

use std::ops;
use std::convert::From;
use std::convert::Into;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ProcValue {
    pub value       : u16,
    pub size        : SizeMode,
}

impl From<u16> for ProcValue { 
    fn from(w : u16) -> Self {
        ProcValue { 
            value   : w,
            size    : SizeMode::Word,
        }
    }
}
impl From<u8> for ProcValue { 
    fn from(w : u8) -> Self {
        ProcValue { 
            value   : w as u16,
            size    : SizeMode::Byte,
        }
    }
}

impl Into<u16> for ProcValue { 
    fn into(self) -> u16 {
        self.value
    }
}

impl ops::Add<ProcValue> for ProcValue { 
    type Output = ProcValue;
    fn add(self, other : ProcValue) -> ProcValue {
        let (res,_) = other.value.overflowing_add(self.value);
        ProcValue { 
            value   : res,
            size    : self.size
        }
    }
}
impl ops::Add<u16> for ProcValue { 
    type Output = ProcValue;
    fn add(self, other : u16) -> ProcValue {
        let (val,_) = other.overflowing_add(self.value);
        ProcValue::from(val)
    }
}

impl ops::BitOr<ProcValue> for ProcValue { 
    type Output = ProcValue;
    fn bitor(self, other : ProcValue) -> ProcValue {
        ProcValue::from(self.value | other.value)
    }
}
impl ops::BitOr<u16> for ProcValue { 
    type Output = ProcValue;
    fn bitor(self, other : u16) -> ProcValue {
        ProcValue::from(self.value | other)
    }
}
impl ops::BitXor<ProcValue> for ProcValue { 
    type Output = ProcValue;
    fn bitxor(self, other : ProcValue) -> ProcValue {
        ProcValue::from(self.value ^ other.value)
    }
}
impl ops::BitXor<u16> for ProcValue { 
    type Output = ProcValue;
    fn bitxor(self, other : u16) -> ProcValue {
        ProcValue::from(self.value ^ other)
    }
}
impl ops::BitAnd<ProcValue> for ProcValue { 
    type Output = ProcValue;
    fn bitand(self, other : ProcValue) -> ProcValue {
        ProcValue::from(self.value & other.value)
    }
}
impl ops::BitAnd<u16> for ProcValue { 
    type Output = ProcValue;
    fn bitand(self, other : u16) -> ProcValue {
        ProcValue::from(self.value & other)
    }
}

impl ops::Mul<ProcValue> for ProcValue { 
    type Output = ProcValue;
    fn mul(self, other : ProcValue) -> ProcValue { 
        let (res, _) = other.value.overflowing_mul(self.value);
        ProcValue {
            value   : res,
            size    : self.size,
        }
    }
}
impl ops::Mul<u16> for ProcValue { 
    type Output = ProcValue;
    fn mul(self, other : u16) -> ProcValue { 
        let (res, _) = other.overflowing_mul(self.value);
        ProcValue {
            value   : res,
            size    : self.size,
        }
    }
}

impl ops::Neg for ProcValue { 
    type Output = ProcValue;
    fn neg(self) -> ProcValue {
        let val = self.value ^ 0xFFFF;
        let (val, _) = val.overflowing_add(1);
        ProcValue { 
            value   : val,
            size    : self.size,
        }
    }   
}

impl fmt::Display for ProcValue { 
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl fmt::UpperHex for ProcValue { 
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:04X}", self.value)
    }
}

impl ProcValue { 
    pub fn is_neg(&self) -> bool{
        self.value & 0x8000u16 != 0
    }
}


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
    Reg(u8),                 // Just a Reg index
    Index(Option<u8>, ProcValue),  // Optional Reg index, Const Offset X
    Indirect(u8, ProcValue),       // Reg index defines address, increment in {0,1,2}
    Constant(ProcValue),           // Used by Constant generator type ops
    Immediate(ProcValue),          // Used for immediate ops
    PcOffset(ProcValue, ProcValue)       // Addr, Offset
}
impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Reg(idx) => match idx {
                0 => write!(f, "PC" ),
                1 => write!(f, "SP" ),
                2 => write!(f, "CG" ),
                _ => write!(f, "R{}", idx)
            },

            // ALl Constants
            Op::Constant(val) => write!(f, "{:X}h", val),
            
            // Jump
            Op::PcOffset(addr, offset) => {
                if offset.value > 0x7FFFu16 {
                    write!(f, "{:04X}h\t(-{:X}h)", 
                        *addr + (*offset * 2) + 2, -*(offset))
                } else 
                {
                    write!(f, "{:04X}h\t({:X}h)", 
                        *addr + (*offset * 2) + 2, offset)
                }
            },

            // Symbolic X(PC)
            // Indirect X(Rn)
            Op::Index(Some(r), offset) => write!(f, "{:X}h({})", offset, Op::Reg(*r)),
            // Absolute &Addr
            Op::Index(None, offset)    => write!(f, "&{:X}h", offset),

            // Indirect
            Op::Indirect(r, v) => {
                if v.value == 0 {
                    write!(f, "@{}", Op::Reg(*r))
                } else {
                    write!(f, "@{}+", Op::Reg(*r))
                }
            },

            // Immediate
            Op::Immediate(imm) => write!(f, "#{:X}h", imm)
        }
    } 
}

#[derive(Copy,Clone,Debug)]
pub enum Condition {
    Eq,
    Ne,
    C,
    Nc,
    N,
    Ge,
    L,
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


#[derive(Copy,Clone,Debug)]
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
    Reti(Op),

    Unimpl,
}

impl fmt::Display for Mnem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Mnem::Mov(src,dst,size)  =>  write!(f, "MOV{}\t {}, {}",  size, src, dst),
            Mnem::Add(src,dst,size)  =>  write!(f, "ADD{}\t {}, {}",  size, src, dst),
            Mnem::Addc(src,dst,size) =>  write!(f, "ADDC{}\t {}, {}", size, src, dst),
            Mnem::Sub(src,dst,size)  =>  write!(f, "SUB{}\t {}, {}",  size, src, dst),
            Mnem::Subc(src,dst,size) =>  write!(f, "SUBC{}\t {}, {}", size, src, dst),
            Mnem::Dadd(src,dst,size) =>  write!(f, "DADD{}\t {}, {}", size, src, dst),
            Mnem::Bit(src,dst,size)  =>  write!(f, "BIT{}\t {}, {}",  size, src, dst),
            Mnem::Bis(src,dst,size)  =>  write!(f, "BIS{}\t {}, {}",  size, src, dst),
            Mnem::Bic(src,dst,size)  =>  write!(f, "BIS{}\t {}, {}",  size, src, dst),
            Mnem::Xor(src,dst,size)  =>  write!(f, "XOR{}\t {}, {}",  size, src, dst),
            Mnem::And(src,dst,size)  =>  write!(f, "AND{}\t {}, {}",  size, src, dst),
            Mnem::Cmp(src,dst,size)  =>  write!(f, "CMP{}\t {}, {}",  size, src, dst),

            Mnem::Jmp(offset, cond)  => match cond {
                Condition::A =>  write!(f, "JMP\t{}", offset),
                _            =>  write!(f, "J{} \t{}", cond, offset),           
            },

            Mnem::Rrc(dst,size)     => write!(f, "RCC{}\t {}", size, dst),
            Mnem::Raa(dst,size)     => write!(f, "RRA{}\t {}", size, dst),
            Mnem::Push(dst,size)    => write!(f, "PUSH{}\t {}", size, dst),
            
            Mnem::Swpb(dst)         => write!(f, "SWPB\t {}", dst),
            Mnem::Call(dst)         => write!(f, "CALL\t {}", dst),
            Mnem::Reti(dst)         => write!(f, "RETI\t {}", dst),
            Mnem::Sxt(dst)          => write!(f, "SXT\t {}", dst),

            Mnem::Unimpl => write!(f, "UNIMPL")       
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
