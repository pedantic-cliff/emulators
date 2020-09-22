
use crate::instr::*;
use crate::cpu::Cpu;

pub fn step(c: &mut Cpu) {
    let bytes = fetch(c);
    let instr = decode(c, bytes);
    exec(c, instr);
}

pub fn fetch(c: &mut Cpu) -> u16 {
    c.fetch_next()
}

// Page 62
pub fn decode(p: &mut Cpu, bytes: u16) -> Instr {

    let b = if (bytes >> 6) & 0x1 == 1 { SizeMode::Byte } else { SizeMode::Word };

    let op_type = bytes & 0xE000;
    let mut size = 2;

    let mnem = match op_type {

        // Jump Type III
        0x2000 => {
            let offset = ((bytes & 0x3FFF)<< 6) as i16 >> 6;
            let offset = ProcValue::from(offset as u16);
            let offset = Op::PcOffset(p.get_pc(), offset);
            match bytes >> 10 & 0x7 {
                0x00 => Mnem::Jmp(offset, Condition::Ne),
                0x01 => Mnem::Jmp(offset, Condition::Eq),
                0x02 => Mnem::Jmp(offset, Condition::Nc),
                0x03 => Mnem::Jmp(offset, Condition::C),
                0x04 => Mnem::Jmp(offset, Condition::N),
                0x05 => Mnem::Jmp(offset, Condition::Ge),
                0x06 => Mnem::Jmp(offset, Condition::L),
                0x07 => Mnem::Jmp(offset, Condition::A),
                _ => Mnem::Unimpl
            }
        },

        // Unary Op Type II
        0x0000 => {
            let (dst,dsize) = decode_ad_mode(p, &bytes);
            size += dsize;

            match bytes & 0x0F80 {
                0x000 => Mnem::Rrc( dst, b),
                0x100 => Mnem::Raa( dst, b),
                0x200 => Mnem::Push(dst, b),
                
                0x080 => Mnem::Swpb(dst),
                0x180 => Mnem::Sxt( dst),
                0x280 => Mnem::Call(dst),
                0x300 => Mnem::Reti(dst),

                _ => Mnem::Unimpl
            }
        },

        // Binary Op Type I
        _ => {
            let (src,ssize) = decode_as_mode(p, &bytes);
            let (dst,dsize) = decode_ad_mode(p, &bytes);
            size += ssize + dsize;

            match bytes & 0xF000 {
                0x4000 => Mnem::Mov( src, dst, b),
                0x5000 => Mnem::Add( src, dst, b),
                0x6000 => Mnem::Addc(src, dst, b),
                0x7000 => Mnem::Subc(src, dst, b),
                0x8000 => Mnem::Sub( src, dst, b),
                0x9000 => Mnem::Cmp( src, dst, b),
                0xA000 => Mnem::Dadd(src, dst, b),
                0xB000 => Mnem::Bit( src, dst, b),
                0xC000 => Mnem::Bic( src, dst, b),
                0xD000 => Mnem::Bis( src, dst, b),
                0xE000 => Mnem::Xor( src, dst, b),
                0xF000 => Mnem::And( src, dst, b),

                _ => Mnem::Unimpl
            }
        }
        // TODO

    };

    instr(mnem, bytes, p.get_pc(), size)
}

pub fn exec(c: &mut Cpu, instr: Instr) -> () {
    println!("{}", instr);

    match instr.mnem {
        
        Mnem::Jmp(_,_) => (), // Don't do anything on Jump, handled below

        Mnem::Mov(src, dst, b) => {
            let val = get_op_value(c, src, b);
            set_op_value(c, dst, b, val);
            // No Flag Update
        },

        Mnem::Add(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let (sum, carry, overflow) = inst_add( x, y, 0 );
            c.set_flags(sum, carry, overflow);
            set_op_value(c, dst, b, sum);
        },
        Mnem::Addc(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let (sum, carry, overflow) = inst_add( x, y, if c.c_get() {1} else {0} );
            c.set_flags(sum, carry, overflow);
            set_op_value(c, dst, b, sum);
        },
        Mnem::Sub(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let (sum, carry, overflow) = inst_sub( x, y, if c.c_get() {1} else {0} );
            c.set_flags(sum, carry, overflow);
            set_op_value(c, dst, b, sum);
        },
        Mnem::Subc(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let (sum, carry, overflow) = inst_sub( x, y, if c.c_get() {1} else {0} );
            c.set_flags(sum, carry, overflow);
            set_op_value(c, dst, b, sum);
        },

        Mnem::Bis(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            set_op_value(c, dst, b, x | y);
            // No Flags Update
        },
        Mnem::Bic(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            set_op_value(c, dst, b, x & (y ^ 0xFFFFu16 ));
            // No Flags Update
        },

        Mnem::Xor(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let res = x ^ y;
            c.set_flags(res, res.value != 0, (!x.is_neg()) && (!y.is_neg()));
            set_op_value(c, dst, b, res);
        },
        Mnem::And(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let res = x & y;
            c.set_flags(res, res.value != 0, false);
            set_op_value(c, dst, b, res);
        },

        Mnem::Cmp(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let (sum, carry, overflow) = inst_sub( x, y, if c.c_get() {1} else {0} );
            c.set_flags(sum, carry, overflow);
            // No Value Write-back
        },
        Mnem::Bit(src, dst, b) => { 
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let res = x & y;
            c.set_flags(res, res.value != 0, false);
            // No Value Write-back
        },
        Mnem::Dadd(_,_,_) => {
            unimplemented!("{} unimplement", instr.mnem)
        }, 


        Mnem::Rrc(dst, size) => {
            unimplemented!("{}{}\t {} unimplemented", instr.mnem, size, dst)
        }
        Mnem::Raa(dst, size) => {
            unimplemented!("{}{}\t {} unimplemented", instr.mnem, size, dst)
        }
        Mnem::Push(dst, size) => {
            unimplemented!("{}{}\t {} unimplemented", instr.mnem, size, dst)
        }
        Mnem::Sxt(dst) => {
            unimplemented!("{}\t {} unimplemented", instr.mnem, dst)
        }
        Mnem::Swpb(dst) => {
            unimplemented!("{}\t {} unimplemented", instr.mnem, dst)
        }
        Mnem::Call(dst) => {
            unimplemented!("{}\t {} unimplemented", instr.mnem, dst)
        }
        Mnem::Reti(dst) => {
            unimplemented!("{}\t {} unimplemented", instr.mnem, dst)
        }

        Mnem::Unimpl => unimplemented!("Illegal Instruction @ {:04X} = {:04X}", instr.addr, instr.bytes)
    
    }

    // Step the CPU 
    match instr.mnem { 
        Mnem::Jmp(Op::PcOffset(_, offset), cond) => {
            if check_cond(c, cond) {
                c.jmp(offset)
            } else {
                c.step(instr)
            }
        },
        _ => c.step(instr)
    };
    
}

fn decode_as_mode(c: &mut Cpu, bytes: &u16) -> (Op,u16) {
    let r = (*bytes >> 8u16) as u8 & 0xFu8;
    let mode = (bytes >> 4u16) & 0x3u16;
    
    // println!("AS: {}, {}", mode, r);
    let mut size = 0;
    ( match (mode, r) {
        // Constants
        (0,3) => Op::Constant(0u16.into()),        
        (1,3) => Op::Constant(1u16.into()), 
        (2,2) => Op::Constant(4u16.into()),
        (2,3) => Op::Constant(2u16.into()),
        (3,2) => Op::Constant(8u16.into()),
        (3,3) => Op::Constant(0xFFFFu16.into()),

        // Register Mode Rn
        (0,r) => Op::Reg(r),

        // Indexed Mode (Value in next word)
        (1,r) => {
            let offset = c.fetch_next().into();
            size = 2;
            match r {
                // Absolute &Addr     => *(X)
                2 => Op::Index(None, offset),

                // Symbolic Addr      => *(PC+X)
                0 => Op::Index(Some(0), offset),

                // Indexed Mode X(Rn) => *(Rn+X)
                _ => Op::Index(Some(r), offset),
            }
        },


        // Indirect Mode @Rn  => *(Rn)
        (2,r) => Op::Indirect(r, 0u16.into()),

        // Immediate Mode         #N   => *(Pc)
        (3,0) => {
            let imm = c.fetch_next().into();
            size = 2;
            Op::Immediate(imm)
        },

        (3,r) => { 
            // Indirect Autoincrement @Rn+ => *(Rn++)
            // Word mode is 0 => +2, Byte mode is 1 => +1
            if (*bytes >> 5 & 0x1) == 0 {
                Op::Indirect(r, 2u16.into())
            } else {
                Op::Indirect(r, 1u16.into())
            }
        }
        
        _ => unimplemented!("Impossible AS Mode")
    }, size)
}

fn decode_ad_mode(c: &mut Cpu, bytes: &u16) -> (Op, u16) {
    let r = (*bytes & 0xFu16) as u8;
    let mode = (bytes >> 7u16) & 0x1u16;

    // println!("AD: {}, {}", mode, r);
    let mut size = 0;
    (match (mode, r) {
        // Register
        (0, r) => Op::Reg(r),

        // Absolute
        (1, r) => {
            let offset = c.fetch_next().into();
            size = 2;
            match r {
                // Absolute
                2 => Op::Index(None, offset),

                // Symbolic Addr      => *(PC+X)
                0 => Op::Index(Some(0), offset),

                // Indexed Mode X(Rn) => *(Rn+X)
                _ => Op::Index(Some(r), offset),
            }
        },
        _ => unimplemented!("Impossible AS Mode")
    }, size)
}

fn check_cond(c: &Cpu, cond: Condition) -> bool {
    match cond {
        Condition::A => true,
        
        // Z flag
        Condition::Eq => c.z_get(),
        Condition::Ne => !c.z_get(),

        // C flag
        Condition::C  => c.c_get(),
        Condition::Nc => !c.c_get(),

        // N flags
        Condition::N  => c.n_get(),

        // N xor V == 0
        Condition::Ge => (c.n_get() == c.v_get()),

        // N xor V == 1
        Condition::L  => (c.n_get() != c.v_get())
    }
}

fn get_op_value(c: &mut Cpu, op : Op, size: SizeMode) -> ProcValue {
    let op = match op {
        // Returns a value
        Op::Constant(c) => c,

        // Returns a value
        Op::Immediate(imm) => imm,

        // Returns a value
        Op::Reg(r) => c.get_reg_value(r),

        // Returns dereferenced address
        // Optionally increments index register
        // Note: R = 0 actually hits the Immediate branch
        Op::Indirect(r,increment) => {
            let addr = c.get_reg_value(r);
            c.set_reg_value(r, (addr + increment).into());
            ProcValue::from( c.read_memory(addr.value, size) )
        } 

        // Absolute
        Op::Index(None, offset) => 
            ProcValue::from( c.read_memory(offset.value, size) ),

        // Symbolic
        Op::Index(Some(0), offset) => {
            let addr = c.get_pc() + offset;
            ProcValue::from( c.read_memory( addr.value, size) )
        }, 

        // Indexed Mode
        Op::Index(Some(r), offset) => {
            let addr = c.get_reg_value(r) + offset;
            ProcValue::from( c.read_memory( addr.value, size) )
        },

        // Returns an Address
        Op::PcOffset(_, offset) => offset,
    };
    
    ProcValue {
        value : op.value,
        size  : size,
    }
}


fn set_op_value(c: &mut Cpu, op : Op, size: SizeMode, value : ProcValue) {
    match op {
        // Register Mode
        Op::Reg(r) => c.set_reg_value(r, value.value),
        
        // Indexed Mode
        Op::Index(Some(r), offset) => {
            let addr = c.get_reg_value(r) + offset;
            c.write_memory(addr.value, value.value, size) 
        },
        // Absolute
        Op::Index(None, offset) => {
            c.write_memory( offset.value, value.value, size ) 
        },

        _ => unimplemented!("Illegal Addr Type")
    }
}

fn inst_add(a: ProcValue, b:ProcValue, c:u16) -> (ProcValue, bool, bool){
    if a.size != b.size {
        unimplemented!("Operands don't match, this shouldn't happen!")
    }
   
    let sum = (a.value as u32) + (b.value as u32) + (c as u32);
   
    match a.size {
        SizeMode::Byte => (
            ProcValue {
                value : (sum & 0xFFu32) as u16,
                size  : a.size
            },            sum & 0x100 != 0,
            ( ProcValue::from((sum & 0x80) as u16) != a & 0x80u16 ) 
                && (a & 0x80 == b & 0x80)
        ),
        
        SizeMode::Word => (
            ProcValue {
                value : (sum & 0xFFFFu32) as u16,
                size  : a.size
            },
            sum & 0x10000 != 0,
            ( ProcValue::from((sum & 0x8000) as u16) != a & 0x8000u16 ) 
                && (a & 0x8000 == b & 0x8000)
        )
    }
}

fn inst_sub(a: ProcValue, b:ProcValue, c:u16) -> (ProcValue, bool, bool){
    inst_add(a, -b, c)
}