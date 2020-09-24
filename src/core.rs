use crate::procvalue::ProcValue;
use crate::instr::*;
use crate::cpu::Cpu;

pub fn step(c: &mut Cpu) {
    let addr  = c.get_pc();
    let bytes = fetch(c, addr);
    let instr = decode(c, bytes,addr);
    exec(c, instr);
}

pub fn disassemble(c: &Cpu, addr: ProcValue) -> () {
    let bytes = c.read_memory(addr.value, SizeMode::Word);
    let instr = decode(c,bytes,addr);
    println!("{}", instr);
}

pub fn fetch(c: &mut Cpu, addr: ProcValue) -> u16 {
    c.read_memory(addr.value, SizeMode::Word)
}

// Page 62
pub fn decode(p: &Cpu, bytes: u16, addr: ProcValue) -> Instr {

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
                _ => Mnem::Unimpl()
            }
        },

        // Unary Op Type II
        0x0000 => {

            if (bytes & 0x0F80) == 0x280 {
                let d_r = (bytes & 0xFu16) as u8;
                let d_mode = (bytes >> 4u16) & 0x3u16;
                let (dst,osize) = decode_as_mode(p, d_r, d_mode as u8, &addr, size);
                size = osize;  
                Mnem::Call(dst)
            } else {
                let d_r = (bytes & 0xFu16) as u8;
                let d_mode = (bytes >> 7u16) & 0x1u16;
                let (dst,osize) = decode_ad_mode(p, d_r, d_mode as u8, &addr, size);
                size = osize;
                
                match bytes & 0x0F80 {
                    0x000 => Mnem::Rrc( dst, b),
                    0x100 => Mnem::Raa( dst, b),
                    0x200 => Mnem::Push(dst, b),
                    
                    0x080 => Mnem::Swpb(dst),
                    0x180 => Mnem::Sxt( dst),
                    0x300 => Mnem::Reti(),

                    _ => Mnem::Unimpl()
                }
            }
        },

        // Binary Op Type I
        _ => {
            let s_r = (bytes >> 8u16) as u8 & 0xFu8;
            let s_mode = (bytes >> 4u16) & 0x3u16;
            let (src,osize) = decode_as_mode(p, s_r, s_mode as u8, &addr, 2);
            
            let d_r = (bytes & 0xFu16) as u8;
            let d_mode = (bytes >> 7u16) & 0x1u16;
            let (dst,osize) = decode_ad_mode(p, d_r, d_mode as u8, &addr, osize);
            size = osize;

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

                _ => Mnem::Unimpl()
            }
        }
        // TODO

    };

    instr(mnem, bytes, addr, size)
}

pub fn exec(c: &mut Cpu, instr: Instr) -> () {

    match instr.mnem {
        
        Mnem::Jmp(_,_) => (), // Don't do anything on Jump, handled below

        Mnem::Mov(src, dst, b) => {
            let val = get_op_value(c, src, b);
            set_op_value(c, dst, b, val);
            // No Flag Update
        },

        Mnem::Add(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let (sum, carry, overflow) = inst_add( x, y, 0, b );
            c.set_flags(carry, sum.value == 0, is_neg(sum, b), overflow);
            set_op_value(c, dst, b, sum);
        },
        Mnem::Addc(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let (sum, carry, overflow) = inst_add( x, y, if c.c_get() {1} else {0}, b);
            c.set_flags(carry, sum.value == 0, is_neg(sum, b), overflow);
            set_op_value(c, dst, b, sum);
        },
        Mnem::Sub(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let (sum, carry, overflow) = inst_sub( x, y, 0, b );
            c.set_flags(carry, sum.value == 0, is_neg(sum, b), overflow);
            set_op_value(c, dst, b, sum);
        },
        Mnem::Subc(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let (sum, carry, overflow) = inst_sub( x, y, if c.c_get() {1} else {0}, b);
            c.set_flags(carry, sum.value == 0, is_neg(sum, b), overflow);
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
            c.set_flags(res.value != 0, res.value == 0, is_neg(res, b), 
                        is_neg(x,b) && is_neg(y,b));
            set_op_value(c, dst, b, res);
        },
        Mnem::And(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let res = x & y;
            c.set_flags(res.value != 0, res.value == 0, is_neg(res,b), false);
            set_op_value(c, dst, b, res);
        },

        Mnem::Cmp(src, dst, b) => {
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let (sum, carry, overflow) = inst_sub( x, y, 0, b );
            c.set_flags(carry, sum.value == 0, is_neg(sum, b), overflow);
            // No Value Write-back
        },
        Mnem::Bit(src, dst, b) => { 
            let (x,y) = ( get_op_value(c, dst, b) , get_op_value(c, src, b));
            let res = x & y;
            c.set_flags(res.value != 0, res.value == 0, is_neg(res, b), false);
            // No Value Write-back
        },
        Mnem::Dadd(_,_,_) => {
            unimplemented!("{} unimplement", instr.mnem)
        }, 

        Mnem::Rrc(dst, b) => {
            let x = get_op_value(c,dst,b);
            let new_c = x.value & 0b1;
            let old_c = if c.c_get() { 1 } else { 0 };
            let x = ProcValue::from( 
                match b { 
                    SizeMode::Word => ((old_c << 15) | (x.value >> 1)),
                    SizeMode::Byte => ((old_c << 15) | (x.value >> 1)),
            });
            c.set_flags(new_c != 0, x.value != 0, is_neg(x,b), false);
            set_op_value(c, dst, b, x);
        },
        Mnem::Raa(dst, b) => {
            let x = get_op_value(c,dst,b);
            let new_c = x.value & 0b1;
            let x = ProcValue::from( 
                match b { 
                    SizeMode::Word => (x.value & 0x8000) | (x.value >> 1),
                    SizeMode::Byte => (x.value & 0x80)   | (x.value >> 1),
            });
            c.set_flags(new_c != 0, x.value != 0, is_neg(x,b), false);
            set_op_value(c, dst, b, x);
        },
        Mnem::Push(dst, b) => {
            let new_sp = c.get_reg_value(1) - 2;
            let value = get_op_value(c, dst, b);
            c.set_reg_value(1, new_sp);
            c.write_memory(new_sp.value, value.value, b);
        },
        Mnem::Sxt(dst) => {
            let value = get_op_value(c, dst, SizeMode::Byte);
            let value = match value.value & 0x80 {
                1 => value | 0xFF00,
                _ => value 
            };
            set_op_value(c, dst, SizeMode::Byte, value);
            c.set_flags(value.value != 0, value.value != 0, 
                        is_neg(value,SizeMode::Word), false);
        },
        Mnem::Swpb(dst) => {
            let value = get_op_value(c, dst, SizeMode::Word);
            let high = (value.value >> 8) & 0xFFu16; 
            let low  = value.value & 0xFFu16; 
            let value = ProcValue::from( high | (low << 8) );
            set_op_value(c, dst, SizeMode::Word, value);
        },
        Mnem::Call(_) => {
            let new_sp = c.get_reg_value(1) - 2;
            c.set_reg_value(1, new_sp);
            let ret_addr = instr.addr + instr.size;
            c.write_memory(new_sp.value, ret_addr.value, SizeMode::Word);
        },
        Mnem::Reti() => {
            let sp = c.get_reg_value(1);
            let restored_sr = c.read_memory(sp.value, SizeMode::Word);
            c.set_reg_value(2, ProcValue::from(restored_sr));
            let sp = sp - 2;
            let restored_pc = c.read_memory(sp.value, SizeMode::Word);
            c.set_reg_value(0, ProcValue::from(restored_pc));
            let sp = sp - 2;
            c.set_reg_value(1, sp);
        },

        Mnem::Unimpl() => unimplemented!("Illegal Instruction @ {:04X} = {:04X}", instr.addr, instr.bytes)
    
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
        Mnem::Call(dst) =>  {
            let value = get_op_value(c, dst, SizeMode::Word);
            c.set_reg_value(0, value);
        },
        // Return, don't increment
        Mnem::Mov(Op::IndirectAutoIncr(1), Op::Reg(0), SizeMode::Word) => (),
        _ => c.step(instr)
    };
    
}

fn decode_as_mode(c: &Cpu, r:u8, mode:u8, addr: &ProcValue, size: u16) -> (Op,u16) {
    
    // println!("AS: {}, {}", mode, r);
    let mut size = size;
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
            let offset = ProcValue::from(c.read_memory(
                addr.value + size, SizeMode::Word));
            size += 2;
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
        (2,r) => Op::Indirect(r),

        // Immediate Mode         #N   => *(Pc)
        (3,0) => {
            let imm = ProcValue::from(c.read_memory(
                addr.value + size, SizeMode::Word));
            size += 2;
            Op::Immediate(imm)
        },

        (3,r) => { 
            // Indirect Autoincrement @Rn+ => *(Rn++)
            // Word mode is 0 => +2, Byte mode is 1 => +1
            // This is handled by get_op_value 
            Op::IndirectAutoIncr(r)
        }
        
        _ => unimplemented!("Impossible AS Mode")
    }, size)
}

fn decode_ad_mode(c: &Cpu, r:u8, mode:u8, addr: &ProcValue, size:u16) -> (Op, u16) {
    // println!("AD: {}, {}", mode, r);
    let mut size = size;
    (match (mode, r) {
        // Register
        (0, r) => Op::Reg(r),

        // Absolute
        (1, r) => {
            let offset = ProcValue::from(c.read_memory(
                addr.value + size, SizeMode::Word));
            size += 2;
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
        Condition::Eq =>  c.z_get(),
        Condition::Ne => !c.z_get(),

        // C flag
        Condition::C  =>  c.c_get(),
        Condition::Nc => !c.c_get(),

        // N flags
        Condition::N  => c.n_get(),

        // N xor V == 0
        Condition::Ge => c.n_get() == c.v_get(),

        // N xor V == 1
        Condition::L  => c.n_get() != c.v_get()
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
        Op::Indirect(r) => {
            let addr = c.get_reg_value(r);
            ProcValue::from( c.read_memory(addr.value, size) )
        } 
        Op::IndirectAutoIncr(r) => {
            let addr = c.get_reg_value(r);
            let increment = if size == SizeMode::Word { 2 } else {1};
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
    
    ProcValue::from(op.value)
}

fn set_op_value(c: &mut Cpu, op : Op, size: SizeMode, value : ProcValue) {
    match op {
        // Register Mode
        Op::Reg(r) => c.set_reg_value(r, value),
        
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

fn inst_add(a: ProcValue, b:ProcValue, c:u16, size: SizeMode) -> (ProcValue, bool, bool){
    
    let sum = (a.value as u32) + (b.value as u32) + (c as u32);
   
    match size {
        SizeMode::Byte => (
            ProcValue {
                value : (sum & 0xFFu32) as u16,
            },            
            sum & 0x100 != 0, // Carry
            (ProcValue::from((sum & 0x80) as u16) != a & 0x80u16) && (a & 0x80 == b & 0x80) // Overflow
        ),
        
        SizeMode::Word => (
            ProcValue {
                value : (sum & 0xFFFFu32) as u16,
            },
            sum & 0x10000 != 0, // Carry
            (ProcValue::from((sum & 0x8000) as u16) != a & 0x8000u16) && (a & 0x8000 == b & 0x8000) //Overflow
        )
    }
}

fn inst_sub(a: ProcValue, b:ProcValue, c:u16, size : SizeMode) -> (ProcValue, bool, bool){
    inst_add(a, -b, c, size)
}

fn is_neg(v: ProcValue, size: SizeMode) -> bool {
    match size { 
        SizeMode::Byte => (v.value & 0x80) != 0,
        SizeMode::Word => (v.value & 0x8000) != 0,
    }
}