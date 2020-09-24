use std::fmt;

use std::ops;
use std::convert::From;
use std::convert::Into;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ProcValue {
    pub value       : u16,
}

impl From<u16> for ProcValue { 
    fn from(w : u16) -> Self {
        ProcValue { 
            value   : w,
        }
    }
}
impl From<u8> for ProcValue { 
    fn from(w : u8) -> Self {
        ProcValue { 
            value   : w as u16,
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
        ProcValue::from(res)
    }
}
impl ops::Add<u16> for ProcValue { 
    type Output = ProcValue;
    fn add(self, other : u16) -> ProcValue {
        let (val,_) = other.overflowing_add(self.value);
        ProcValue::from(val)
    }
}
impl ops::Sub<ProcValue> for ProcValue { 
    type Output = ProcValue;
    fn sub(self, other : ProcValue) -> ProcValue {
        let other = -other;
        let (res,_) = other.value.overflowing_add(self.value);
        ProcValue::from(res)
    }
}
impl ops::Sub<u16> for ProcValue { 
    type Output = ProcValue;
    fn sub(self, other : u16) -> ProcValue {
        let other = 1 + (other ^ 0xFFFF);
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
        ProcValue::from(res)
    }
}
impl ops::Mul<u16> for ProcValue { 
    type Output = ProcValue;
    fn mul(self, other : u16) -> ProcValue { 
        let (res, _) = other.overflowing_mul(self.value);
        ProcValue::from(res)
    }
}

impl ops::Neg for ProcValue { 
    type Output = ProcValue;
    fn neg(self) -> ProcValue {
        let val = self.value ^ 0xFFFF;
        let (val, _) = val.overflowing_add(1);
        ProcValue::from(val)
    }   
}

impl fmt::Display for ProcValue { 
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl fmt::UpperHex for ProcValue { 
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:X}", self.value)
    }
}
