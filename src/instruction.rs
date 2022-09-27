use super::Value;
use crate::utils::*;

#[derive(Debug, Copy, Clone)]
pub enum Instruction<'a> {
    I{ opcode: IFormat, rd: u8, imm: Value<'a> },
    M{ opcode: MFormat, ra: u8, ro: u8, rd: u8 },
    J{ opcode: JFormat, addr: Value<'a> },
    R{ opcode: RFormat, rs: u8, rd: u8 }
}
#[derive(Debug, Copy, Clone)]
pub enum Opcode {
    I(IFormat),
    M(MFormat),
    J(JFormat),
    R(RFormat)
}
impl Opcode {
    pub fn from_str(s: &str) -> Result<Self> {
        match s {
			"ldl" => Ok(Self::I(IFormat::Ldl)),
			"ldh" => Ok(Self::I(IFormat::Ldh)),
			"adi" => Ok(Self::I(IFormat::Adi)),
			"sbi" => Ok(Self::I(IFormat::Sbi)),
			"ldi" => Ok(Self::I(IFormat::Ldi)),
			"ld" => Ok(Self::M(MFormat::Ld)),
			"st" => Ok(Self::M(MFormat::St)),
			"rjmp" => Ok(Self::J(JFormat::Rjmp)),
			"rjal" => Ok(Self::J(JFormat::Rjal)),
			"add" => Ok(Self::R(RFormat::Add)),
			"adc" => Ok(Self::R(RFormat::Adc)),
			"sub" => Ok(Self::R(RFormat::Sub)),
			"sbc" => Ok(Self::R(RFormat::Sbc)),
			"and" => Ok(Self::R(RFormat::And)),
			"not" => Ok(Self::R(RFormat::Not)),
			"or" => Ok(Self::R(RFormat::Or)),
			"xor" => Ok(Self::R(RFormat::Xor)),
			"lsl" => Ok(Self::R(RFormat::Lsl)),
			"lsr" => Ok(Self::R(RFormat::Lsr)),
			"asl" => Ok(Self::R(RFormat::Asl)),
			"asr" => Ok(Self::R(RFormat::Asr)),
			"rol" => Ok(Self::R(RFormat::Rol)),
			"ror" => Ok(Self::R(RFormat::Ror)),
			"sf" => Ok(Self::R(RFormat::Sf)),
			"gf" => Ok(Self::R(RFormat::Gf)),
			"int" => Ok(Self::R(RFormat::Int)),
			"gr" => Ok(Self::R(RFormat::Gr)),
			"push" => Ok(Self::R(RFormat::Push)),
			"pop" => Ok(Self::R(RFormat::Pop)),
			"mov" => Ok(Self::R(RFormat::Mov)),
			"movsx" => Ok(Self::R(RFormat::Movsx)),
			"jmp" => Ok(Self::R(RFormat::Jmp)),
			"jz" => Ok(Self::R(RFormat::Jz)),
			"jnz" => Ok(Self::R(RFormat::Jnz)),
			"jn" => Ok(Self::R(RFormat::Jn)),

            _ => Err(AsmErr::BadOpcode(s))
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum IFormat {
    Ldl, Ldh, Adi, Sbi, Ldi
    // ldi is a pseudo-instruction that desugars into a ldl and a ldh
}
impl IFormat {
	pub fn binary(&self) -> u16 {
		match self {
			Self::Ldl => 0x0,
			Self::Ldh => 0x1,
			Self::Adi => 0x2,
			Self::Sbi => 0x3,
			Self::Ldi => unreachable!()
		}
	}
}
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum MFormat {
    Ld, St
}
impl MFormat {
	pub fn binary(&self) -> u16 {
		match self {
			Self::Ld => 0x4,
			Self::St => 0x5
		}
	}
}
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum JFormat {
    Rjmp, Rjal
}
impl JFormat {
	pub fn binary(&self) -> u16 {
		match self {
			Self::Rjmp => 0x6,
			Self::Rjal => 0x7
		}
	}
}
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum RFormat {
    Add, Adc, Sub, Sbc,
    And, Not, Or, Xor,
    Lsl, Lsr, Asl, Asr, Rol, Ror,
    Sf, Gf, Int, Gr,
    Push, Pop, Mov, Movsx,
	Jmp, Jz, Jnz, Jn
}
impl RFormat {
	pub fn binary(&self) -> u16 {
		match self {
			Self::Add => 0x09,
			Self::Adc => 0x19,
			Self::Sub => 0x29,
			Self::Sbc => 0x39,

			Self::And => 0x49,
			Self::Not => 0x59,
			Self::Or  => 0x69,
			Self::Xor => 0x79,

			Self::Lsl => 0x89,
			Self::Lsr => 0x99,
			Self::Asl => 0xa9,
			Self::Asr => 0xb9,
			Self::Rol => 0xc9,
			Self::Ror => 0xd9,

			Self::Gf  => 0xe9,
			Self::Sf  => 0xf9,

			Self::Jmp => 0x08,
			Self::Jz  => 0x18,
			Self::Jnz => 0x28,
			Self::Jn  => 0x38,

			Self::Push => 0x88,
			Self::Pop => 0x98,
			Self::Mov => 0xa8,
			Self::Movsx => 0xb8,

			Self::Int => 0x0a,
			Self::Gr => 0x1a
		}
	}
}

