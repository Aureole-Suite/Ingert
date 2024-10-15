use std::{collections::VecDeque, num::NonZeroU8};

use gospel::read::{Le as _, Reader};
use snafu::{OptionExt as _, ResultExt as _};

#[extend::ext]
impl Reader<'_> {
	fn vec3(&mut self) -> Result<glam::Vec3, gospel::read::Error> {
		Ok(glam::Vec3::new(self.f32()?, self.f32()?, self.f32()?))
	}
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(scp), context(suffix(false)))]
pub enum ScpError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("invalid string: {lossy_string:?}"))]
	Utf8 {
		source: std::str::Utf8Error,
		lossy_string: String,
	},
}

#[derive(Debug, Clone, PartialEq)]
struct Entry3 {
	offset: u32,
	count1: u8,
	count2: u8,
	a1: Vec<Value>,
	a2: Vec<Value>,
	a4: Vec<(i32, u16, Vec<TaggedValue>)>,
	a5: u32,
	a6: Value,
}

fn multi<T>(
	f: &mut Reader,
	n: usize,
	g: impl Fn(&mut Reader) -> Result<T, ScpError>,
) -> Result<Vec<T>, ScpError> {
	(0..n).map(|_| g(f)).collect()
}

fn parse_entries2(f: &mut Reader<'_>, n_entries: u32) -> Result<Vec<Entry3>, ScpError> {
	let mut entries = Vec::with_capacity(n_entries as usize);
	for _ in 0..n_entries {
		let offset = f.u32()?;
		let count0 = f.u8()? as usize;
		let count1 = f.u8()?;
		let count2 = f.u8()?;
		let count3 = f.u8()? as usize;
		let a1 = f.u32()? as usize;
		let a2 = f.u32()? as usize;
		let a3 = f.u32()? as usize;
		let a4 = f.u32()? as usize;
		let a5 = f.u32()?;
		let a6 = value(f)?;

		let a1 = multi(&mut f.at(a1)?, count3, value)?;
		let a2 = multi(&mut f.at(a2)?, count0, value)?;
		let a4 = multi(&mut f.at(a4)?, a3, |f| {
			let x = f.i32()?;
			let y = f.u16()?;
			let z = f.u16()? as usize;
			let w = f.u32()? as usize;
			let z = multi(&mut f.at(w)?, z, tagged_value)?;
			Ok((x, y, z))
		})?;
		entries.push(Entry3 {
			offset,
			count1,
			count2,
			a1,
			a2,
			a4,
			a5,
			a6,
		});
	}
	Ok(entries)
}

#[derive(Clone, PartialEq)]
pub enum Value {
	Uint(u32),
	Int(i32),
	Float(f32),
	String(String),
}

impl std::fmt::Debug for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Uint(v) => write!(f, "{:#X}", v),
			Self::Int(v) => write!(f, "{v}"),
			Self::Float(v) => v.fmt(f),
			Self::String(v) => v.fmt(f),
		}
	}
}

#[derive(Clone, PartialEq)]
pub struct TaggedValue(Value, u32);

impl std::fmt::Debug for TaggedValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.1 == 0 {
			self.0.fmt(f)
		} else {
			write!(f, "{:?}:{}", self.0, self.1)
		}
	}
}

fn f30(v: u32) -> f32 {
	fn float_len(v: &f32) -> usize {
		use std::io::Write;
		struct Counter(usize);
		impl Write for Counter {
			fn write(&mut self, v: &[u8]) -> std::io::Result<usize> {
				self.0 += v.len();
				Ok(v.len())
			}
			fn flush(&mut self) -> std::io::Result<()> {
				Ok(())
			}
		}
		let mut c = Counter(0);
		write!(&mut c, "{:?}", v).unwrap();
		c.0
	}
	(0..4)
		.map(|n| f32::from_bits(v << 2 | n))
		.min_by_key(float_len)
		.unwrap()
}

fn value(f: &mut Reader) -> Result<Value, ScpError> {
	let v = f.u32()?;
	let hi = v >> 30;
	let lo = v & 0x3FFFFFFF;
	match hi {
		0 => Ok(Value::Uint(lo)),
		1 => Ok(Value::Int((lo as i32) << 2 >> 2)),
		2 => Ok(Value::Float(f30(lo))),
		3 => {
			let zs = f.at(lo as usize)?.cstr()?.to_bytes();
			let s = std::str::from_utf8(zs).with_context(|_| scp::Utf8 {
				lossy_string: String::from_utf8_lossy(zs).into_owned(),
			})?;
			Ok(Value::String(s.to_string()))
		}
		_ => unreachable!(),
	}
}

fn tagged_value(f: &mut Reader) -> Result<TaggedValue, ScpError> {
	Ok(TaggedValue(value(f)?, f.u32()?))
}

pub fn parse_da(data: &[u8]) -> Result<(), ScpError> {
	tracing::info!("reading");
	let mut f = Reader::new(data);
	f.check(b"#scp")?;
	f.check_u32(24)?;
	let n_entries = f.u32()?;
	let code_start = f.u32()?;
	let n3 = f.u32()?;
	f.check_u32(0)?;

	let entries3 = parse_entries2(&mut f, n_entries)?;
	f.seek(code_start as usize)?;
	let extras = multi(&mut f, n3 as usize, tagged_value)?;

	for g in &entries3 {
		println!(
			"{:04x} {:02X} {:02X} {:08X} {:?} {:?} {:?}",
			g.offset, g.count1, g.count2, g.a5, g.a6, g.a1, g.a2
		);
		for v in &g.a4 {
			println!("  {v:?}");
		}
	}

	let last_offset = entries3
		.iter()
		.map(|e| e.offset)
		.max()
		.unwrap_or(code_start);

	// print!("{:#1X}", f.dump());
	enum Expr {
		Value(Value),
		Op(u8),
		_02(i32),
		_09(u8),
	}
	impl std::fmt::Debug for Expr {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			match self {
				Self::Value(v) => v.fmt(f),
				Self::Op(op) => write!(f, "op_{:X}", op),
				Self::_02(v) => write!(f, "_02({})", v),
				Self::_09(v) => write!(f, "_09({})", v),
			}
		}
	}
	let mut stack = VecDeque::new();
	loop {
		let start = f.pos();
		let op = f.u8()?;
		match op {
			0x00 => {
				f.check_u8(4)?;
				stack.push_front(Expr::Value(value(&mut f)?));
				continue;
			}
			0x01 => {
				f.u8()?;
			}
			0x02 => {
				stack.push_front(Expr::_02(f.i32()?));
				continue;
			}
			0x03 => {
				f.i32()?;
			}
			0x04 => {
				f.i32()?;
			}
			0x05 => {
				f.i32()?;
			}
			0x06 => {
				f.i32()?;
			}
			0x07 => {
				f.u32()?;
			}
			0x08 => {
				f.u32()?;
			}
			0x09 => {
				stack.push_front(Expr::_09(f.u8()?));
				continue;
			}
			0x0A => {
				let v = f.u8()?;
				println!("  _0A {:X} {:?}", v, stack);
				stack.clear();
				continue;
			}
			0x0B => {
				let target = f.u32()?;
				println!("  jump_0B {:X} {:?}", target, stack);
				stack.clear();
				continue;
			}
			0x0C => {
				println!("  call {} {:?} (-> {:X})", f.u16()?, stack, f.pos());
				stack.clear();
				continue;
			}
			0x0D => {
				println!("  return {:?}", stack);
				stack.clear();
				if f.pos() > last_offset as usize {
					break;
				}
				continue;
			}
			0x0E => {
				f.u32()?;
			}
			0x0F => {
				let target = f.u32()?;
				println!("  jump_0F {:X} {:?}", target, stack);
				stack.clear();
				continue;
			}
			0x10..=0x21 => {
				stack.push_front(Expr::Op(op));
				continue;
			}
			0x22 => {
				let a = value(&mut f)?;
				let b = value(&mut f)?;
				let c = f.u8()?;
				println!("  call_extern {:?} {:?} {:X} {:?}", a, b, c, stack);
				stack.clear();
				continue
			}
			0x23 => {
				value(&mut f)?;
				value(&mut f)?;
				f.u8()?;
			}
			0x24 => {
				let a = (f.u8()?, f.u8()?);
				if let Some(v) = NonZeroU8::new(f.u8()?) {
					let b = (f.u8()?, f.u8()?);
					println!("  _24 {a:?} {v} {b:?} {:?}", stack);
				} else {
					println!("  _24 {a:?} {:?}", stack);
				}
				stack.clear();
				continue;
			}
			0x25 => {
				f.u32()?;
			}
			0x26 => {
				let _line = f.u16()?;
				continue;
			}
			0x27 => {
				f.u8()?;
			}
			0x28.. => {
				print!("what? {:#1X}", f.at(f.pos() - 1)?.dump().num_width_as(0));
				// println!();
				break;
			}
		}
		let end = f.pos();
		if !stack.is_empty() {
			println!("  _stack {:?}", stack);
			stack.clear();
		}
		print!("{:#.16X}", f.at(start)?.dump().end(end));
	}

	Ok(())
}
