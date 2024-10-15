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

#[derive(Debug)]
struct Entry {
	offset: u32,
	counts: [u8; 4],
	a1: u32,
	a2: u32,
	a3: u32,
	a4: u32,
	a5: u32,
	a6: Value,
}

#[derive(Debug)]
struct Entry2 {
	offset: u32,
	count1: u8,
	count2: u8,
	a1: Vec<Value>,
	a2: Vec<Value>,
	a4: Vec<(i32, u16, u16, u32)>,
	a5: u32,
	a6: Value,
}

#[derive(Debug, Clone, PartialEq)]
struct Entry3 {
	offset: u32,
	count1: u8,
	count2: u8,
	a1: Vec<Value>,
	a2: Vec<Value>,
	a4: Vec<(i32, u16, Vec<(Value, u32)>)>,
	a5: u32,
	a6: Value,
}

fn parse_entries(f: &mut Reader<'_>, n_entries: u32) -> Result<Vec<Entry3>, ScpError> {
	let mut entries = Vec::new();
	for _ in 0..n_entries {
		let offset = f.u32()?;
		let counts = f.array()?;
		let a1 = f.u32()?;
		let a2 = f.u32()?;
		let a3 = f.u32()?;
		let a4 = f.u32()?;
		let a5 = f.u32()?;
		let a6 = value(f)?;
		entries.push(Entry {
			offset,
			counts,
			a1,
			a2,
			a3,
			a4,
			a5,
			a6,
		});
	}

	let mut entries2 = entries
		.iter()
		.map(|e| Entry2 {
			offset: e.offset,
			count1: e.counts[1],
			count2: e.counts[2],
			a1: Vec::new(),
			a2: Vec::new(),
			a4: Vec::new(),
			a5: e.a5,
			a6: e.a6.clone(),
		})
		.collect::<Vec<_>>();

	for (e, g) in std::iter::zip(&entries, &mut entries2) {
		assert_eq!(f.pos(), e.a1 as usize);
		for _ in 0..e.counts[3] {
			g.a1.push(value(f)?);
		}
	}

	for (e, g) in std::iter::zip(&entries, &mut entries2) {
		assert_eq!(f.pos(), e.a2 as usize);
		for _ in 0..e.counts[0] {
			g.a2.push(value(f)?);
		}
	}

	for (e, g) in std::iter::zip(&entries, &mut entries2) {
		assert_eq!(f.pos(), e.a4 as usize);
		for _ in 0..e.a3 {
			g.a4.push((f.i32()?, f.u16()?, f.u16()?, f.u32()?));
		}
	}

	let mut entries3 = entries2
		.iter()
		.map(|e| Entry3 {
			offset: e.offset,
			count1: e.count1,
			count2: e.count2,
			a1: e.a1.clone(),
			a2: e.a2.clone(),
			a4: e.a4.iter().map(|x| (x.0, x.1, Vec::new())).collect(),
			a5: e.a5,
			a6: e.a6.clone(),
		})
		.collect::<Vec<_>>();

	for (e, g) in std::iter::zip(&entries2, &mut entries3) {
		for (e, g) in std::iter::zip(&e.a4, &mut g.a4) {
			assert_eq!(f.pos(), e.3 as usize);
			for _ in 0..e.2 {
				g.2.push((value(f)?, f.u32()?));
			}
		}
	}

	Ok(entries3)
}

fn parse_entries2(f: &mut Reader<'_>, n_entries: u32) -> Result<Vec<Entry3>, ScpError> {
	fn multi<T>(
		f: &mut Reader,
		n: usize,
		g: impl Fn(&mut Reader) -> Result<T, ScpError>,
	) -> Result<Vec<T>, ScpError> {
		(0..n).map(|_| g(f)).collect()
	}
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
			let z = multi(&mut f.at(w)?, z, |f| Ok((value(f)?, f.u32()?)))?;
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

pub fn parse_da(data: &[u8]) -> Result<(), ScpError> {
	tracing::info!("reading");
	let mut f = Reader::new(data);
	f.check(b"#scp")?;
	f.check_u32(24)?;
	let n_entries = f.u32()?;
	let code_start = f.u32()?;
	let n3 = f.u32()?;
	f.check_u32(0)?;

	let start = f.pos();
	let entries2 = parse_entries(&mut f, n_entries)?;
	assert_eq!(f.pos(), code_start as usize);

	f.seek(start)?;
	let entries3 = parse_entries2(&mut f, n_entries)?;
	assert_eq!(entries2, entries3);
	f.seek(code_start as usize)?;
	for _ in 0..n3 {
		f.u32()?;
		f.u32()?;
	}

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
	loop {
		let start = f.pos();
		let op = f.u8()?;
		match op {
			0x00 => {
				f.check_u8(4)?;
				println!("  push {:x?}", value(&mut f)?);
				continue;
			}
			0x01 => {
				f.u8()?;
			}
			0x02 => {
				f.i32()?;
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
				f.u8()?;
			}
			0x0A => {
				f.u8()?;
			}
			0x0B => {
				f.u32()?;
			}
			0x0C => {
				f.u16()?;
			}
			0x0D => {
				println!("  return");
				if f.pos() > last_offset as usize {
					break;
				}
				continue;
			}
			0x0E => {
				f.u32()?;
			}
			0x0F => {
				f.u32()?;
			}
			0x10..=0x21 => {}
			0x22 | 0x23 => {
				f.u32()?;
				f.u32()?;
				f.u8()?;
			}
			0x24 => {
				f.u16()?;
				if f.u8()? != 0 {
					f.u16()?;
				}
			}
			0x25 => {
				f.u32()?;
			}
			0x26 => {
				println!("  line {}", f.u16()?);
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
		print!("{:#.16X}", f.at(start)?.dump().end(end));
	}
	// print!("{:#1X}", f.dump());
	print!("{:#.16X}", f.dump());

	Ok(())
}
