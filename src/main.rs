use clap::Parser;
use std::path::{Path, PathBuf};
use std::collections::{BTreeMap, HashSet};

#[derive(clap::Parser)]
struct Args {
	files: Vec<PathBuf>,
}

fn main() {
	tracing_subscriber::fmt::fmt()
		.with_writer(std::io::stderr)
		.init();
	let args = Args::parse();

	let mut prelude = BTreeMap::new();
	let mut prelude_order = Vec::new();
	for file in &args.files {
		let _span = tracing::info_span!("file", path = %file.display()).entered();
		if file.ends_with("mon9996_c00.da") {
			// this one has a very different format, where the first table is half the width
			tracing::info!("Skipping mon9996_c00.da");
			continue;
		}
		let Ok(data) = std::fs::read(file) else {
			tracing::error!("Failed to read file");
			continue;
		};

		let scena = ingert::decompile(&data).unwrap();
		let out = Path::new("out").join(file.file_name().unwrap());
		let out = std::fs::File::create(&out).unwrap();
		let out = std::io::BufWriter::new(out);
		let mut out = ingert::Write(Box::new(out));

		let mut prelude_funcs = Vec::new();
		for item in &scena {
			match item {
				ingert::Item::Global(g) => {
					if let Some(line) = g.line {
						write!(out, "line {} ", line);
					}
					writeln!(out, "global {} = {}", g.name, g.ty);
				}
				ingert::Item::Function(f) => {
					if f.is_prelude {
						if let Some(prev) = prelude.get(&f.name) {
							if prev != f {
								tracing::warn!("{} differs from prelude", f.name);
								write!(out, "prelude ");
								write_fn(&mut out, f);
							}
						} else {
							prelude.insert(f.name.clone(), f.clone());
						}
						prelude_funcs.push(f.name.clone());
					} else {
						write_fn(&mut out, f);
					}
				}
			}
		}
		prelude_order.push(prelude_funcs);
	}

	let prelude_order = prelude_order.into_iter().fold(Vec::new(), ordered_union);

	let out = Path::new("out").join("prelude.da");
	let out = std::fs::File::create(&out).unwrap();
	let out = std::io::BufWriter::new(out);
	let mut out = ingert::Write(Box::new(out));

	for name in prelude_order {
		write_fn(&mut out, &prelude.remove(&name).unwrap());
	}
	writeln!(out, "// {} functions left in prelude", prelude.len());
	for f in prelude.values() {
		write_fn(&mut out, f);
	}
}

fn ordered_union<T: Clone + std::hash::Hash + Eq>(a: Vec<T>, b: Vec<T>) -> Vec<T> {
	let in_a = HashSet::<_>::from_iter(a.iter().cloned());
	let in_b = HashSet::<_>::from_iter(b.iter().cloned());
	let mut a = a.into_iter();
	let mut b = b.into_iter();
	let mut out = Vec::<T>::new();
	while let Some(a) = a.next() {
		if in_b.contains(&a) {
			while let Some(b) = b.next() {
				if b == a {
					break
				}
				if !in_a.contains(&b) {
					out.push(b);
				}
			}
		}
		out.push(a);
	}
	out.extend(b.filter(|b| !in_a.contains(b)));
	out
}

fn write_fn(out: &mut ingert::Write, f: &ingert::Function) {
	write!(out, "function {}(", f.name);
	for (i, arg) in f.args.iter().enumerate() {
		if i != 0 {
			write!(out, ", ");
		}
		write!(out, "{}", arg);
	}
	writeln!(out, ")");
	if f.dup {
		writeln!(out, " (dup)");
	}
	struct Block<'a>(&'a [ingert::decompile::Stmt]);
	impl std::fmt::Display for Block<'_> {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			ingert::decompile::Stmt::display_block(self.0, f, 1)
		}
	}
	writeln!(out, "{}", Block(&f.body));
}
