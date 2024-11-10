#![feature(let_chains)]
use clap::Parser;
use std::path::{Path, PathBuf};
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(clap::Parser)]
struct Args {
	files: Vec<PathBuf>,
}

fn main() {
	tracing_subscriber::fmt::fmt()
		.with_writer(std::io::stderr)
		.init();
	unsafe { compact_debug::enable(true) };
	let args = Args::parse();

	let mut prelude = ingert::prelude::Prelude::new();
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

		let mut scena = ingert::decompile(&data).unwrap();
		let out = Path::new("out").join(file.file_name().unwrap());
		let out = std::fs::File::create(&out).unwrap();
		let out = std::io::BufWriter::new(out);
		let mut out = ingert::Write(Box::new(out));

		prelude.add(&mut scena);

		// for item in &scena {
		// 	match item {
		// 		ingert::Item::Global(g) => {
		// 			if let Some(line) = g.line {
		// 				write!(out, "line {} ", line);
		// 			}
		// 			writeln!(out, "global {} = {}", g.name, g.ty);
		// 		}
		// 		ingert::Item::Function(f) => {
		// 			if f.is_prelude {
		// 				write!(out, "prelude ");
		// 			}
		// 			write_fn(&mut out, f);
		// 		}
		// 	}
		// }

		let printed = ingert::print::print(&scena, ingert::print::Settings::default());

		writeln!(out, "{printed}");

		// println!("{:#?}", ingert::scp::parse_scp(&data));
	}

	let out = Path::new("out").join("prelude.da");
	let out = std::fs::File::create(&out).unwrap();
	let out = std::io::BufWriter::new(out);
	let mut out = ingert::Write(Box::new(out));
	for f in prelude.into_scena() {
		if let ingert::Item::Function(f) = f {
			write_fn(&mut out, &f);
		}
	}
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
