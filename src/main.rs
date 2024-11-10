#![feature(let_chains)]
use clap::Parser;
use std::path::{Path, PathBuf};

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

		let printed = ingert::print::print(&scena, ingert::print::Settings::default());
		writeln!(out, "{printed}");

		// println!("{:#?}", ingert::scp::parse_scp(&data));
	}

	let out = Path::new("out").join("prelude.da");
	let out = std::fs::File::create(&out).unwrap();
	let out = std::io::BufWriter::new(out);
	let mut out = ingert::Write(Box::new(out));
	let printed = ingert::print::print(&prelude.into_scena(), ingert::print::Settings::default());
	writeln!(out, "{printed}");
}
