#![feature(let_chains)]
use clap::Parser;
use std::path::PathBuf;
use tracing_subscriber::prelude::*;

#[derive(clap::Parser)]
struct Args {
	files: Vec<PathBuf>,
}

fn main() {
	tracing_subscriber::registry()
		.with(tracing_subscriber::fmt::layer().with_writer(std::io::stderr))
		.with(tracing_subscriber::EnvFilter::from_default_env())
		.init();
	unsafe { compact_debug::enable(true) };
	let args = Args::parse();

	for file in &args.files {
		let _span = tracing::info_span!("file", path = %file.display()).entered();
		if file.ends_with("mon9996_c00.da") {
			// this one has a very different format, where the first table is half the width
			tracing::info!("Skipping mon9996_c00.da");
			continue;
		}
		let data = std::fs::read(file).unwrap();

		let scp = ingert::scp::read(&data).unwrap();
		let data2 = ingert::scp::write(&scp).unwrap();
		similar_asserts::assert_eq!(data, data2);

		let mut scena = ingert::scena::from_scp(scp.clone());
		ingert::scena::decompile(&mut scena);
	}
}
