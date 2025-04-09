use clap::Parser;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
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

	let writer = StandardStream::stderr(ColorChoice::Always);
	let config = codespan_reporting::term::Config::default();

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

		let str = ingert::print::print(&scena);
		std::fs::write("system.ing", &str).unwrap();

		let (tokens, errors) = ingert::parse::lex::lex(&str);
		dbg!(errors);
		let (_, errors) = ingert::parse::parse(&tokens);

		let mut errors = errors.errors;
		let file = SimpleFile::new("system.ing", &str);
		errors.sort_by_key(|e| e.sort_key());

		for error in errors {
			let severity = match error.severity {
				ingert::parse::error::Severity::Fatal => codespan_reporting::diagnostic::Severity::Error,
				ingert::parse::error::Severity::Error => codespan_reporting::diagnostic::Severity::Error,
				ingert::parse::error::Severity::Warning => codespan_reporting::diagnostic::Severity::Warning,
				ingert::parse::error::Severity::Info => codespan_reporting::diagnostic::Severity::Note,
			};
			let diag = Diagnostic::<()>::new(severity)
				.with_message(&error.main.desc)
				.with_label(Label::primary((), error.main.span.clone()).with_message(&error.main.desc))
				.with_labels_iter(error.notes.iter().map(|note| {
					Label::secondary((), note.span.clone()).with_message(&note.desc)
				}));
			codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &diag).unwrap();
		}


		let scp2 = ingert::scena::compile(scena).unwrap();
		similar_asserts::assert_eq!(scp, scp2);
	}
}
