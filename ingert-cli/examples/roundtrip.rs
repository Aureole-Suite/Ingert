use clap::Parser;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use ingert::scena::{DecompileMode, DecompileOptions};
use ingert_syntax::diag;
use rayon::prelude::*;
use similar_asserts::SimpleDiff;
use std::path::{Path, PathBuf};
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

	args.files.par_iter().for_each(|file| {
		let _span = tracing::info_span!("file", path = %file.display()).entered();
		let data = std::fs::read(file).unwrap();

		let scp = match ingert::scp::read(&data) {
			Ok(scp) => scp,
			Err(e) => {
				tracing::error!("Error reading file: {e}");
				return;
			}
		};
		let data2 = ingert::scp::write(&scp).unwrap();

		if data != data2 {
			tracing::error!("Scp did not roundtrip");
		}

		check_roundtrip(file, scp.clone(), DecompileMode::Asm, false);
		check_roundtrip(file, scp.clone(), DecompileMode::Flat, false);
		check_roundtrip(file, scp.clone(), DecompileMode::Tree, false);
		check_roundtrip(file, scp.clone(), DecompileMode::Asm, true);
		check_roundtrip(file, scp.clone(), DecompileMode::Flat, true);
		check_roundtrip(file, scp.clone(), DecompileMode::Tree, true);
	});
}

fn check_roundtrip(file: &Path, scp: ingert::scp::Scp, mode: DecompileMode, called: bool) {
	let _span = tracing::info_span!("roundtrip", ?mode, called).entered();

	let mut scena = ingert::scena::from_scp(scp.clone());
	ingert::scena::decompile(&mut scena, &DecompileOptions { mode, called });

	match ingert::scena::compile(scena.clone()) {
		Ok(scp2) => {
			if scp != scp2 {
				tracing::error!("Decompile did not roundtrip\n{}", SimpleDiff::from_str(
					&format!("{scp:#?}"),
					&format!("{scp2:#?}"),
					"original",
					"recompiled",
				));
			}
		}
		Err(e) => {
			tracing::error!("Error recompiling scena: {e}");
			return;
		}
	}

	let str = ingert_syntax::print::print(&scena);

	let mut errors = diag::Errors::new();
	let tokens = ingert_syntax::lex::lex(&str, &mut errors);
	if errors.max_severity() >= diag::Severity::Fatal {
		print_errors(file, &errors, &str);
		return;
	}
	let scena2 = ingert_syntax::parse::parse(&tokens, &mut errors);
	print_errors(file, &errors, &str);

	if scena != scena2 {
		tracing::error!("Parsing did not roundtrip\n{}", SimpleDiff::from_str(
			&format!("{scena:#?}"),
			&format!("{scena2:#?}"),
			"original",
			"parsed",
		));
	}
}

fn print_errors(path: &Path, errors: &diag::Errors, str: &str) {
	let writer = StandardStream::stderr(ColorChoice::Always);
	let config = codespan_reporting::term::Config::default();
	let file = SimpleFile::new(path.display().to_string(), &str);

	let mut errors = errors.errors.iter().collect::<Vec<_>>();
	errors.sort_by(|a, b| a.sort_key().cmp(&b.sort_key()));

	for error in &errors {
		let diag = to_diagnostic(error);
		codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &diag).unwrap();
	}
}

fn to_diagnostic(error: &diag::Diagnostic) -> Diagnostic<()> {
	let severity = match error.severity {
		diag::Severity::Fatal => codespan_reporting::diagnostic::Severity::Error,
		diag::Severity::Error => codespan_reporting::diagnostic::Severity::Error,
		diag::Severity::Warning => codespan_reporting::diagnostic::Severity::Warning,
		diag::Severity::Info => codespan_reporting::diagnostic::Severity::Note,
	};
	Diagnostic::new(severity)
		.with_message(&error.main.desc)
		.with_label(Label::primary((), error.main.span.clone()).with_message(&error.main.desc))
		.with_labels_iter(error.notes.iter().map(|note| {
			Label::secondary((), note.span.clone()).with_message(&note.desc)
		}))
}
