pub mod lex;
pub mod error;

pub fn parse(tokens: &lex::Tokens) {
	let mut cursor = tokens.cursor();
	dbg!(&cursor);
}
