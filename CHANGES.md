# 1.1.0

- Make partial decompile failure a warning, not an error
- Remove \x escape, add \u instead
- Add --roundtrip flag, which is more conservative about what passes it applies
- Fix some issues with unnecessary braces, and a few roundtrips

# 1.0.2

- Allow large pops, caused by functions with many locals

# 1.0.1

- Fix panic if failing to make sense of called table
- Print floats in a consistent format (no scientific notation)
- Sink line numbers in system, debug, and tailcall expressions

# 1.0.0

- Initial release
