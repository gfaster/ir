//! I need to think about how this is supposed to work. I need some way to transform all of the
//! following situations:
//!
//! ```txt
//! %1 = 5
//! %2 = add i64 %1 %other
//! to
//! %2 = add i64 5 %other
//! ```
//! Q: can a binding be an immediate?
//!
//! A: it's clear that an immediate can't be an lvalue, but otherwise it should be ok. It will make
//! folding checks much easier, but can potentially complicate legalization
//!
//! ```txt
//! %1 = 5
//! %2 = add i64 %1 %1
//! to
//! %2 = 10
//! ```
