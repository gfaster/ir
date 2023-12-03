use std::{ops::Range, collections::{HashMap, BTreeMap}};

use crate::{OpInner, Routine, Target, unique_label, Val, GlobalData, Op};

struct IdentMap(HashMap<Box<str>, (usize, bool)>, usize);
impl IdentMap {
    fn new() -> Self {
        Self(HashMap::new(), 0)
    }
    fn lookup(&self, ident: impl AsRef<str>) -> Option<usize> {
        self.0.get(ident.as_ref()).map(|&(i, _)| i)
    }
    fn lookup_or_declare(&mut self, ident: impl AsRef<str>) -> usize {
        self.0.entry(ident.as_ref().into()).or_insert_with(|| {
            let prev = self.1;
            self.1 += 1;
            (prev, false)
        }).0
    }
    fn assign(&mut self, ident: impl AsRef<str>) -> usize {
        self.0.entry(ident.as_ref().into()).and_modify(|(_, assigned)| {
            if *assigned {
                panic!("cannot assign twice to identifier {}", ident.as_ref())
            }
        }).or_insert_with(|| {
            let prev = self.1;
            self.1 += 1;
            (prev, true)
        }).0
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokenKind {
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    Ident,
    OpenSquare,
    CloseSquare,
    Comma,
    Assign,
    Define,
    Keyword,
    Literal,
    Quote,
}

impl TokenKind {
    fn try_match(self, data: &[u8]) -> Option<(&[u8], &[u8], &[u8])> {
        let start_off = data.iter().take_while(|&&b| b.is_ascii_whitespace()).count();
        let start = &data[0..start_off];
        if start.contains(&b'\n') {
            return None;
        }
        let data = &data[start_off..];
        let ident_char = |&b: &&u8| !b.is_ascii_whitespace() && ![b'(', b')', b',', b'=', b'{', b'}'].contains(b);
        let match_char = |ch: u8| (*data.get(0)? == ch).then_some(1);
        let match_str = |st: &[u8]| {
            data.starts_with(st).then_some(st.len())
        };
        let match_len = match self {
            TokenKind::OpenCurly => {
                match_char(b'{')?
            },
            TokenKind::CloseCurly => {
                match_char(b'}')?
            },
            TokenKind::OpenParen => {
                match_char(b'(')?
            },
            TokenKind::CloseParen => {
                match_char(b')')?
            },
            TokenKind::Ident => {
                data.get(0).is_some_and(|b| ident_char(&b)).then_some(())?;
                data[1..].iter().take_while(ident_char).count() + 1
            },
            TokenKind::OpenSquare => {
                match_char(b'[')?
            },
            TokenKind::CloseSquare => {
                match_char(b']')?
            },
            TokenKind::Comma => {
                match_char(b',')?
            },
            TokenKind::Define => {
                match_str(b"define")?
            },
            TokenKind::Assign => {
                match_char(b'=')?
            },
            TokenKind::Literal => {
                // eprintln!("{}", String::from_utf8_lossy(&data.iter().copied().take(5).collect::<Vec<u8>>()));
                data.first().is_some_and(|&b| b.is_ascii_digit()).then_some(())?;
                data.iter().take_while(|b| b.is_ascii_digit()).count()
            },
            TokenKind::Quote => {
                match_char(b'"')?;
                data[1..].iter().take_while(|&&b| b != b'"').count() + 2
            },
            TokenKind::Keyword => panic!("cannot try_match against a keyword, use try_match_str"),
        };

        if data[..match_len].contains(&b'\n') {
            return None;
        }

        Some((start, &data[..match_len], &data[match_len..]))
    }
}

fn try_match_str<'a>(data: &'a [u8], s: &'_ str) -> Option<(&'a [u8], &'a [u8], &'a [u8])> {
    let start_off = data.iter().take_while(|&&b| b.is_ascii_whitespace()).count();
    let start = &data[0..start_off];
    let data = &data[start_off..];
    let match_len = data.starts_with(s.as_bytes()).then_some(s.len())?;
    Some((start, &data[..match_len], &data[match_len..]))
}

fn ident_is_keyword(data: &[u8], keys: &[&str]) -> Option<usize> {
    keys.iter().position(|&k| data == k.as_bytes())
}

enum OpType {
    Add,
    Jmp,
    Br,
    Load,
    Label,
    Global,
    Call,
}

impl OpType {
    fn is_op_key(tok: &Token, data: &str) -> Option<Self> {
        macro_rules! op_match {
            ($matcher:expr => {$($idt:ident),* $(,)?}) => {
                {
                    let data: &[u8] = $matcher;
                    if !data.is_empty() && data[0].is_ascii_lowercase() {
                        'ret: {
                            if let Some((first, rest)) = data.split_first() {
                                $(
                                if first.to_ascii_uppercase() == stringify!($idt).as_bytes()[0] 
                                && rest == stringify!($idt)[1..].as_bytes() {
                                    break 'ret Some(OpType::$idt)
                                })*
                            }
                            None
                        }
                    } else {
                        None
                    }
                }
            };
        }
        op_match!(data[tok.span.clone()].as_bytes() => {Add, Br, Jmp, Load, Global, Label, Call})
    }
}

macro_rules! parse_panic {
    () => {
        compile_error!("pass a parser as the first argument")
    };
    ($parser:expr) => {
        parse_panic!($parser, "Generic parsing error")
    };
    ($parser:expr, $($fmt:tt)*) => {
        {
            let parser: &Parser = $parser;
            parser.parse_panic(|| format!($($fmt)*));
        }
    };
}

enum Statement {
    Op(OpInner),
    Global(GlobalData)
}

impl From<OpInner> for Statement {
    fn from(v: OpInner) -> Self {
        Self::Op(v)
    }
}

#[derive(Clone)]
struct Token {
    kind: TokenKind,
    span: Range<usize>
}

pub(crate) struct Parser {
    file: Option<std::path::PathBuf>,
    line: usize,
    buf: String,
    off: usize,
}

impl Parser {
    pub fn new_file(file: impl AsRef<std::path::Path>) -> std::io::Result<Self> {
        let name = file.as_ref().to_owned();
        let buf = std::fs::read_to_string(&name)?;
        Ok(
            Self {
                file: Some(name),
                line: 1,
                buf,
                off: 0,
            }
        )
    }

    fn peek_next_token_any(&self) -> &str {
        let data = &self.buf[self.off..];
        let off = data.bytes().take_while(|b| b.is_ascii_whitespace()).count();
        if data[..off].as_bytes().contains(&b'\n') {
            return "[[EOL]]";
        }
        let data = &data[off..];
        let Some((start, ch)) = data.char_indices().next() else {
            return "[[empty input]]";
        };
        let first_byte = ch as u32 as u8;
        if [b'{', b'}', b'[', b']', b'(', b')'].contains(&first_byte) {
            &data[..1]
        } else {
            let end = data.bytes().take_while(|b| b.is_ascii_alphabetic() || [b'.', b':'].contains(b)).count();
            &data[..end]
        }
    }

    #[track_caller]
    fn parse_panic<'a, F, D>(&self, msg: F) -> !
    where 
        F: FnOnce() -> D,
        D: std::fmt::Display
    {
        let file_name = self.file.as_ref().map(|f| f.display().to_string()).unwrap_or_else(|| "anonymous file".into());
        let line = self.line;
        panic!("Error reading file {file_name} on line {line}:\n\t\
            {}", msg())
    }

    fn maybe(&mut self, kind: TokenKind) -> Option<Token> {
        let (pre, tok, rem) = kind.try_match(&self.buf[self.off..].as_bytes())?;
        let span = (self.off + pre.len())..(self.off + pre.len() + tok.len());
        let new_off = self.off + pre.len() + tok.len();
        self.line += self.buf[self.off..new_off].as_bytes().iter().filter(|&&b| b == b'\n').count();
        self.off = new_off;
        // eprintln!("read token {kind:?}: {:?}", &self.buf[span.clone()]);
        Some( Token { kind, span } )
    }

    fn maybe_keyword(&mut self, key: &str) -> Option<Token> {
        let (pre, tok, rem) = try_match_str(&self.buf[self.off..].as_bytes(), key)?;
        let span = (self.off + pre.len())..(self.off + pre.len() + tok.len());
        let new_off = self.off + pre.len() + tok.len();
        self.line += self.buf[self.off..new_off].as_bytes().iter().filter(|&&b| b == b'\n').count();
        self.off = new_off;
        Some( Token { kind: TokenKind::Keyword, span } )
    }

    #[track_caller]
    fn expect(&mut self, kind: TokenKind) -> Token {
        let Some(tok) = self.maybe(kind) else {
            parse_panic!(self, "expected {kind:?}, found {next:?}",
                next = self.peek_next_token_any());
        };
        tok
    }

    fn maybe_eol(&mut self) -> Option<()> {
        let Some(off) = self.buf[self.off..]
            .bytes()
            .take_while(|b| b.is_ascii_whitespace())
            .position(|b| b == b'\n') else {
            return None;
        };
        self.off += off + 1;
        self.line += 1;
        Some(())
    }

    #[track_caller]
    fn expect_eol(&mut self) {
        if self.maybe_eol().is_none() {
            parse_panic!(self, "expected EOL, found {next:?}",
                next = self.peek_next_token_any());
        };
    }

    #[track_caller]
    fn expect_keyword(&mut self, key: &str) -> Token {
        let Some(tok) = self.maybe_keyword(key) else {
            parse_panic!(self, "expected keyword {key:?}, found {next:?}",
                next = self.peek_next_token_any());
        };
        tok
    }

    fn maybe_set(&mut self, kinds: &[TokenKind]) -> Option<Token> {
        for &kind in kinds {
            let tok = self.maybe(kind);
            if tok.is_some() {
                return tok;
            }
        }
        None
    }

    fn maybe_keyword_set(&mut self, keys: &[&str]) -> Option<Token> {
        for &key in keys {
            let tok = self.maybe_keyword(key);
            if tok.is_some() {
                return tok;
            }
        }
        None
    }

    #[track_caller]
    fn expect_set(&mut self, kinds: &[TokenKind]) -> Token {
        let Some(tok) = self.maybe_set(kinds) else {
            parse_panic!(self, "expected one of {kinds:?}, found {next:?}",
                next = self.peek_next_token_any());
        };
        tok
    }

    #[track_caller]
    fn expect_keyword_set(&mut self, keys: &[&str]) -> Token {
        let Some(tok) = self.maybe_keyword_set(keys) else {
            parse_panic!(self, "expected one of {keys:?} keywords, found {next:?}",
                next = self.peek_next_token_any());
        };
        tok
    }

    fn token_str<'a>(&'a self, tok: &'_ Token) -> &'a str {
        &self.buf[tok.span.clone()]
    }

    fn maybe_val(&mut self, idents: &mut IdentMap) -> Option<Val> {
        if let Some(tok) = self.maybe(TokenKind::Literal) {
            let lit: u64 = self.token_str(&tok).parse().expect("checked for ascii");
            Some(Val::Literal(lit))
        } else if let Some(tok) = self.maybe_keyword("label") {
            let tok = self.maybe(TokenKind::Ident)?;
            let id = idents.lookup_or_declare(self.token_str(&tok));
            Some(Val::GlobalLabel(id))
        } else {
            None
        }
    }

    #[track_caller]
    fn expect_val(&mut self, idents: &mut IdentMap) -> Val {
        let Some(val) = self.maybe_val(idents) else {
            parse_panic!(self, "expected a value, found {next:?}",
                next = self.peek_next_token_any());
        };
        val
    }

    fn next_line(&mut self) {
        let Some(off) = self.buf[self.off..].bytes().position(|b| b == b'\n') else {
            self.off = self.buf.len();
            return;
        };
        self.off += off + 1;
    }

    pub fn parse(&mut self) -> (Routine, BTreeMap<usize, GlobalData>) {
        let mut idents = IdentMap::new();
        self.expect(TokenKind::Define);
        let name_tok = self.expect(TokenKind::Ident);
        let name: String = self.buf[name_tok.span].into();
        let args = self.parse_args_assign(&mut idents);
        assert_eq!(args.len(), 0, "for now routines can't take arguments");
        self.expect(TokenKind::OpenCurly);
        self.expect_eol();
        let mut ops = Vec::new();
        let mut globals = Vec::new();
        while let Some(tok) = self.maybe(TokenKind::Ident) {
            let cf_keys = &["br", "jmp"];
            let tok_str = &self.buf[tok.span.clone()];
            if let Some(op) = OpType::is_op_key(&tok, &self.buf) {
                match self.parse_op(&mut idents, op, None) {
                    Statement::Op(op) => ops.push(Op {
                        inner: op,
                        fileno: 0,
                        line: self.line as u32,
                    }),
                    Statement::Global(global) => {
                        globals.push(global)
                    },
                }
            } else {
                let id = idents.assign(tok_str);
                self.expect(TokenKind::Assign);
                let tok = self.expect(TokenKind::Ident);
                let Some(op_type) = OpType::is_op_key(&tok, &self.buf) else {
                    parse_panic!(self, "expected operation");
                };
                match self.parse_op(&mut idents, op_type, Some(id)) {
                    Statement::Op(op) => ops.push(Op {
                        inner: op,
                        fileno: 0,
                        line: self.line as u32,
                    }),
                    Statement::Global(_) => parse_panic!(self, "should only be able to have globals without assign"),
                }
            }
            self.expect_eol();
        }
        self.expect(TokenKind::CloseCurly);
        let globals = globals.into_iter().map(|g| (idents.lookup(&g.name).expect("previously declared"), g)).collect();
        (Routine { name: name.into(), ops }, globals)
    }

    fn lookup_ident(&self, idents: &IdentMap, tok: &Token) -> usize {
        let tok_str = &self.buf[tok.span.clone()];
        let Some(id) = idents.lookup(tok_str) else {
            parse_panic!(self, "identifier {tok_str:?} is undeclared");
        };
        id
    }

    #[track_caller]
    fn expect_dst(&self, dst: Option<usize>) -> usize {
        let Some(dst) = dst else {
            parse_panic!(self, "argument requires destination");
        };
        dst
    }

    fn ident_id(&mut self, idents: &IdentMap) -> usize {
        let tok = self.expect(TokenKind::Ident);
        self.lookup_ident(idents, &tok)
    }

    fn ident_id_decl(&mut self, idents: &mut IdentMap) -> usize {
        let tok = self.expect(TokenKind::Ident);
        idents.lookup_or_declare(&self.buf[tok.span])
    }

    fn ident_id_assign(&mut self, idents: &mut IdentMap) -> usize {
        let tok = self.expect(TokenKind::Ident);
        idents.assign(&self.buf[tok.span])
    }

    fn parse_op(&mut self, idents: &mut IdentMap, op_type: OpType, dst: Option<usize>) -> Statement {
        match op_type {
            OpType::Add => {
                let op1 = self.ident_id(idents);
                self.expect(TokenKind::Comma);
                let op2 = self.ident_id(idents);
                let dst = self.expect_dst(dst);
                OpInner::Add { dst, op1, op2 }
            },
            OpType::Jmp => {
                self.expect_keyword("label");
                let id = self.ident_id_decl(idents);
                let args = self.parse_args(idents);
                OpInner::Jmp { target: Target { id, args } }
            },
            OpType::Br => {
                let lhs = self.ident_id(idents);
                self.expect(TokenKind::Comma);
                let rhs = self.ident_id(idents);
                self.expect(TokenKind::Comma);
                self.expect_keyword("label");
                let success = Target {
                    id: self.ident_id_decl(idents),
                    args: self.parse_args(idents),
                };
                self.expect(TokenKind::Comma);
                self.expect_keyword("label");
                let fail = Target {
                    id: self.ident_id_decl(idents),
                    args: self.parse_args(idents),
                };
                OpInner::Bne { check: (lhs, rhs), success, fail }
            },
            OpType::Load => {
                let loc = self.ident_id_assign(idents);
                self.expect(TokenKind::Comma);
                let val = self.expect_val(idents);
                OpInner::Load { loc, val }
            },
            OpType::Label => {
                let id = self.ident_id_assign(idents);
                let args = self.parse_args_assign(idents);
                OpInner::Block { id, args }
            },
            OpType::Global => {
                let tok = self.expect(TokenKind::Ident);
                let name = self.token_str(&tok).into();
                self.expect(TokenKind::Assign);
                let data = self.parse_global().into();
                return Statement::Global(GlobalData { name, data })
            },
            OpType::Call => {
                self.expect_keyword("syscall");
                let args = self.parse_args(idents);
                OpInner::Call { id: 0, args }
            },
        }.into()
    }

    fn parse_global(&mut self) -> Vec<u8> {
        let tok = self.expect_set(&[TokenKind::OpenSquare, TokenKind::Quote]);
        if tok.kind == TokenKind::Quote {
            let mut ret = Vec::with_capacity(tok.span.len() - 2);
            let mut skip_next = false;
            for w in self.buf[tok.span.start..(tok.span.end - 1)].as_bytes().windows(2) {
                if skip_next {
                    skip_next = false;
                    continue;
                }
                if w[0] == b'\\' {
                    match w[1] {
                        b'n' => ret.push(b'\n'),
                        b'r' => ret.push(b'\r'),
                        b'\\' => {
                            skip_next = true;
                            ret.push(b'\\');
                        },
                        _ => parse_panic!(self, "unknown escape \\{:}", w[1])
                    }
                } else {
                    if w[1] != b'\\' {
                        ret.push(w[1]);
                    }
                }
            }
            return ret
        } else {
            todo!()
        }
    }

    fn parse_args(&mut self, idents: &IdentMap) -> Vec<usize> {
        self.expect(TokenKind::OpenParen);
        let mut ret = Vec::new();
        while let Some(tok) = self.maybe(TokenKind::Ident) {
            ret.push(self.lookup_ident(idents, &tok));
            if self.maybe(TokenKind::Comma).is_none() {
                break;
            }
        };
        self.expect(TokenKind::CloseParen);
        ret
    }

    fn parse_args_assign(&mut self, idents: &mut IdentMap) -> Vec<usize> {
        self.expect(TokenKind::OpenParen);
        let mut ret = Vec::new();
        while let Some(tok) = self.maybe(TokenKind::Ident) {
            let id = idents.assign(self.token_str(&tok));
            ret.push(id);
            if self.maybe(TokenKind::Comma).is_none() {
                break;
            }
        };
        self.expect(TokenKind::CloseParen);
        ret
    }

}
