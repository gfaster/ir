use std::{
    collections::{BTreeMap, HashMap},
    ops::Range,
    rc::Rc, cell::Cell,
};

use crate::{unique_label, GlobalData, Op, OpInner, Routine, Target, Val, Loc};

use self::rules::{collect_decl_args, Rule};

struct IdentMap(HashMap<Box<str>, (usize, bool)>, usize);
impl IdentMap {
    fn new() -> Self {
        Self(HashMap::new(), 0)
    }
    fn try_lookup(&self, ident: impl AsRef<str>) -> Option<usize> {
        self.0.get(ident.as_ref()).map(|&(i, _)| i)
    }
    fn lookup(&self, ident: impl AsRef<str>) -> usize {
        let Some(ret) = self.0.get(ident.as_ref()) else {
            panic!("identifier {} undeclared", ident.as_ref());
        };
        ret.0
    }
    fn lookup_or_declare(&mut self, ident: impl AsRef<str>) -> usize {
        self.0
            .entry(ident.as_ref().into())
            .or_insert_with(|| {
                let prev = self.1;
                self.1 += 1;
                (prev, false)
            })
            .0
    }
    fn assign(&mut self, ident: impl AsRef<str>) -> usize {
        self.0
            .entry(ident.as_ref().into())
            .and_modify(|(_, assigned)| {
                if *assigned {
                    panic!("cannot assign twice to identifier {}", ident.as_ref())
                }
                *assigned = true;
            })
            .or_insert_with(|| {
                let prev = self.1;
                self.1 += 1;
                (prev, true)
            })
            .0
    }
    fn assert_all_initialized<T>(&self, globals: &BTreeMap<usize, T>) {
        for (key, val) in self.0.iter() {
            if !val.1 && !globals.contains_key(&val.0) {
                panic!("identifier {key:?} was never initialized")
            }
        }
    }
    fn invert(self) -> BTreeMap<usize, Box<str>> {
        self.0.into_iter().map(|(k, (v, _init))| (v, k)).collect()
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
        let start_off = data
            .iter()
            .take_while(|&&b| b.is_ascii_whitespace())
            .count();
        let start = &data[0..start_off];
        if start.contains(&b'\n') {
            return None;
        }
        let data = &data[start_off..];
        let ident_char = |&b: &&u8| {
            !b.is_ascii_whitespace() && ![b'(', b')', b',', b'=', b'{', b'}'].contains(b)
        };
        let match_char = |ch: u8| (*data.get(0)? == ch).then_some(1);
        let match_str = |st: &[u8]| data.starts_with(st).then_some(st.len());
        let match_len = match self {
            TokenKind::OpenCurly => match_char(b'{')?,
            TokenKind::CloseCurly => match_char(b'}')?,
            TokenKind::OpenParen => match_char(b'(')?,
            TokenKind::CloseParen => match_char(b')')?,
            TokenKind::Ident => {
                data.get(0).is_some_and(|b| ident_char(&b)).then_some(())?;
                data[1..].iter().take_while(ident_char).count() + 1
            }
            TokenKind::OpenSquare => match_char(b'[')?,
            TokenKind::CloseSquare => match_char(b']')?,
            TokenKind::Comma => match_char(b',')?,
            TokenKind::Define => match_str(b"define")?,
            TokenKind::Assign => match_str(b":=")?,
            TokenKind::Literal => {
                // eprintln!("{}", String::from_utf8_lossy(&data.iter().copied().take(5).collect::<Vec<u8>>()));
                data.first()
                    .is_some_and(|&b| b.is_ascii_digit())
                    .then_some(())?;
                data.iter().take_while(|b| b.is_ascii_digit()).count()
            }
            TokenKind::Quote => {
                match_char(b'"')?;
                data[1..].iter().take_while(|&&b| b != b'"').count() + 2
            }
            TokenKind::Keyword => panic!("cannot try_match against a keyword, use try_match_str"),
        };

        if data[..match_len].contains(&b'\n') {
            return None;
        }

        Some((start, &data[..match_len], &data[match_len..]))
    }
}

fn try_match_str<'a>(data: &'a [u8], s: &'_ str) -> Option<(&'a [u8], &'a [u8], &'a [u8])> {
    let start_off = data
        .iter()
        .take_while(|&&b| b.is_ascii_whitespace())
        .count();
    let start = &data[0..start_off];
    let data = &data[start_off..];
    let match_len = data.starts_with(s.as_bytes()).then_some(s.len())?;
    Some((start, &data[..match_len], &data[match_len..]))
}

fn ident_is_keyword(data: &[u8], keys: &[&str]) -> Option<usize> {
    keys.iter().position(|&k| data == k.as_bytes())
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

mod rules {
    use std::fmt::{Debug, Display};

    type Ob<T> = Option<Box<T>>;

    const VALID_IDENT: &'static [u8] =
        b"1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM._";

    pub trait Rule<'a>: Sized + Debug {
        type Output: Rule<'a>;
        fn try_parse(input: &'a str) -> Option<(Self::Output, &'a str)>;
        fn name() -> &'static str {
            std::any::type_name::<Self>()
        }
        // fn parse(input: &'a str) -> Option<(Self::Output, &'a str)>;
    }

    impl<'a, T: Rule<'a>> Rule<'a> for Option<T> {
        type Output = Option<<T as Rule<'a>>::Output>;

        fn try_parse(input: &'a str) -> Option<(Self::Output, &'a str)> {
            Some(T::try_parse(input).map_or((None, input), |(r, s)| (Some(r), s)))
        }
    }

    impl<'a, T: Rule<'a>> Rule<'a> for Box<T> {
        type Output = Box<<T as Rule<'a>>::Output>;

        fn try_parse(input: &'a str) -> Option<(Self::Output, &'a str)> {
            T::try_parse(input).map(|(r, s)| (Box::new(r), s))
        }
    }

    fn trim_amt(s: &str) -> (usize, &str) {
        let Some((amt, _)) = s
            .char_indices()
            .take_while(|(_, c)| c.is_whitespace() && *c != '\n')
            .last()
        else {
            return (0, s);
        };
        (amt, &s[amt..])
    }

    fn trim_nolf(s: &str) -> &str {
        s.trim_start()
        // let Some((amt, _)) = s.char_indices().take_while(|(_, c)| c.is_whitespace() && *c != '\n').last() else {
        //     return s;
        // };
        // &s[amt..]
    }

    #[derive(PartialEq, Eq)]
    struct Span<'a>(&'a str);
    impl Debug for Span<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            Debug::fmt(&self.0, f)
        }
    }

    impl AsRef<str> for Span<'_> {
        fn as_ref(&self) -> &str {
            self.0
        }
    }

    macro_rules! keyword {
        ($($name:ident($val:literal));* $(;)?) => {
            $(
            #[doc = concat!("literal '", $val, "' token")]
            #[derive(PartialEq, Eq, Debug)]
            pub struct $name<'a> (Span<'a>);
            impl<'a> Rule<'a> for $name<'a> {
                type Output = Self;
                fn try_parse(input: &'a str) -> Option<(Self::Output, &'a str)> {
                    let s = trim_nolf(input);
                    s.starts_with($val).then(|| (Self(Span(&s[..$val.len()])), &s[$val.len()..]))
                }
                fn name() -> &'static str {
                    concat!("'", $val, "'")
                }
            }
            impl std::default::Default for $name<'_> {
                fn default() -> Self {
                    Self(Span($val))
                }
            }
            )*
        };
    }


    fn parse_mut<'a, T: Rule<'a>>(s: &'_ mut &'a str) -> Option<<T as Rule<'a>>::Output> {
        let x = T::try_parse(*s)?;
        *s = x.1;
        Some(x.0)
    }

    macro_rules! impl_rules_struct {
        ($(struct $name:ident<$lf:lifetime> ($($field_ty:ty),*$(,)?));* $(;)?) => {
            $(impl_rules_struct!{@inner $name<$lf> ($($field_ty,)*)})*
        };
        (@inner $name:ident<$lf:lifetime> ($($field_ty:ty),*$(,)?)) => {
            #[derive(PartialEq, Eq, Debug)]
            pub struct $name<$lf> ($(pub $field_ty),*);
            impl<'a> Rule<'a> for $name<'a> {
                type Output = Self;
                fn try_parse(mut s: &'a str) -> Option<(Self, &'a str)> {
                    Some(($name (
                    $(
                        parse_mut::<$field_ty>(&mut s)?.into(),
                    )*
                    ), s))
                }
            }
        };
    }
    macro_rules! impl_rules_enum {
        ($(enum $name:ident<$lf:lifetime> {$($variant:ident($($field_ty:ty),* $(,)?)),*$(,)?})*) => {
            $(impl_rules_enum!{@inner $name<$lf> {$($variant ($($field_ty,)*)),*}})*
        };
        (@attempt_closure $str:ident $variant:ident $($field_ty:ty)*) => {
            || -> Option<(Self, &str)> {
                let mut c = $str;
                Some((Self::$variant($(parse_mut::<$field_ty>(&mut c)?.into()),*), c))
            }
        };
        (@inner $name:ident<$lf:lifetime> {
                $first_var:ident ($($first_field_ty:ty),* $(,)?)
                $(, $variant:ident($($field_ty:ty),* $(,)?))*$(,)?
            }) => {
            #[derive(PartialEq, Eq, Debug)]
            pub enum $name<$lf> {
                $first_var($($first_field_ty),*),
                $($variant ($($field_ty),*)),*
            }
            impl<'a> Rule<'a> for $name<'a> {
                type Output = Self;
                fn try_parse(mut s: &'a str) -> Option<(Self, &'a str)> {

                    Some(impl_rules_enum!(@attempt_closure s $first_var $($first_field_ty)*)()
                        $(
                        .or_else(impl_rules_enum!(@attempt_closure s $variant $($field_ty)*))
                    )*?)
                }
            }
        };
    }

    macro_rules! decl_unit_rule {
        ($($name:ident),*) => {
            $(
            #[derive(PartialEq, Eq, Debug)]
            pub struct $name<'a>(Span<'a>);

            impl AsRef<str> for $name<'_> {
                fn as_ref(&self) -> &str {
                    self.0.0
                }
            })*
        };
    }

    impl<'a> Rule<'a> for Register<'a> {
        type Output = Self;

        fn try_parse(input: &'a str) -> Option<(Self::Output, &'a str)> {
            let s = trim_nolf(input);
            s.starts_with('%').then_some(())?;
            let off = s
                .bytes()
                .skip(1)
                .take_while(|b| VALID_IDENT.contains(b))
                .count()
                + 1;
            (off > 1).then_some(())?;
            Some((Self(Span(&s[..off])), &s[off..]))
        }
    }

    impl<'a> Rule<'a> for FnIdent<'a> {
        type Output = Self;

        fn try_parse(input: &'a str) -> Option<(Self::Output, &'a str)> {
            let s = trim_nolf(input);
            s.starts_with('@').then_some(())?;
            let off = s
                .bytes()
                .skip(1)
                .take_while(|b| VALID_IDENT.contains(b))
                .count()
                + 1;
            (off > 1).then_some(())?;
            Some((Self(Span(&s[..off])), &s[off..]))
        }
    }

    impl<'a> Rule<'a> for LabelIdent<'a> {
        type Output = Self;

        fn try_parse(input: &'a str) -> Option<(Self::Output, &'a str)> {
            let s = trim_nolf(input);
            let off = s.bytes().take_while(|b| VALID_IDENT.contains(b)).count();
            (off > 0).then_some(())?;
            Some((Self(Span(&s[..off])), &s[off..]))
        }
    }

    impl<'a> Rule<'a> for TypeQual<'a> {
        type Output = Self;

        fn try_parse(input: &'a str) -> Option<(Self::Output, &'a str)> {
            let s = trim_nolf(input);
            let off = "i64".len();
            s.starts_with("i64").then_some(())?;
            Some((Self(Span(&s[..off])), &s[off..]))
        }
    }

    impl<'a> Rule<'a> for StrLiteral<'a> {
        type Output = Self;

        fn try_parse(input: &'a str) -> Option<(Self::Output, &'a str)> {
            let s = trim_nolf(input);
            let len = (|| {
                s.starts_with('"').then_some(())?;
                let len = s.bytes().skip(1).take_while(|&b| b != b'"').count() + 1;
                s.as_bytes().get(len).filter(|&&b| b == b'"')?;
                Some(len + 1)
            })()?;
            Some((Self(Span(&s[..len])), &s[len..]))
        }
    }

    impl<'a> Rule<'a> for IntLiteral<'a> {
        type Output = Self;

        fn try_parse(input: &'a str) -> Option<(Self::Output, &'a str)> {
            let s = trim_nolf(input);
            s.bytes().next().filter(|b| b"0123456789".contains(b))?;
            let len = s.bytes().take_while(|b| !b.is_ascii_whitespace()).count();
            (len > 0).then_some(())?;
            s.bytes().take(len).all(|b| b"0123456789".contains(&b)).then_some(())?;
            Some((Self(Span(&s[..len])), &s[len..]))
        }
    }

    impl<'a> Rule<'a> for Op<'a> {
        type Output = Self;

        fn try_parse(input: &'a str) -> Option<(Self::Output, &'a str)> {
            let s = trim_nolf(input);
            let off = "add".len();
            s.starts_with("add").then_some(())?;
            Some((Self(Span(&s[..off])), &s[off..]))
        }
    }

    keyword! {
        OpenParen("(");
        CloseParen(")");
        OpenCurly("{");
        CloseCurly("}");
        Comma(",");
        Load("load");
        Label("label");
        Ptr("ptr");
        Jmp("jmp");
        Equals("=");
        Colon(":");
        Lf("\n");
        Define("define");
        CmpEq("eq");
        CmpNe("ne");
        Br("br");
        Call("call");
        AddOp("add");
        SubOp("sub");
        MulOp("mul");
        SlrOp("slr");
        SllOp("sll");
        SarOp("sar");
        AndOp("and");
        OrOp("or");
        XorOp("xor");
    }

    impl_rules_struct! {
        struct QualReg<'a> (TypeQual<'a>, Register<'a>);
        struct ArgDeclList<'a> (OpenParen<'a>, Option<ArgDeclNode<'a>>, CloseParen<'a>);
        struct ArgDeclNode<'a> (QualReg<'a>, Option<ArgDeclNodeRem<'a>>);
        struct ArgDeclNodeRem<'a> (Comma<'a>, Box<ArgDeclNode<'a>>);
        struct ArgList<'a> (OpenParen<'a>, Option<ArgNode<'a>>, CloseParen<'a>);
        struct ArgNode<'a> (Register<'a>, Option<ArgNodeRem<'a>>);
        struct ArgNodeRem<'a> (Comma<'a>, Box<ArgNode<'a>>);
        struct FunctionDef<'a> (Define<'a>, TypeQual<'a>, FnIdent<'a>, ArgDeclList<'a>, OpenCurly<'a>);
        struct LabelDef<'a> (Label<'a>, LabelIdent<'a>, ArgDeclList<'a>);
        struct AssignStatement<'a> (Register<'a>, Equals<'a>, Source<'a>);
        struct BranchTarget<'a> (Label<'a>, LabelIdent<'a>, ArgList<'a>);
        struct JmpStmt<'a> (Jmp<'a>, BranchTarget<'a>);
        struct Branch<'a> (Br<'a>, CmpType<'a>, Register<'a>, Comma<'a>, 
            Register<'a>, Comma<'a>, BranchTarget<'a>, Comma<'a>, BranchTarget<'a>);
        struct CallStmt<'a> (Call<'a>, FnIdent<'a>, ArgList<'a>);
        // struct GlobalStmt<'a> (Global<'a>, Equals<'a>, Literal<'a>);
    }

    impl_rules_enum! {
        enum Source<'a> {
            Arithmetic(ArithOp<'a>, Register<'a>, Comma<'a>, Register<'a>),
            Reg(TypeQual<'a>, Register<'a>),
            Load(Load<'a>, TypeQual<'a>, Ptr<'a>, Register<'a>),
            StrLiteral(StrLiteral<'a>),
            IntLiteral(IntLiteral<'a>),
        }
        enum Statement<'a> {
            Assign(AssignStatement<'a>),
            Label(LabelDef<'a>),
            Jmp(JmpStmt<'a>),
            Branch(Branch<'a>),
            Call(CallStmt<'a>)
        }
        enum CmpType<'a> {
            Eq(CmpEq<'a>),
            Ne(CmpNe<'a>),
        }
        enum ArithOp<'a> {
            Add(AddOp<'a>),
            Sub(SubOp<'a>),
            Mul(MulOp<'a>),
            Slr(SlrOp<'a>),
            Sll(SllOp<'a>),
            Sar(SarOp<'a>),
            And(AndOp<'a>),
            Or(OrOp<'a>),
            Xor(XorOp<'a>),
        }
    }

    decl_unit_rule!(Register, TypeQual, Op, LabelIdent, StrLiteral, IntLiteral, FnIdent);

    pub enum Type {
        I64,
    }

    pub fn collect_args(mut args: ArgList) -> Vec<Register> {
        let mut list = args.1;
        let mut out = vec![];
        let Some(ArgNode(reg, mut rem)) = list else {
            return out;
        };
        out.push(reg);
        while let Some(ArgNodeRem(_, nrem)) = rem {
            out.push( nrem.0 );
            rem = nrem.1;
        }
        out
    }

    pub fn collect_decl_args(mut args: ArgDeclList) -> Vec<(Type, Register)> {
        let mut list = args.1;
        let mut out = vec![];
        let Some(ArgDeclNode(reg, mut rem)) = list else {
            return out;
        };
        out.push((Type::I64, reg.1));
        while let Some(ArgDeclNodeRem(_, nrem)) = rem {
            out.push((Type::I64, nrem.0 .1));
            rem = nrem.1;
        }
        out
    }

    #[cfg(test)]
    mod test {
        use super::*;
        use std::default::{Default, self};

        macro_rules! parse_structure_assert {
            ($input:expr, $pty:ident match $pat:pat) => {
                {
                    let res = $pty::try_parse($input);
                    assert!(matches!(res, Some(($pat, ""))), "{res:?} matches {}", stringify!($pat));
                }
            };
        }

        #[test]
        fn parse_statement() {
            let input = "%x = i64 %y";
            let expected: Statement = Statement::Assign(AssignStatement(
                Register(Span("%x")),
                Equals(Span("=")),
                Source::Reg(TypeQual(Span("i64")), Register(Span("%y"))),
            ));
            assert_eq!(Statement::try_parse(input), Some((expected, "")))
        }

        #[test]
        fn parse_reg() {
            let input = "%x";
            let expected = Register(Span("%x"));
            assert_eq!(Register::try_parse(input), Some((expected, "")))
        }

        #[test]
        fn parse_reg2() {
            let input = "  %x";
            let expected = Register(Span("%x"));
            assert_eq!(Register::try_parse(input), Some((expected, "")))
        }

        #[test]
        fn parse_fndef() {
            let input = "define i64 @start () {";
            let expected = FunctionDef(
                Default::default(), 
                TypeQual(Span("i64")), 
                FnIdent(Span("@start")), 
                ArgDeclList(Default::default(), None, Default::default()),
                Default::default());
            assert_eq!(FunctionDef::try_parse(input), Some((expected, "")))
        }

        #[test]
        fn parse_stmt() {
            let input = "   %x = \"asdfasdf\"";
            parse_structure_assert!(input, Statement match Statement::Assign(AssignStatement(_, _, _)))
        }

        #[test]
        fn parse_branch() {
            let input = "   br eq %x, %y, label %x.l (), label %y.l ()";
            parse_structure_assert!(input, Statement match Statement::Branch(_))
        }

        #[test]
        fn parse_jmp() {
            let input = "   jmp label %x.l (%y.arg )";
            parse_structure_assert!(input, Statement match Statement::Jmp(_))
        }

        #[test]
        fn parse_literal() {
            let input = "\"asdfasdf\"";
            parse_structure_assert!(input, StrLiteral match StrLiteral(_))
        }
    }
}

enum Statement {
    Op(OpInner),
    Global(GlobalData),
}

impl From<OpInner> for Statement {
    fn from(v: OpInner) -> Self {
        Self::Op(v)
    }
}

pub(crate) struct Parser {
    file: Option<std::path::PathBuf>,
    line: usize,
    buf: String,
    off: std::cell::Cell<usize>,
}

pub(crate) struct ParsedFile {
    pub routine: Routine,
    pub vars: Rc<BTreeMap<usize, Box<str>>>,
    pub globals: BTreeMap<usize, GlobalData>,
}

impl Parser {
    pub fn new_file(file: impl AsRef<std::path::Path>) -> std::io::Result<Self> {
        let name = file.as_ref().to_owned();
        let buf = std::fs::read_to_string(&name)?;
        Ok(Self {
            file: Some(name),
            line: 1,
            buf,
            off: Cell::new(0),
        })
    }

    fn maybe<'p: 'a, 'a, T: Rule<'a>>(&'p self) -> Option<T::Output> {
        let off = self.off.get();
        let (rule, rem) = T::try_parse(&self.buf[off..])?;
        let start = self.buf.as_ptr() as usize;
        let rem_start = rem.as_ptr() as usize;
        self.off.set(rem_start - start);
        Some(rule)
    }

    fn expect<'p: 'a, 'a, T: Rule<'a>>(&'p self) -> T::Output {
        let res = match self.maybe::<T>() {
            Some(res) => return res,
            _ => {
                let name = T::name();
                parse_panic!(self, "could not parse, failed expectation for {name}")
            },
        };
    }

    fn collect_decl_args_idents(idents: &mut IdentMap, args: rules::ArgDeclList) -> Vec<Loc> {
        rules::collect_decl_args(args)
            .into_iter()
            .map(|(_, idt)| idents.assign(idt))
            .collect()
    }

    fn collect_args_idents(idents: &IdentMap, args: rules::ArgList) -> Vec<Loc> {
        rules::collect_args(args)
            .into_iter()
            .map(|idt| idents.lookup(idt))
            .collect()
    }

    fn process_statement(&self, idents: &mut IdentMap, globals: &mut BTreeMap<usize, GlobalData>, stmt: rules::Statement) -> Op {
        let inner: OpInner = match stmt {
            rules::Statement::Assign(rules::AssignStatement(dst, _, src)) => {
                let loc = idents.assign(dst);
                match src {
                    rules::Source::Reg(_, reg) => OpInner::Assign {
                        loc,
                        val: Val::Local(idents.lookup(reg)),
                    },
                    rules::Source::Load(_, _, _, reg) => OpInner::Load {
                        loc,
                        ptr: idents.lookup_or_declare(reg),
                    },
                    rules::Source::StrLiteral(s) => {
                        let gid = super::new_global_id();
                        globals.insert(gid, GlobalData {
                            name: super::global_label(gid).into(),
                            data: self.parse_str_lit(s).into(),
                        });
                        OpInner::Assign { 
                            loc,
                            val: Val::GlobalLabel(gid)
                        }
                    },
                    rules::Source::IntLiteral(int) => OpInner::Assign {
                        loc,
                        val: Val::Literal(int.as_ref().parse().expect("valid int literal")),
                    },
                    rules::Source::Arithmetic(ty, lhs, _, rhs) => {
                        let op1 = idents.lookup(lhs);
                        let op2 = idents.lookup(rhs);
                        match ty {
                            rules::ArithOp::Add(_) => OpInner::Add { dst: loc, op1, op2 },
                            rules::ArithOp::Sub(_) => todo!(),
                            rules::ArithOp::Mul(_) => todo!(),
                            rules::ArithOp::Slr(_) => todo!(),
                            rules::ArithOp::Sll(_) => todo!(),
                            rules::ArithOp::Sar(_) => todo!(),
                            rules::ArithOp::And(_) => todo!(),
                            rules::ArithOp::Or(_) => todo!(),
                            rules::ArithOp::Xor(_) => todo!(),
                        }
                    },
                }
            },
            rules::Statement::Label(rules::LabelDef(_, idt, args)) => OpInner::Block {
                id: idents.assign(idt),
                args: Self::collect_decl_args_idents(idents, args),
            },
            rules::Statement::Jmp(rules::JmpStmt(_, rules::BranchTarget(_, l, args))) => OpInner::Jmp {
                target: Target { 
                    id: idents.lookup_or_declare(l),
                    args: Self::collect_args_idents(idents, args)
                }
            },
            rules::Statement::Branch(rules::Branch(_, ty, lhs, _, rhs, _, rules::BranchTarget(_, l_pass,
                args_pass), _, rules::BranchTarget(_, l_fail, args_fail))) => OpInner::Bne {
                    check: (idents.lookup(lhs), idents.lookup(rhs)),
                    success: Target { id: idents.lookup_or_declare(l_pass), args: Self::collect_args_idents(idents, args_pass) },
                    fail: Target { id: idents.lookup_or_declare(l_fail), args: Self::collect_args_idents(idents, args_fail) },
                },
            rules::Statement::Call(rules::CallStmt(_, func, args)) => OpInner::Call {
                id: 0,
                args: Self::collect_args_idents(idents, args),
            },
        };
        Op {
            inner,
            fileno: 0,
            line: 0,
        }
    }

    pub fn parse(mut self) -> ParsedFile {
        let rules::FunctionDef(_, _, fn_name, args, _) = self.expect::<rules::FunctionDef>();
        let args = rules::collect_decl_args(args);
        let mut idents = IdentMap::new();
        for (_, arg) in args {
            idents.assign(arg);
        }
        let mut ops = vec![];
        let mut globals = BTreeMap::new();
        while let Some(stmt) = self.maybe::<rules::Statement>() {
            // dbg!(&stmt);
            // dbg!(&self.buf[self.off.get()..]);
            ops.push(self.process_statement(&mut idents, &mut globals, stmt));
        }
        self.expect::<rules::CloseCurly>();
        let routine = Routine {
            name: fn_name.as_ref()[1..].into(),
            ops,
        };
        idents.assert_all_initialized(&globals);
        ParsedFile {
            routine,
            vars: idents.invert().into(),
            globals,
        }
    }

    fn peek_next_token_any(&self) -> &str {
        let data = &self.buf[self.off.get()..];
        let off = data.bytes().take_while(|b| b.is_ascii_whitespace()).count();
        // if data[..off].as_bytes().contains(&b'\n') {
        //     return "[[EOL]]";
        // }
        let data = &data[off..];
        let Some((start, ch)) = data.char_indices().next() else {
            return "[[empty input]]";
        };
        let first_byte = ch as u32 as u8;
        if [b'{', b'}', b'[', b']', b'(', b')'].contains(&first_byte) {
            &data[..1]
        } else {
            let end = data
                .bytes()
                .take_while(|b| b.is_ascii_alphabetic() || [b'.', b':'].contains(b))
                .count();
            &data[..end]
        }
    }

    #[track_caller]
    fn parse_panic<'a, F, D>(&self, msg: F) -> !
    where
        F: FnOnce() -> D,
        D: std::fmt::Display,
    {
        let not_eol = |b: u8| b != b'\n';
        let eol = |b| !not_eol(b);
        let off = self.off.get() + self.buf[self.off.get()..].bytes().take_while(u8::is_ascii_whitespace).count();
        let next_tok = self.peek_next_token_any();
        let error_len = if self.buf.len() == off {1} else {next_tok.len()};

        let line_off_start = off
            - self.buf[..off]
                .bytes()
                .rev()
                .position(eol)
                .unwrap_or(off);
        let line_off_end = self.buf[line_off_start..]
            .bytes()
            .position(eol)
            .unwrap_or(self.buf.len() - line_off_start)
            + line_off_start;
        let mut line_content: String = self.buf[line_off_start..line_off_end].replace("\t", "    ");
        let file_name = self
            .file
            .as_ref()
            .map(|f| f.display().to_string())
            .unwrap_or_else(|| "anonymous file".into());
        let line = self.line;
        let line_off: usize = self.buf[line_off_start..].bytes()
            .take(off - line_off_start).map(|b| {
                match b {
                    b'\t' => 4,
                    _ => 1
                }
            }).sum();
        let blue = "\x1b[94;1m";
        let red = "\x1b[31m";
        let reset = "\x1b[0m";
        panic!(
            "Error reading file {file_name} on line {line}:\n\n\
            {blue}    --> {reset}{file_name}:\n\
            {blue}     | {reset}\n\
            {blue}{line:<4} | {reset}{line_content}\n\
            {blue}     | {empty:>line_off$}{red}{marker:}{reset}\n\t\
            {msg}",
            msg = msg(),
            empty = ' ',
            marker = "^".repeat(error_len)
        )
    }

    fn parse_str_lit(&self, s: impl AsRef<str>) -> Box<str> {
        let s = s.as_ref();
        assert_eq!(s.as_bytes().first(), Some(&b'"'));
        assert_eq!(s.as_bytes().last(), Some(&b'"'));
        assert!(s.len() >= 2);
        let s = &s[1..s.len() - 1];
        let mut out = String::new();
        let mut escape = false;
        for c in s.chars() {
            if escape {
                let esc = match c {
                    'n' => '\n',
                    't' => '\t',
                    _ => parse_panic!(self, "unknown escape \\{c}")
                };
                escape = false;
                out.push(esc)
            } else {
                if c == '\\' {
                    escape = true;
                } else {
                    out.push(c);
                }
            }
        }
        out.into()
    }
}
