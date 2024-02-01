use crate::{
    attr::BindAttributes,
    instr::{AllocationType, BindList, Instruction, Target, ValueList},
    reg::{Binding, Immediate},
    ty::Type,
    value::{Function, ValueHandle},
};
use std::{
    cell::Cell,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    ops::Range,
    rc::Rc,
    sync::Arc,
};

use crate::{
    instr::{MachineInstruction, OpInner},
    ir::instruction_map,
    unique_label, BlockId, GlobalData, IdTy, InstrArg, Val, VarSet,
};

use self::rules::{collect_decl_args, Rule};

trait Generator {
    type Item;
    fn next(&mut self) -> Self::Item;
}

// struct BindingGen;
// impl Generator for BindingGen {
//     type Item = Binding;
//     fn next(&mut self) -> Self::Item {
//         Binding::new_virtual()
//     }
// }

impl<I, F> Generator for F
where
    F: FnMut() -> I,
{
    type Item = I;
    fn next(&mut self) -> Self::Item {
        self()
    }
}

struct IdentMap {
    map: HashMap<Rc<str>, Option<ValueHandle>>,
}

impl IdentMap {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn try_lookup(&self, ident: impl AsRef<str>) -> Option<ValueHandle> {
        *self.map.get(ident.as_ref())?
    }

    fn lookup(&self, ident: impl AsRef<str>) -> ValueHandle {
        let Some(ret) = self.try_lookup(&ident) else {
            panic!("identifier {} undeclared", ident.as_ref());
        };
        ret
    }

    fn assign(&mut self, ident: impl AsRef<str>, val: ValueHandle) {
        let name = ident.as_ref().into();
        self.map
            .entry(Rc::clone(&name))
            .and_modify(|v| {
                panic!("cannot assign twice to identifier {} (This could also be a use before decleare)", ident.as_ref())
            })
            .or_insert_with(|| Some(val));
    }
}

type BindMap = IdentMap;

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

    use crate::{ir::instruction_map, Type};

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

    #[derive(PartialEq, Eq, Clone, Copy)]
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
            impl<'a> $name<'a> {
                pub fn get_str(self) -> &'a str {
                    self.0.0
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
            #[derive(PartialEq, Eq, Debug, Clone, Copy)]
            pub struct $name<'a>(Span<'a>);

            impl AsRef<str> for $name<'_> {
                fn as_ref(&self) -> &str {
                    self.0.0
                }
            }
            impl<'a> $name<'a> {
                pub fn get_str(self) -> &'a str {
                    self.0.0
                }
            }
            )*
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
            s.bytes()
                .take(len)
                .all(|b| b"0123456789".contains(&b))
                .then_some(())?;
            Some((Self(Span(&s[..len])), &s[len..]))
        }
    }

    impl<'a> Rule<'a> for Op<'a> {
        type Output = Self;

        fn try_parse(input: &'a str) -> Option<(Self::Output, &'a str)> {
            let s = trim_nolf(input);
            let map = instruction_map();
            let len = s.bytes().take_while(u8::is_ascii_alphanumeric).count();
            (len > 0).then_some(())?;
            map.contains_key(&s[..len]).then_some(())?;
            Some((Self(Span(&s[..len])), &s[len..]))
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
        Alloca("alloca");
        Store("store");
        Ret("ret");
    }

    impl_rules_struct! {
        struct QualReg<'a> (TypeQual<'a>, Register<'a>);
        struct ArgDeclList<'a> (OpenParen<'a>, Option<ArgDeclNode<'a>>, CloseParen<'a>);
        struct ArgDeclNode<'a> (QualReg<'a>, Option<ArgDeclNodeRem<'a>>);
        struct ArgDeclNodeRem<'a> (Comma<'a>, Box<ArgDeclNode<'a>>);
        struct ArgList<'a> (OpenParen<'a>, Option<ArgNode<'a>>, CloseParen<'a>);
        struct ArgNode<'a> (TypeQual<'a>, Arg<'a>, Option<ArgNodeRem<'a>>);
        struct ArgNodeRem<'a> (Comma<'a>, Box<ArgNode<'a>>);
        struct FunctionDef<'a> (Define<'a>, TypeQual<'a>, FnIdent<'a>, ArgDeclList<'a>, OpenCurly<'a>);
        struct LabelDef<'a> (Label<'a>, LabelIdent<'a>, ArgDeclList<'a>);
        struct AssignStatement<'a> (Register<'a>, Equals<'a>, Source<'a>);
        struct BranchTarget<'a> (Label<'a>, LabelIdent<'a>, ArgList<'a>);
        struct JmpStmt<'a> (Jmp<'a>, BranchTarget<'a>);
        struct Branch<'a> (Br<'a>, Arg<'a>, Comma<'a>, BranchTarget<'a>, Comma<'a>, BranchTarget<'a>);
        struct CallStmt<'a> (Call<'a>, FnIdent<'a>, ArgList<'a>);
        struct StoreStmt<'a> (Store<'a>, TypeQual<'a>, Arg<'a>, Comma<'a>, Ptr<'a>, Register<'a>);
        struct RetStmt<'a> (Ret<'a>, TypeQual<'a>, Arg<'a>);
        // struct GlobalStmt<'a> (Global<'a>, Equals<'a>, Literal<'a>);
    }

    impl_rules_enum! {
        enum Literal<'a> {
            StrLiteral(StrLiteral<'a>),
            IntLiteral(IntLiteral<'a>),
        }
        enum Source<'a> {
            Binary(Op<'a>, TypeQual<'a>, Arg<'a>, Comma<'a>, Arg<'a>),
            Alloca(Alloca<'a>, TypeQual<'a>),
            Load(Load<'a>, TypeQual<'a>, Comma<'a>, Ptr<'a>, Register<'a>),
            Literal(TypeQual<'a>, Literal<'a>),
        }
        enum Arg<'a> {
            Reg(Register<'a>),
            Literal(Literal<'a>),
        }
        enum Statement<'a> {
            Assign(AssignStatement<'a>),
            Label(LabelDef<'a>),
            Jmp(JmpStmt<'a>),
            Branch(Branch<'a>),
            Call(CallStmt<'a>),
            Store(StoreStmt<'a>),
            Ret(RetStmt<'a>),
        }
    }

    decl_unit_rule!(Register, TypeQual, Op, LabelIdent, StrLiteral, IntLiteral, FnIdent);

    impl From<TypeQual<'_>> for Type {
        fn from(value: TypeQual<'_>) -> Self {
            let s = value.as_ref();
            match s {
                "ptr" => Type::ptr(),
                "i64" => Type::i64(),
                "i32" => Type::i32(),
                "i16" => Type::i16(),
                "i8" => Type::i8(),
                "i1" => Type::i1(),
                _ => panic!("invalid type {s:?}"),
            }
        }
    }

    impl From<IntLiteral<'_>> for crate::reg::InstrArg {
        fn from(value: IntLiteral) -> Self {
            let s: &str = value.as_ref();
            let Ok(n) = s.parse() else {
                panic!("cannot parse as an integer literal {s:?}")
            };
            crate::reg::InstrArg::from(crate::reg::Immediate(n))
        }
    }

    impl From<IntLiteral<'_>> for crate::reg::Immediate {
        fn from(value: IntLiteral) -> Self {
            let s: &str = value.as_ref();
            let Ok(n) = s.parse() else {
                panic!("cannot parse as an integer literal {s:?}")
            };
            crate::reg::Immediate(n)
        }
    }

    pub fn collect_args(mut args: ArgList) -> Vec<(TypeQual, Arg)> {
        let mut list = args.1;
        let mut out = vec![];
        let Some(ArgNode(ty, reg, mut rem)) = list else {
            return out;
        };
        out.push((ty, reg));
        while let Some(ArgNodeRem(_, nrem)) = rem {
            let ArgNode(ty, reg, nrem) = *nrem;
            out.push((ty, reg));
            rem = nrem;
        }
        out
    }

    pub(crate) fn collect_decl_args<'a>(mut args: &ArgDeclList<'a>) -> Vec<(Type, Register<'a>)> {
        let mut list = &args.1;
        let mut out = vec![];
        let Some(ArgDeclNode(reg, ref rem)) = list.as_ref() else {
            return out;
        };
        let mut rem = rem.as_ref();
        out.push((reg.0.into(), reg.1.clone()));
        while let Some(ArgDeclNodeRem(_, nrem)) = rem {
            out.push((nrem.0 .0.into(), nrem.0 .1));
            rem = nrem.1.as_ref();
        }
        out
    }

    #[cfg(test)]
    mod test {
        use super::*;
        use std::default::{self, Default};

        macro_rules! parse_structure_assert {
            ($input:expr, $pty:ident match $pat:pat) => {{
                let res = $pty::try_parse($input);
                assert!(
                    matches!(res, Some(($pat, ""))),
                    "{res:?} matches {}",
                    stringify!($pat)
                );
            }};
        }

        // #[test]
        // fn parse_statement() {
        //     let input = "%x = i64 %y";
        //     let expected: Statement = Statement::Assign(AssignStatement(
        //         Register(Span("%x")),
        //         Equals(Span("=")),
        //         Source::Reg(TypeQual(Span("i64")), Register(Span("%y"))),
        //     ));
        //     assert_eq!(Statement::try_parse(input), Some((expected, "")))
        // }

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
                Default::default(),
            );
            assert_eq!(FunctionDef::try_parse(input), Some((expected, "")))
        }

        #[test]
        #[ignore = "outdated"]
        fn parse_stmt() {
            let input = "   %x = \"asdfasdf\"";
            parse_structure_assert!(input, Statement match Statement::Assign(AssignStatement(_, _, _)))
        }

        #[test]
        #[ignore = "outdated"]
        fn parse_branch() {
            let input = "   br eq %x, %y, label %x.l (), label %y.l ()";
            parse_structure_assert!(input, Statement match Statement::Branch(_))
        }

        #[test]
        #[ignore = "outdated"]
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
    pub routine: Function,
    pub globals: BTreeMap<Binding, GlobalData>,
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
            }
        };
    }

    fn collect_args_idents(
        &self,
        args: rules::ArgList,
        idents: &mut BindMap,
        func: &Function,
    ) -> ValueList {
        rules::collect_args(args)
            .into_iter()
            .map(|(ty, idt)| self.process_arg(idt, ty.into(), idents, func))
            .collect()
    }

    fn process_arg(
        &self,
        a: rules::Arg,
        ty: Type,
        idents: &mut BindMap,
        func: &Function,
    ) -> ValueHandle {
        match a {
            rules::Arg::Reg(r) => idents.lookup(r),
            rules::Arg::Literal(rules::Literal::StrLiteral(s)) => {
                let gid = Binding::new_ir_bind();
                func.push_global(GlobalData {
                    name: super::global_label(gid).into(),
                    data: self.parse_str_lit(s).into(),
                })
            }
            rules::Arg::Literal(rules::Literal::IntLiteral(i)) => func.push_imm(ty, i.into()),
        }
    }

    fn add_blocks<'a: 'b, 'b>(
        &self,
        func: &Function,
        idents: &mut BindMap,
        stmts: impl IntoIterator<Item = &'b rules::Statement<'a>>,
    ) {
        for stmt in stmts {
            let rules::Statement::Label(rules::LabelDef(_, idt, args)) = stmt else {
                continue;
            };
            let collected = rules::collect_decl_args(args);
            let iter = collected.iter().map(|(ty, _)| BindAttributes::new(*ty));
            for (handle, ident) in func.push_block(iter).zip(
                [idt.as_ref()]
                    .into_iter()
                    .chain(collected.iter().map(|i| i.1.get_str())),
            ) {
                idents.assign(ident, handle)
            }
        }
    }

    fn process_statement(
        &self,
        current_trail: &mut ValueHandle,
        func: &Function,
        idents: &mut BindMap,
        globals: &mut BTreeMap<Binding, GlobalData>,
        stmt: rules::Statement,
    ) {
        let inner: OpInner = match stmt {
            rules::Statement::Assign(rules::AssignStatement(dst, _, src)) => {
                let res_ty;
                let inner = match src {
                    rules::Source::Load(_, ty, _, _, reg) => {
                        res_ty = ty.into();
                        OpInner::Load {
                            ptr: idents.lookup(reg).into(),
                        }
                    }
                    rules::Source::Binary(op, ty, lhs, _, rhs) => {
                        let ty = ty.into();
                        res_ty = ty;
                        let op1 = self.process_arg(lhs, ty, idents, func);
                        let op2 = self.process_arg(rhs, ty, idents, func);
                        OpInner::from_binary_op(op, ty.into(), op1, op2)
                            .expect("valid ir instruction")
                    }
                    rules::Source::Alloca(_, ty) => {
                        res_ty = Type::ptr();
                        OpInner::Alloc {
                            ty: AllocationType::Stack,
                        }
                    }
                    rules::Source::Literal(ty, rules::Literal::StrLiteral(lit)) => {
                        todo!()
                    }
                    rules::Source::Literal(ty, rules::Literal::IntLiteral(lit)) => {
                        let res = func.insert_imm_after(ty.into(), lit.into(), *current_trail);
                        *current_trail = res;
                        idents.assign(dst, res);
                        return;
                    }
                };
                let res = func.insert_instruction_after(
                    Instruction::from_inner(inner),
                    [BindAttributes::new(res_ty).with_name_ref(dst)],
                    *current_trail,
                );
                let Some(res) = res.last() else {
                    panic!("assignment instructions should produce a result");
                };
                *current_trail = res;
                idents.assign(dst, res);
                return;
            }
            rules::Statement::Label(rules::LabelDef(_, idt, args)) => {
                *current_trail = idents.lookup(idt);
                return;
            }
            rules::Statement::Jmp(rules::JmpStmt(_, rules::BranchTarget(_, l, args))) => {
                OpInner::Jmp {
                    target: Target {
                        id: idents.lookup(l),
                        args: self.collect_args_idents(args, idents, func),
                    },
                }
            }
            rules::Statement::Branch(rules::Branch(
                _,
                ck,
                _,
                rules::BranchTarget(_, l_pass, args_pass),
                _,
                rules::BranchTarget(_, l_fail, args_fail),
            )) => OpInner::Br {
                check: self.process_arg(ck, Type::i1(), idents, func),
                success: Target {
                    id: idents.lookup(l_pass),
                    args: self.collect_args_idents(args_pass, idents, func),
                },
                fail: Target {
                    id: idents.lookup(l_fail),
                    args: self.collect_args_idents(args_fail, idents, func),
                },
            },
            rules::Statement::Call(rules::CallStmt(_, _func_name, args)) => OpInner::Call {
                id: 0,
                args: self.collect_args_idents(args, idents, func),
            },
            rules::Statement::Store(rules::StoreStmt(_, ty, src, _, _, dst)) => OpInner::Store {
                val: self.process_arg(src, ty.into(), idents, func),
                dst: idents.lookup(dst).into(),
            },
            rules::Statement::Ret(rules::RetStmt(_, ty, ret)) => OpInner::Return {
                val: self.process_arg(ret, ty.into(), idents, func),
            },
        };

        let res = func.insert_instruction_after(
            Instruction::from_inner(inner),
            [BindAttributes::new(Type::void())],
            *current_trail,
        );
        let Some(res) = res.last() else {
            panic!("all ops should produce a result");
        };
        *current_trail = res;
    }

    pub fn parse(mut self) -> ParsedFile {
        let rules::FunctionDef(_, _, fn_name, args, _) = self.expect::<rules::FunctionDef>();
        let mut idents = BindMap::new();
        let mut routine = Function::new();

        let mut current_trail = {
            let collected = rules::collect_decl_args(&args);
            let iter = collected.iter().map(|(ty, _)| BindAttributes::new(*ty));
            routine
                .push_block(iter)
                .last()
                .expect("inserting a function yields at least one Value")
        };

        // let mut ops = vec![];
        let mut globals = BTreeMap::new();
        let mut stmts = vec![];
        while let Some(stmt) = self.maybe::<rules::Statement>() {
            // dbg!(&stmt);
            // dbg!(&self.buf[self.off.get()..]);
            // ops.push(self.process_statement(&mut blocks, &mut idents, &mut globals, stmt).into());
            stmts.push(stmt);
        }
        self.expect::<rules::CloseCurly>();

        self.add_blocks(&routine, &mut idents, &stmts);

        for stmt in stmts {
            self.process_statement(
                &mut current_trail,
                &routine,
                &mut idents,
                &mut globals,
                stmt,
            );
        }
        ParsedFile { routine, globals }
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
        let off = self.off.get()
            + self.buf[self.off.get()..]
                .bytes()
                .take_while(u8::is_ascii_whitespace)
                .count();
        let next_tok = self.peek_next_token_any();
        let error_len = if self.buf.len() == off {
            1
        } else {
            next_tok.len()
        };

        let line_off_start = off - self.buf[..off].bytes().rev().position(eol).unwrap_or(off);
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
        let line_off: usize = self.buf[line_off_start..]
            .bytes()
            .take(off - line_off_start)
            .map(|b| match b {
                b'\t' => 4,
                _ => 1,
            })
            .sum();
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
                    _ => parse_panic!(self, "unknown escape \\{c}"),
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
