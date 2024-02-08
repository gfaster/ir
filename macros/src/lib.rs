//!  
//! RegGroup name = (idt, idt, idt);
//!
//! #[types($ty = $Group)]
//! name = "$mnemonic $var:$ty";
//!
//!

use std::collections::HashMap;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{braced, parenthesized, punctuated::Punctuated, Token};

mod kw {
    use syn::custom_keyword;

    custom_keyword!(req);
    custom_keyword!(iset);
    custom_keyword!(iused);
    custom_keyword!(set);
    custom_keyword!(used);
}

#[derive(Clone)]
enum SPropIdentListsType {
    Req(kw::req),
    Set(kw::set),
    Used(kw::used),
}

impl syn::parse::Parse for SPropIdentListsType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let look = input.lookahead1();
        if look.peek(kw::req) {
            Ok(Self::Req(input.parse()?))
        }else if look.peek(kw::set) {
            Ok(Self::Set(input.parse()?))
        }else if look.peek(kw::used) {
            Ok(Self::Used(input.parse()?))
        }else {
            Err(look.error())
        }
    }
}

#[derive(Clone)]
struct SPropIdentLists {
    ty: SPropIdentListsType,
    idents: Vec<syn::Ident>,
}

impl syn::parse::Parse for SPropIdentLists {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ty = input.parse()?;
        let mut idents = vec![];
        while !input.is_empty() {
            idents.push(input.parse()?);
        }
        Ok(Self {
            ty,
            idents,
        })
    }
}

#[derive(Clone)]
enum SPropExprListsType {
    Iset(kw::iset),
    Iused(kw::iused),
}

impl syn::parse::Parse for SPropExprListsType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let look = input.lookahead1();
        if look.peek(kw::iset) {
            Ok(Self::Iset(input.parse()?))
        }else if look.peek(kw::iused) {
            Ok(Self::Iused(input.parse()?))
        } else {
            Err(look.error())
        }
    }
}

#[derive(Clone)]
struct SPropExprLists {
    ty: SPropExprListsType,
    arr: syn::ExprArray,
}

impl syn::parse::Parse for SPropExprLists {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ty = input.parse()?;
        let arr = input.parse()?;
        Ok(Self { ty, arr })
    }
}


#[derive(Clone)]
enum SPropOverrideType {
    Idents(SPropIdentLists),
    Expr(SPropExprLists)
}

impl syn::parse::Parse for SPropOverrideType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::set) || lookahead.peek(kw::used) || lookahead.peek(kw::req) {
            Ok(Self::Idents(input.parse()?))
        }  else if lookahead.peek(kw::iset) || lookahead.peek(kw::iused) {
            Ok(Self::Expr(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Clone)]
struct SPropOverride {
    inner: SPropOverrideType,
}

impl syn::parse::Parse for SPropOverride {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let inner;
        parenthesized!(inner in input);
        let ty = inner.parse()?;
        if !inner.is_empty() {
            return Err(inner.error("unexpected token"))
        }
        Ok(
            Self { inner: ty }
        )
    }
}


#[derive(Clone)]
struct BasicPropOverride {
    name: syn::Ident,
    value: syn::Lit,
}

impl syn::parse::Parse for BasicPropOverride {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name: syn::Ident = input.parse()?;
        let _: Token![=] = input.parse()?;
        let value = input.parse()?;
        Ok(
            BasicPropOverride { name, value }
        )
    }
}

impl quote::ToTokens for BasicPropOverride {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let Self { name, value } = self;
        tokens.extend(quote!{
            #name: #value
        });
    }
}

#[derive(Clone)]
enum PropOverride {
    Basic(BasicPropOverride),
    S(SPropOverride),
}

impl PropOverride {
    fn as_basic(&self) -> Option<&BasicPropOverride> {
        if let Self::Basic(b) = self {
            Some(b)
        } else {
            None
        }
    }
}

impl syn::parse::Parse for PropOverride {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let look = input.lookahead1();
        if look.peek(syn::token::Paren) {
            Ok(Self::S(input.parse()?))
        } else if look.peek(syn::Ident) {
            Ok(Self::Basic(input.parse()?))
        } else {
            Err(look.error())
        }
    }
}

struct Block {
    stmts: Punctuated<DeclExpression, Token![;]>,
}

impl syn::parse::Parse for Block {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Block {
            stmts: input.parse_terminated(DeclExpression::parse, Token![;])?,
        })
    }
}

enum DeclExpression {
    Instruction(InstructionDecl),
    OverrideBlock(OverrideBlock),
}

impl syn::parse::Parse for DeclExpression {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        let res;
        if lookahead.peek(Token![let]) {
            res = DeclExpression::OverrideBlock(input.parse()?);
        } else {
            res = DeclExpression::Instruction(input.parse()?);
        }
        Ok(res)
    }
}

struct OverrideBlock {
    overrides: Vec<PropOverride>,
    inner: Block,
}

impl syn::parse::Parse for OverrideBlock {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _: Token![let] = input.parse()?;
        let mut overrides: Vec<PropOverride> = vec![];
        while input.lookahead1().peek(syn::Ident) {
            overrides.push(input.parse()?);
            if !input.lookahead1().peek(Token![,]) {
                break
            }
            let _: Token![,] = input.parse()?;
        }
        let _: Token![=>] = input.parse()?;
        let inner_stream;
        let _ = braced!(inner_stream in input);
        let inner: Block = inner_stream.parse()?;
        Ok(Self {
            overrides,
            inner,
        })
    }
}

struct InstructionDecl {
    name: syn::Ident,
    attr: Vec<syn::Attribute>,
    def_str: syn::LitStr,
}

impl syn::parse::Parse for InstructionDecl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut attr = vec![];
        while input.lookahead1().peek(Token![#]){
            attr.extend(input.call(syn::Attribute::parse_inner)?);
        }
        let name: syn::Ident = input.parse()?;
        let _: Token![=] = input.parse()?;
        let def_str = input.parse()?;
        let _: Token![;] = input.parse()?;
        Ok(
            InstructionDecl { name, attr, def_str }
        )
    }
}

#[proc_macro]
pub fn new_instr(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as Block);
    new_instr_inner(input, &[]).unwrap_or_else(syn::Error::into_compile_error).into()
}

fn new_instr_inner(input: Block, overrides: &[PropOverride]) -> syn::Result<proc_macro2::TokenStream> {
    let mut output = proc_macro2::TokenStream::new();
    for item in input.stmts.into_iter() {
        match item {
            DeclExpression::Instruction(instr) => output.extend(new_instr_decl(instr, overrides)?),
            DeclExpression::OverrideBlock(ob) => {
                let mut new_overrides = overrides.to_vec();
                new_overrides.extend_from_slice(&ob.overrides);
                output.extend(new_instr_inner(ob.inner, &new_overrides)?)
            },
        }
    }
    Ok(output)
}

fn new_instr_decl(mut input: InstructionDecl, overrides: &[PropOverride]) -> syn::Result<proc_macro2::TokenStream> {
    let params = parse_params(&mut input)?;
    let Def { name, args } = parse_def(input.def_str, &params)?;
    let basic_overrides = overrides.iter().map(PropOverride::as_basic);
    let args_ty = args.iter().map(|(_, t)| t);
    let op_cnt = args.len() as u8;
    let ty_name = input.name;
    let ret = quote! {
        const #ty_name: MachineInstrProp = MachineInstrProp {
            basic: BasicInstrProp {
                mnemonic: #name,
                op_cnt: #op_cnt,
                res_cnt: 1,
                #(#basic_overrides, )*
                ..BasicInstrProp {
                    op_cnt: 0,
                    res_cnt: 0,
                    mnemonic: "[TEMPLATE]",
                    is_branch: false,
                    is_terminator: false,
                    is_block_header: false,
                    is_commutative: false,
                    has_side_effects: true,
                    may_read_memory: true,
                    may_write_memory: true,
                    is_barrier: false,
                    operand_relative_type_constraints: &[],
                    simulation: None,
                }
            },
            op_ty: &[#(#args_ty),*],
            op_eq_constraints: &[],
            ref_regs: &[],
            operand_use: &[]
        };
    };
    Ok(ret)
}



struct Params {
    types: HashMap<String, syn::Ident>,
}


fn parse_params(input: &mut InstructionDecl) -> syn::Result<Params> {
    let mut types = HashMap::new();
    let attrs = std::mem::take(&mut input.attr);
    for attr in attrs {
        if attr.path().is_ident("ty") {
            type_attr(attr, &mut types)?;
        }
    }
    Ok(Params { types })
}

fn type_attr(attr: syn::Attribute, set: &mut HashMap<String, syn::Ident>) -> syn::Result<()> {
    attr.parse_nested_meta(|meta| {
        let var = meta.path.require_ident()?;
        let value = meta.value()?;
        let idt: syn::Ident = value.parse()?;
        if set.insert(var.to_string(), idt).is_some() {
            Err(meta.error(format!("duplicate definition of type {:?}", var.to_string())))?;
        };
        Ok(())
    })?;
    Ok(())
}

struct Def {
    name: String,
    /// (name, type)
    args: Vec<(String, syn::Ident)>
}

fn parse_def(input: syn::LitStr, params: &Params) -> syn::Result<Def> {
    let oval = input.value();

    if !oval.is_ascii() {
        return Err(syn::Error::new(input.span(), "definition string must be ascii"))
    }

    let val = oval.trim_start();

    fn maybe_str<'a>(stream: &'a str, expected: &'_ str) -> Option<&'a str> {
        let stream = stream.trim_start();
        if !stream.starts_with(expected) {
            return None
        }
        Some(&stream[expected.len()..])
    }
    fn parse_idt<'a>(s: &'a str) -> Option<(&'a str, &'a str)> {
        let s = s.trim_start();
        let it = s.char_indices();
        let (end, _) = it.take_while(|(_, c)| c.is_ascii_alphanumeric() || c == &'_').last()?;
        let end = end + 1;
        Some((&s[0..end], &s[end..]))
    }


    let Some((name, mut rem)) = parse_idt(val) else {
        return Err(syn::Error::new(input.span(), "expected instruction name"))
    };

    let mut def = Def {
        name: name.to_owned(),
        args: Vec::new(),
    };

    while !rem.is_empty() {
        let Some((reg_name, prem)) = parse_idt(rem) else {
            return Err(syn::Error::new(input.span(), "expected instruction argument"))
        };
        rem = prem;
        rem = maybe_str(rem, ":").ok_or_else(|| syn::Error::new(input.span(), "expected ':'"))?;
        let Some((reg_type, prem)) = parse_idt(rem) else {
            return Err(syn::Error::new(input.span(), "expected argument type"))
        };
        rem = prem;

        let reg_name = reg_name.to_owned();
        let reg_type = params.types.get(reg_type).cloned().unwrap_or_else(|| syn::Ident::new(reg_type, Span::call_site()));
        def.args.push((reg_name, reg_type));
    }

    Ok(def)
}

#[cfg(test)]
mod test {

    #[test]
    fn test0() {
    }

}
