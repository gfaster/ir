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
use syn::Token;


struct IGenInput {
    name: syn::Ident,
    attr: Vec<syn::Attribute>,
    def_str: syn::LitStr,
}

impl syn::parse::Parse for IGenInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        let mut attr = vec![];
        while lookahead.peek(Token![#]){
            attr.extend(input.call(syn::Attribute::parse_inner)?);
        }
        let name: syn::Ident = input.parse()?;
        let _: Token![=] = input.parse()?;
        let def_str = input.parse()?;
        let _: Token![;] = input.parse()?;
        Ok(
            IGenInput { name, attr, def_str }
        )
    }
}

#[proc_macro]
pub fn new_instr(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as IGenInput);
    new_instr_inner(input).unwrap_or_else(syn::Error::into_compile_error).into()
}

fn new_instr_inner(mut input: IGenInput) -> syn::Result<proc_macro2::TokenStream> {
    let params = parse_params(&mut input)?;
    let Def { name, args } = parse_def(input.def_str, &params)?;
    let ty_name = input.name;
    let arg_names = args.iter().map(|(n, _)| n).take(args.len().saturating_sub(1));
    let last = args.last().map(|(n, _)| n).into_iter();
    let ret = quote! {
        struct #ty_name;
        impl std::fmt::Debug for #ty_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}<{}>", #name, stringify!(#ty_name))?;
                #(write!(f, " {},", #arg_names)?;)*
                #(write!(f, " {}", #last)?;)*
                Ok(())
            }
        }
    };
    Ok(ret)
}

struct Params {
    types: HashMap<String, syn::Ident>,
}


fn parse_params(input: &mut IGenInput) -> syn::Result<Params> {
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
