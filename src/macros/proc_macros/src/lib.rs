extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn;
use syn::DeriveInput;

#[proc_macro_attribute]
pub fn display_fmt(attr: TokenStream, item: TokenStream) -> TokenStream {
    let fmt: syn::Expr = syn::parse(attr).unwrap();
    let ast: DeriveInput = syn::parse(item).unwrap();

    let syn::Expr::Lit(lit) = fmt else {
        unreachable!()
    };
    let syn::Lit::Str(fmt) = lit.lit else {
        unreachable!()
    };

    let syn::Data::Struct(ref data) = ast.data else {
        unreachable!()
    };
    let syn::Fields::Named(ref fields) = data.fields else {
        unreachable!()
    };
    let mut args = quote!();

    for syn::Field{ident, ..} in fields.named.iter() {
        args.extend(quote!(self.#ident,));
    }

    let ref name = ast.ident;
    quote! {
        #ast

        impl Display for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, #fmt, #args)
            }
        }
    }.into()
}

#[proc_macro_attribute]
pub fn debug_fmt(attr: TokenStream, item: TokenStream) -> TokenStream {
    let fmt: syn::Expr = syn::parse(attr).unwrap();
    let ast: DeriveInput = syn::parse(item).unwrap();

    let syn::Expr::Lit(lit) = fmt else {
        unreachable!()
    };
    let syn::Lit::Str(fmt) = lit.lit else {
        unreachable!()
    };

    let syn::Data::Struct(ref data) = ast.data else {
        unreachable!()
    };
    let syn::Fields::Named(ref fields) = data.fields else {
        unreachable!()
    };
    let mut args = quote!();

    for syn::Field{ident, ..} in fields.named.iter() {
        args.extend(quote!(let #ident = &self.#ident;));
    }

    let ref name = ast.ident;
    quote! {
        #ast

        impl Debug for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #args
                write!(f, #fmt)
            }
        }
    }.into()
}
