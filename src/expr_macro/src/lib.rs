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

#[proc_macro_derive(Expression)]
pub fn expr_macro_derive(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    impl_expr(&ast)
}

fn impl_expr(ast: &DeriveInput) -> TokenStream {
    let syn::Data::Struct(ref data) = ast.data else {
        unreachable!()
    };
    let syn::Fields::Named(ref fields) = data.fields else {
        unreachable!()
    };
    let name = &ast.ident;
    let mut args = quote!();
    let mut fds = quote!();

    for syn::Field{ident, ty, ..} in fields.named.iter() {
        args.extend(quote!(#ident: #ty,));
        fds.extend(quote!(#ident,))
    }
    quote! {
        impl #name {
            pub fn new(#args) -> #name {
                #name { #fds }
            }
        }

        impl Expression for #name {
            fn as_any(&self) -> &dyn Any {
                self
            }

            fn into_any(self: Box<Self>) -> Box<dyn Any> {
                self
            }
        }
    }.into()
}
