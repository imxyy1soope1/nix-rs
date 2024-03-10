extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn;
use syn::DeriveInput;

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
