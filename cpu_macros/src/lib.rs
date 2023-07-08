use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput};

#[proc_macro_derive(FromHexCode)]
pub fn derive_from_hex_code(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    if let Data::Enum(data) = input.data {
        let name = input.ident;

        let tokens = data.variants.into_iter().map(|variant| {
            let name = variant.ident;
            let value = variant.discriminant.unwrap().1;
            quote! {#value => Ok(#name),}
        });
        let expanded = quote! {
            impl TryFrom<u8> for #name {
                type Error = ();
                fn try_from(value: u8) -> Result<Self, Self::Error> {
                    use #name::*;
                    match value {
                        #(#tokens)*
                        _ => {Err(())}
                    }
                }
            }
        };
        TokenStream::from(expanded)
    } else {
        panic!("must be enum");
    }
}
