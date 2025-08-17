mod generate;
mod parse;

use generate::generate;
use parse::parse;
use proc_macro::TokenStream;
use proc_macro_error::{proc_macro_error, set_dummy};
use quote::quote;

#[proc_macro]
#[proc_macro_error]
pub fn script(tokens: TokenStream) -> TokenStream {
    set_dummy(quote!((::tondi_script::builder::StructuredScript::new("script"))));
    generate(parse(tokens.into())).into()
}
