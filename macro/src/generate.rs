use super::parse::Syntax;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};

pub fn generate(syntax: Vec<(Syntax, Span)>) -> TokenStream {
    let mut tokens = quote!(::tondi_script::Script::new(
        ::tondi_script::function_name!()
    ));

    for (item, span) in syntax {
        let push = match item {
            Syntax::Opcode(opcode) => generate_opcode(opcode, span),
            Syntax::Bytes(bytes) => generate_bytes(bytes, span),
            Syntax::Int(int) => generate_int(int, span),
            Syntax::Escape(expression) => generate_escape(expression, span),
        };
        tokens.extend(push);
    }
    // tokens.extend(quote! {.analyze_stack()}); // for debug
    tokens
}

fn generate_opcode(opcode: u8, span: Span) -> TokenStream {
    quote_spanned!(span=>
            .push_opcode(#opcode)
    )
}

fn generate_bytes(bytes: Vec<u8>, span: Span) -> TokenStream {
    let mut slice = TokenStream::new();
    for byte in bytes {
        slice.extend(quote!(#byte,));
    }
    quote_spanned!(span=>.push_slice(&[#slice]))
}

fn generate_int(n: i64, span: Span) -> TokenStream {
    quote_spanned!(span=>.push_int(#n))
}

fn generate_escape(expression: TokenStream, span: Span) -> TokenStream {
    quote_spanned!(span=>
            .push_expression(#expression)
    )
}
