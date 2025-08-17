use hex::FromHex;
use tondi_txscript::opcodes::codes::*;
use proc_macro2::{
    Delimiter, Span, TokenStream,
    TokenTree::{self, *},
};
use quote::quote;
use std::iter::Peekable;
use std::str::FromStr;
#[derive(Debug)]
pub enum Syntax {
    Opcode(u8),
    Escape(TokenStream),
    Bytes(Vec<u8>),
    Int(i64),
}
macro_rules! emit_error {
    ($span:expr, $($message:expr),*) => {{
        #[cfg(not(test))]
        proc_macro_error::emit_error!($span, $($message),*);
        #[cfg(test)]
        panic!($($message),*);
        #[allow(unreachable_code)]
        {
            panic!();
        }
    }}
}
macro_rules! abort {
    ($span:expr, $($message:expr),*) => {{
        #[cfg(not(test))]
        proc_macro_error::abort!($span, $($message),*);
        #[cfg(test)]
        panic!($($message),*);
    }}
}
/// Generates a function that parses a string into an opcode value.
macro_rules! generate_opcode_parser {
    ($($op:ident => $val:expr, $doc:expr);*) => {
        fn parse_opcode(s: &str) -> Result<u8, ()> {
            match s {
                // Special cases with aliases
                "Op0" => Ok(OpFalse),
                "OpTrue" | "TRUE" => Ok(OpTrue),
                "OpFalse" | "FALSE" => Ok(OpFalse),
                "Op1" => Ok(OpTrue),
                "OpNEG1" | "NEG1" => Ok(Op1Negate),
                "OpNOP2" | "NOP2" => Ok(OpCheckLockTimeVerify),
                "OpNOP3" | "NOP3" => Ok(OpCheckSequenceVerify),
                "Op2" => Ok(Op2),
                "Op3" => Ok(Op3),
                "Op4" => Ok(Op4),
                "Op5" => Ok(Op5),
                "Op6" => Ok(Op6),
                "Op7" => Ok(Op7),
                "Op8" => Ok(Op8),
                "Op9" => Ok(Op9),
                "Op10" => Ok(Op10),
                "Op11" => Ok(Op11),
                "Op12" => Ok(Op12),
                "Op13" => Ok(Op13),
                "Op14" => Ok(Op14),
                "Op15" => Ok(Op15),
                "Op16" => Ok(Op16),
                $(
                    // For all other opcodes, match both with and without Op prefix
                    s if s == stringify!($op) || s == &stringify!($op)[2..] => Ok($op),
                )*
                _ => Err(()),
            }
        }
    }
}
generate_opcode_parser! {
    OpFalse => 0x00, "Push an empty array onto the stack.";
    OpData1 => 0x01, "Push the next byte as an array onto the stack.";
    OpData2 => 0x02, "Push the next 2 bytes as an array onto the stack.";
    OpData3 => 0x03, "Push the next 3 bytes as an array onto the stack.";
    OpData4 => 0x04, "Push the next 4 bytes as an array onto the stack.";
    OpData5 => 0x05, "Push the next 5 bytes as an array onto the stack.";
    OpData6 => 0x06, "Push the next 6 bytes as an array onto the stack.";
    OpData7 => 0x07, "Push the next 7 bytes as an array onto the stack.";
    OpData8 => 0x08, "Push the next 8 bytes as an array onto the stack.";
    OpData9 => 0x09, "Push the next 9 bytes as an array onto the stack.";
    OpData10 => 0x0a, "Push the next 10 bytes as an array onto the stack.";
    OpData11 => 0x0b, "Push the next 11 bytes as an array onto the stack.";
    OpData12 => 0x0c, "Push the next 12 bytes as an array onto the stack.";
    OpData13 => 0x0d, "Push the next 13 bytes as an array onto the stack.";
    OpData14 => 0x0e, "Push the next 14 bytes as an array onto the stack.";
    OpData15 => 0x0f, "Push the next 15 bytes as an array onto the stack.";
    OpData16 => 0x10, "Push the next 16 bytes as an array onto the stack.";
    OpData17 => 0x11, "Push the next 17 bytes as an array onto the stack.";
    OpData18 => 0x12, "Push the next 18 bytes as an array onto the stack.";
    OpData19 => 0x13, "Push the next 19 bytes as an array onto the stack.";
    OpData20 => 0x14, "Push the next 20 bytes as an array onto the stack.";
    OpData21 => 0x15, "Push the next 21 bytes as an array onto the stack.";
    OpData22 => 0x16, "Push the next 22 bytes as an array onto the stack.";
    OpData23 => 0x17, "Push the next 23 bytes as an array onto the stack.";
    OpData24 => 0x18, "Push the next 24 bytes as an array onto the stack.";
    OpData25 => 0x19, "Push the next 25 bytes as an array onto the stack.";
    OpData26 => 0x1a, "Push the next 26 bytes as an array onto the stack.";
    OpData27 => 0x1b, "Push the next 27 bytes as an array onto the stack.";
    OpData28 => 0x1c, "Push the next 28 bytes as an array onto the stack.";
    OpData29 => 0x1d, "Push the next 29 bytes as an array onto the stack.";
    OpData30 => 0x1e, "Push the next 30 bytes as an array onto the stack.";
    OpData31 => 0x1f, "Push the next 31 bytes as an array onto the stack.";
    OpData32 => 0x20, "Push the next 32 bytes as an array onto the stack.";
    OpData33 => 0x21, "Push the next 33 bytes as an array onto the stack.";
    OpData34 => 0x22, "Push the next 34 bytes as an array onto the stack.";
    OpData35 => 0x23, "Push the next 35 bytes as an array onto the stack.";
    OpData36 => 0x24, "Push the next 36 bytes as an array onto the stack.";
    OpData37 => 0x25, "Push the next 37 bytes as an array onto the stack.";
    OpData38 => 0x26, "Push the next 38 bytes as an array onto the stack.";
    OpData39 => 0x27, "Push the next 39 bytes as an array onto the stack.";
    OpData40 => 0x28, "Push the next 40 bytes as an array onto the stack.";
    OpData41 => 0x29, "Push the next 41 bytes as an array onto the stack.";
    OpData42 => 0x2a, "Push the next 42 bytes as an array onto the stack.";
    OpData43 => 0x2b, "Push the next 43 bytes as an array onto the stack.";
    OpData44 => 0x2c, "Push the next 44 bytes as an array onto the stack.";
    OpData45 => 0x2d, "Push the next 45 bytes as an array onto the stack.";
    OpData46 => 0x2e, "Push the next 46 bytes as an array onto the stack.";
    OpData47 => 0x2f, "Push the next 47 bytes as an array onto the stack.";
    OpData48 => 0x30, "Push the next 48 bytes as an array onto the stack.";
    OpData49 => 0x31, "Push the next 49 bytes as an array onto the stack.";
    OpData50 => 0x32, "Push the next 50 bytes as an array onto the stack.";
    OpData51 => 0x33, "Push the next 51 bytes as an array onto the stack.";
    OpData52 => 0x34, "Push the next 52 bytes as an array onto the stack.";
    OpData53 => 0x35, "Push the next 53 bytes as an array onto the stack.";
    OpData54 => 0x36, "Push the next 54 bytes as an array onto the stack.";
    OpData55 => 0x37, "Push the next 55 bytes as an array onto the stack.";
    OpData56 => 0x38, "Push the next 56 bytes as an array onto the stack.";
    OpData57 => 0x39, "Push the next 57 bytes as an array onto the stack.";
    OpData58 => 0x3a, "Push the next 58 bytes as an array onto the stack.";
    OpData59 => 0x3b, "Push the next 59 bytes as an array onto the stack.";
    OpData60 => 0x3c, "Push the next 60 bytes as an array onto the stack.";
    OpData61 => 0x3d, "Push the next 61 bytes as an array onto the stack.";
    OpData62 => 0x3e, "Push the next 62 bytes as an array onto the stack.";
    OpData63 => 0x3f, "Push the next 63 bytes as an array onto the stack.";
    OpData64 => 0x40, "Push the next 64 bytes as an array onto the stack.";
    OpData65 => 0x41, "Push the next 65 bytes as an array onto the stack.";
    OpData66 => 0x42, "Push the next 66 bytes as an array onto the stack.";
    OpData67 => 0x43, "Push the next 67 bytes as an array onto the stack.";
    OpData68 => 0x44, "Push the next 68 bytes as an array onto the stack.";
    OpData69 => 0x45, "Push the next 69 bytes as an array onto the stack.";
    OpData70 => 0x46, "Push the next 70 bytes as an array onto the stack.";
    OpData71 => 0x47, "Push the next 71 bytes as an array onto the stack.";
    OpData72 => 0x48, "Push the next 72 bytes as an array onto the stack.";
    OpData73 => 0x49, "Push the next 73 bytes as an array onto the stack.";
    OpData74 => 0x4a, "Push the next 74 bytes as an array onto the stack.";
    OpData75 => 0x4b, "Push the next 75 bytes as an array onto the stack.";
    OpPushData1 => 0x4c, "Read the next byte as N; push the next N bytes as an array onto the stack.";
    OpPushData2 => 0x4d, "Read the next 2 bytes as N; push the next N bytes as an array onto the stack.";
    OpPushData4 => 0x4e, "Read the next 4 bytes as N; push the next N bytes as an array onto the stack.";
    Op1Negate => 0x4f, "Push the array `0x81` onto the stack.";
    OpReserved => 0x50, "Synonym for OpReturn.";
    OpTrue => 0x51, "Push the array `0x01` onto the stack.";
    Op2 => 0x52, "Push the array `0x02` onto the stack.";
    Op3 => 0x53, "Push the array `0x03` onto the stack.";
    Op4 => 0x54, "Push the array `0x04` onto the stack.";
    Op5 => 0x55, "Push the array `0x05` onto the stack.";
    Op6 => 0x56, "Push the array `0x06` onto the stack.";
    Op7 => 0x57, "Push the array `0x07` onto the stack.";
    Op8 => 0x58, "Push the array `0x08` onto the stack.";
    Op9 => 0x59, "Push the array `0x09` onto the stack.";
    Op10 => 0x5a, "Push the array `0x0a` onto the stack.";
    Op11 => 0x5b, "Push the array `0x0b` onto the stack.";
    Op12 => 0x5c, "Push the array `0x0c` onto the stack.";
    Op13 => 0x5d, "Push the array `0x0d` onto the stack.";
    Op14 => 0x5e, "Push the array `0x0e` onto the stack.";
    Op15 => 0x5f, "Push the array `0x0f` onto the stack.";
    Op16 => 0x60, "Push the array `0x10` onto the stack.";
    OpNop => 0x61, "Does nothing.";
    OpVer => 0x62, "Synonym for OpReturn.";
    OpIf => 0x63, "Pop and execute the next statements if a nonzero element was popped.";
    OpNotIf => 0x64, "Pop and execute the next statements if a zero element was popped.";
    OpVerIf => 0x65, "Fail the script unconditionally, does not even need to be executed.";
    OpVerNotIf => 0x66, "Fail the script unconditionally, does not even need to be executed.";
    OpElse => 0x67, "Execute statements if those after the previous OpIf were not, and vice-versa. \
             If there is no previous OpIf, this acts as a RETURN.";
    OpEndIf => 0x68, "Ends the if/else block.";
    OpVerify => 0x69, "If the top value is zero or the stack is empty, fail; otherwise, pop the stack.";
    OpReturn => 0x6a, "Fail the script immediately. (Must be executed.).";
    OpToAltStack => 0x6b, "Pop one element from the main stack onto the alt stack.";
    OpFromAltStack => 0x6c, "Pop one element from the alt stack onto the main stack.";
    Op2Drop => 0x6d, "Drops the top two stack items.";
    Op2Dup => 0x6e, "Duplicates the top two stack items as AB -> ABAB.";
    Op3Dup => 0x6f, "Duplicates the two three stack items as ABC -> ABCABC.";
    Op2Over => 0x70, "Copies the two stack items of items two spaces back to the front, as xxAB -> ABxxAB.";
    Op2Rot => 0x71, "Moves the two stack items four spaces back to the front, as xxxxAB -> ABxxxx.";
    Op2Swap => 0x72, "Swaps the top two pairs, as ABCD -> CDAB.";
    OpIfDup => 0x73, "Duplicate the top stack element unless it is zero.";
    OpDepth => 0x74, "Push the current number of stack items onto the stack.";
    OpDrop => 0x75, "Drops the top stack item.";
    OpDup => 0x76, "Duplicates the top stack item.";
    OpNip => 0x77, "Drops the second-to-top stack item.";
    OpOver => 0x78, "Copies the second-to-top stack item, as xA -> AxA.";
    OpPick => 0x79, "Pop the top stack element as N. Copy the Nth stack element to the top.";
    OpRoll => 0x7a, "Pop the top stack element as N. Move the Nth stack element to the top.";
    OpRot => 0x7b, "Rotate the top three stack items, as [top next1 next2] -> [next2 top next1].";
    OpSwap => 0x7c, "Swap the top two stack items.";
    OpTuck => 0x7d, "Copy the top stack item to before the second item, as [top next] -> [top next top].";
    OpCat => 0x7e, "Concatenates two arrays.";
    OpSubStr => 0x7f, "Returns a section of an array.";
    OpLeft => 0x80, "Keeps only bytes left of the specified point in an array.";
    OpRight => 0x81, "Keeps only bytes right of the specified point in an array.";
    OpSize => 0x82, "Pushes the length of the top stack item onto the stack.";
    OpInvert => 0x83, "Flips all of the bits in the input.";
    OpAnd => 0x84, "Boolean and between each bit in the inputs.";
    OpOr => 0x85, "Boolean or between each bit in the inputs.";
    OpXor => 0x86, "Boolean exclusive or between each bit in the inputs.";
    OpEqual => 0x87, "Pushes 1 if the inputs are exactly equal, 0 otherwise.";
    OpEqualVerify => 0x88, "Returns success if the inputs are exactly equal, failure otherwise.";
    OpReserved1 => 0x89, "Synonym for OpReturn.";
    OpReserved2 => 0x8a, "Synonym for OpReturn.";
    Op1Add => 0x8b, "Increment the top stack element in place.";
    Op1Sub => 0x8c, "Decrement the top stack element in place.";
    Op2Mul => 0x8d, "The input is multiplied by 2.";
    Op2Div => 0x8e, "The input is divided by 2.";
    OpNegate => 0x8f, "Multiply the top stack item by -1 in place.";
    OpAbs => 0x90, "Absolute value the top stack item in place.";
    OpNot => 0x91, "Map 0 to 1 and everything else to 0, in place.";
    Op0NotEqual => 0x92, "Map 0 to 0 and everything else to 1, in place.";
    OpAdd => 0x93, "Pop two stack items and push their sum.";
    OpSub => 0x94, "Pop two stack items and push the second minus the top.";
    OpMul => 0x95, "Pop two stack items and push their product.";
    OpDiv => 0x96, "Pop two stack items and push the second divided by the top.";
    OpMod => 0x97, "Pop two stack items and push the modulo.";
    OpLShift => 0x98, "Shifts the second item left by first item bits.";
    OpRShift => 0x99, "Shifts the second item right by first item bits.";
    OpBoolAnd => 0x9a, "Pop the top two stack items and push 1 if both are nonzero, else push 0.";
    OpBoolOr => 0x9b, "Pop the top two stack items and push 1 if either is nonzero, else push 0.";
    OpNumEqual => 0x9c, "Pop the top two stack items and push 1 if both are numerically equal, else push 0.";
    OpNumEqualVerify => 0x9d, "Pop the top two stack items and return success if both are numerically equal, else return failure.";
    OpNumNotEqual => 0x9e, "Pop the top two stack items and push 0 if both are numerically equal, else push 1.";
    OpLessThan => 0x9f, "Pop the top two items; push 1 if the second is less than the top, 0 otherwise.";
    OpGreaterThan => 0xa0, "Pop the top two items; push 1 if the second is greater than the top, 0 otherwise.";
    OpLessThanOrEqual => 0xa1, "Pop the top two items; push 1 if the second is <= the top, 0 otherwise.";
    OpGreaterThanOrEqual => 0xa2, "Pop the top two items; push 1 if the second is >= the top, 0 otherwise.";
    OpMin => 0xa3, "Pop the top two items; push the smaller.";
    OpMax => 0xa4, "Pop the top two items; push the larger.";
    OpWithin => 0xa5, "Pop the top three items; if the top is >= the second and < the third, push 1, otherwise push 0.";
    OpUnknown166 => 0xa6, "Reserved Undefined opcodes.";
    OpUnknown167 => 0xa7, "Reserved Undefined opcodes.";
    OpSHA256 => 0xa8, "Pop the top stack item and push its SHA256 hash.";
    OpCheckMultiSigECDSA => 0xa9, "ECDSA multisig verification.";
    OpBlake3 => 0xaa, "Pop the top stack item and push its Blake3 hash.";
    OpCheckSigECDSA => 0xab, "ECDSA signature verification.";
    OpCheckSig => 0xac, "Signature verification pushing 1/0 for success/failure.";
    OpCheckSigVerify => 0xad, "Signature verification returning success/failure.";
    OpCheckMultiSig => 0xae, "Multisig verification pushing 1/0 for success/failure.";
    OpCheckMultiSigVerify => 0xaf, "Multisig verification returning success/failure.";
    OpCheckLockTimeVerify => 0xb0, "Check lock time verification.";
    OpCheckSequenceVerify => 0xb1, "Check sequence verification.";
    // OpUnknown178 => 0xb2, "Reserved opcode.";
    // OpUnknown179 => 0xb3, "Reserved opcode.";
    // OpUnknown180 => 0xb4, "Reserved opcode.";
    // OpUnknown181 => 0xb5, "Reserved opcode.";
    // OpUnknown182 => 0xb6, "Reserved opcode.";
    // OpUnknown183 => 0xb7, "Reserved opcode.";
    // OpUnknown184 => 0xb8, "Reserved opcode.";
    // OpUnknown185 => 0xb9, "Reserved opcode.";
    // OpUnknown186 => 0xba, "Reserved opcode.";
    // OpUnknown187 => 0xbb, "Reserved opcode.";
    // OpUnknown188 => 0xbc, "Reserved opcode.";
    // OpUnknown189 => 0xbd, "Reserved opcode.";
    // OpUnknown190 => 0xbe, "Reserved opcode.";
    // OpUnknown191 => 0xbf, "Reserved opcode.";
    // OpUnknown192 => 0xc0, "Reserved opcode.";
    // OpUnknown193 => 0xc1, "Reserved opcode.";
    // OpUnknown194 => 0xc2, "Reserved opcode.";
    // OpUnknown195 => 0xc3, "Reserved opcode.";
    OpUnknown196 => 0xc4, "Reserved opcode.";
    OpUnknown197 => 0xc5, "Reserved opcode.";
    OpUnknown198 => 0xc6, "Reserved opcode.";
    // OpUnknown199 => 0xc7, "Reserved opcode.";
    // OpUnknown200 => 0xc8, "Reserved opcode.";
    // OpUnknown201 => 0xc9, "Reserved opcode.";
    // OpUnknown202 => 0xca, "Reserved opcode.";
    // OpUnknown203 => 0xcb, "Reserved opcode.";
    // OpUnknown204 => 0xcc, "Reserved opcode.";
    // OpUnknown205 => 0xcd, "Reserved opcode.";
    // OpUnknown206 => 0xce, "Reserved opcode.";
    // OpUnknown207 => 0xcf, "Reserved opcode.";
    // OpUnknown208 => 0xd0, "Reserved opcode.";
    // OpUnknown209 => 0xd1, "Reserved opcode.";
    // OpUnknown210 => 0xd2, "Reserved opcode.";
    // OpUnknown211 => 0xd3, "Reserved opcode.";
    // OpUnknown212 => 0xd4, "Reserved opcode.";
    // OpUnknown213 => 0xd5, "Reserved opcode.";
    // OpUnknown214 => 0xd6, "Reserved opcode.";
    // OpUnknown215 => 0xd7, "Reserved opcode.";
    // OpUnknown216 => 0xd8, "Reserved opcode.";
    // OpUnknown217 => 0xd9, "Reserved opcode.";
    // OpUnknown218 => 0xda, "Reserved opcode.";
    // OpUnknown219 => 0xdb, "Reserved opcode.";
    // OpUnknown220 => 0xdc, "Reserved opcode.";
    // OpUnknown221 => 0xdd, "Reserved opcode.";
    // OpUnknown222 => 0xde, "Reserved opcode.";
    // OpUnknown223 => 0xdf, "Reserved opcode.";
    // OpUnknown224 => 0xe0, "Reserved opcode.";
    // OpUnknown225 => 0xe1, "Reserved opcode.";
    // OpUnknown226 => 0xe2, "Reserved opcode.";
    // OpUnknown227 => 0xe3, "Reserved opcode.";
    // OpUnknown228 => 0xe4, "Reserved opcode.";
    // OpUnknown229 => 0xe5, "Reserved opcode.";
    // OpUnknown230 => 0xe6, "Reserved opcode.";
    // OpUnknown231 => 0xe7, "Reserved opcode.";
    // OpUnknown232 => 0xe8, "Reserved opcode.";
    // OpUnknown233 => 0xe9, "Reserved opcode.";
    // OpUnknown234 => 0xea, "Reserved opcode.";
    // OpUnknown235 => 0xeb, "Reserved opcode.";
    // OpUnknown236 => 0xec, "Reserved opcode.";
    // OpUnknown237 => 0xed, "Reserved opcode.";
    // OpUnknown238 => 0xee, "Reserved opcode.";
    // OpUnknown239 => 0xef, "Reserved opcode.";
    // OpUnknown240 => 0xf0, "Reserved opcode.";
    // OpUnknown241 => 0xf1, "Reserved opcode.";
    // OpUnknown242 => 0xf2, "Reserved opcode.";
    // OpUnknown243 => 0xf3, "Reserved opcode.";
    // OpUnknown244 => 0xf4, "Reserved opcode.";
    // OpUnknown245 => 0xf5, "Reserved opcode.";
    // OpUnknown246 => 0xf6, "Reserved opcode.";
    // OpUnknown247 => 0xf7, "Reserved opcode.";
    // OpUnknown248 => 0xf8, "Reserved opcode.";
    // OpUnknown249 => 0xf9, "Reserved opcode.";
    OpSmallInteger => 0xfa, "Reserved opcode.";
    OpPubKeys => 0xfb, "Reserved opcode.";
    OpUnknown252 => 0xfc, "Reserved opcode.";
    OpPubKeyHash => 0xfd, "Reserved opcode.";
    OpPubKey => 0xfe, "Reserved opcode.";
    OpInvalidOpCode => 0xff, "Synonym for OpReturn."
}
pub fn parse(tokens: TokenStream) -> Vec<(Syntax, Span)> {
    let mut tokens = tokens.into_iter().peekable();
    let mut syntax = Vec::with_capacity(2048);
    while let Some(token) = tokens.next() {
        let token_str = token.to_string();
        syntax.push(match (&token, token_str.as_ref()) {
            // Wrap for loops such that they return a Vec<ScriptBuf>
            (Ident(_), "for") => parse_for_loop(token, &mut tokens),
            // Wrap if-else statements such that they return a Vec<ScriptBuf>
            (Ident(_), "if") => parse_if(token, &mut tokens),
            // Replace DEBUG with OpReserved
            (Ident(_), "DEBUG") => (Syntax::Opcode(OpReserved), token.span()),
            // identifier, look up opcode
            (Ident(_), _) => match parse_opcode(&token_str) {
                Ok(opcode) => (Syntax::Opcode(opcode), token.span()),
                Err(_) => {
                    let span = token.span();
                    let mut pseudo_stream = TokenStream::from(token);
                    pseudo_stream.extend(TokenStream::from_str("()"));
                    (Syntax::Escape(pseudo_stream), span)
                }
            },
            (Group(inner), _) => {
                let escape = inner.stream().clone();
                (Syntax::Escape(escape), token.span())
            }
            // '<', start of escape (parse until first '>')
            (Punct(_), "<") => parse_escape(token, &mut tokens),
            // '~' start of escape (parse until the next '~') ignores '<' and '>'
            (Punct(_), "~") => parse_escape_extra(token, &mut tokens),
            // literal, push data (int or bytes)
            (Literal(_), _) => parse_data(token),
            // negative sign, parse negative int
            (Punct(_), "-") => parse_negative_int(token, &mut tokens),
            // anything else is invalid
            _ => abort!(token.span(), "unexpected token"),
        });
    }
    syntax
}
fn parse_if<T>(token: TokenTree, tokens: &mut Peekable<T>) -> (Syntax, Span)
where
    T: Iterator<Item = TokenTree>,
{
    // Use a Vec here to get rid of warnings when the variable is overwritten
    let mut escape = quote! {
        let mut script_var = bitcoin_script::Script::new("if");
    };
    escape.extend(std::iter::once(token.clone()));
    while let Some(if_token) = tokens.next() {
        match if_token {
            Group(block) if block.delimiter() == Delimiter::Brace => {
                let inner_block = block.stream();
                escape.extend(quote! {
                    {
                        script_var = script_var.push_env_script(script! {
                            #inner_block
                        });
                    }
                });
                match tokens.peek() {
                    Some(else_token) if else_token.to_string().as_str() == "else" => continue,
                    _ => break,
                }
            }
            _ => {
                escape.extend(std::iter::once(if_token));
                continue;
            }
        };
    }
    escape = quote! {
        {
            #escape;
            script_var
        }
    };
    (Syntax::Escape(escape), token.span())
}
fn parse_for_loop<T>(token: TokenTree, tokens: &mut T) -> (Syntax, Span)
where
    T: Iterator<Item = TokenTree>,
{
    let mut escape = quote! {
        let mut script_var = bitcoin_script::Script::new("for");
    };
    escape.extend(std::iter::once(token.clone()));
    for for_token in tokens.by_ref() {
        match for_token {
            Group(block) if block.delimiter() == Delimiter::Brace => {
                let inner_block = block.stream();
                escape.extend(quote! {
                    {
                        script_var = script_var.push_env_script(script !{
                            #inner_block
                        });
                    }
                    script_var
                });
                break;
            }
            _ => {
                escape.extend(std::iter::once(for_token));
                continue;
            }
        };
    }
    (Syntax::Escape(quote! { { #escape } }), token.span())
}
fn parse_escape<T>(token: TokenTree, tokens: &mut T) -> (Syntax, Span)
where
    T: Iterator<Item = TokenTree>,
{
    let mut escape = TokenStream::new();
    let mut span = token.span();
    loop {
        let token = tokens
            .next()
            .unwrap_or_else(|| abort!(token.span(), "unterminated escape"));
        let token_str = token.to_string();
        span = span.join(token.span()).unwrap_or(token.span());
        // end of escape
        if let (Punct(_), ">") = (&token, token_str.as_ref()) {
            break;
        }
        escape.extend(TokenStream::from(token));
    }
    (Syntax::Escape(escape), span)
}
fn parse_escape_extra<T>(token: TokenTree, tokens: &mut T) -> (Syntax, Span)
where
    T: Iterator<Item = TokenTree>,
{
    let mut escape = TokenStream::new();
    let mut span = token.span();
    loop {
        let token = tokens
            .next()
            .unwrap_or_else(|| abort!(token.span(), "unterminated escape"));
        let token_str = token.to_string();
        span = span.join(token.span()).unwrap_or(token.span());
        // end of escape
        if let (Punct(_), "~") = (&token, token_str.as_ref()) {
            break;
        }
        escape.extend(TokenStream::from(token));
    }
    (Syntax::Escape(escape), span)
}
fn parse_data(token: TokenTree) -> (Syntax, Span) {
    if token.to_string().starts_with("0x") {
        if token
            .to_string()
            .strip_prefix("0x")
            .unwrap_or_else(|| unreachable!())
            .trim_start_matches('0')
            .len()
            <= 8
        {
            parse_hex_int(token)
        } else {
            parse_bytes(token)
        }
    } else {
        parse_int(token, false)
    }
}
fn parse_bytes(token: TokenTree) -> (Syntax, Span) {
    let hex_bytes = &token.to_string()[2..];
    let bytes = Vec::<u8>::from_hex(hex_bytes).unwrap_or_else(|err| {
        emit_error!(token.span(), "invalid hex literal ({})", err);
    });
    (Syntax::Bytes(bytes), token.span())
}
fn parse_hex_int(token: TokenTree) -> (Syntax, Span) {
    let token_str = &token.to_string()[2..];
    let n: u32 = u32::from_str_radix(token_str, 16).unwrap_or_else(|err| {
        emit_error!(token.span(), "invalid hex string ({})", err);
    });
    (Syntax::Int(n as i64), token.span())
}
fn parse_int(token: TokenTree, negative: bool) -> (Syntax, Span) {
    let token_str = token.to_string();
    let n: i64 = token_str.parse().unwrap_or_else(|err| {
        emit_error!(token.span(), "invalid number literal ({})", err);
    });
    let n = if negative { -n } else { n };
    (Syntax::Int(n), token.span())
}
fn parse_negative_int<T>(token: TokenTree, tokens: &mut T) -> (Syntax, Span)
where
    T: Iterator<Item = TokenTree>,
{
    let fail = || {
        #[allow(unused_variables)]
        let span = token.span();
        emit_error!(
            span,
            "expected negative sign to be followed by number literal"
        );
    };
    let maybe_token = tokens.next();
    if let Some(token) = maybe_token {
        if let Literal(_) = token {
            parse_int(token, true)
        } else {
            fail()
        }
    } else {
        fail()
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use tondi_txscript::opcodes::all as opcodes;
    use quote::quote;
    macro_rules! test_opcode {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let syntax = parse(quote!($input));
                if let Syntax::Opcode(opcode) = &syntax[0].0 {
                    assert_eq!(*opcode, $expected);
                } else {
                    panic!("Expected Syntax::Opcode, got {:?}", syntax[0].0);
                }
            }
        };
    }
    macro_rules! test_invalid_opcode {
        ($name:ident, $input:expr) => {
            #[test]
            fn $name() {
                let syntax = parse(quote!($input));
                assert!(matches!(syntax[0].0, Syntax::Escape(_)));
            }
        };
    }
    #[test]
    fn parse_empty() {
        assert!(parse(quote!()).is_empty());
    }
    #[test]
    #[should_panic(expected = "unexpected token")]
    fn parse_unexpected_token() {
        parse(quote!(OpCheckSig &));
    }
    // Basic opcode tests
    test_opcode!(parse_op_0, Op0, OpFalse);
    test_opcode!(parse_op_false, FALSE, OpFalse);
    test_opcode!(parse_op_true, TRUE, OpTrue);
    test_opcode!(parse_op_checksig, OpCheckSig, OpCheckSig);
    test_opcode!(parse_op_sha256, OpSHA256, OpSHA256);
    // Test numeric opcodes
    test_opcode!(parse_op_1, Op1, OpTrue);
    test_opcode!(parse_op_2, Op2, Op2);
    test_opcode!(parse_op_3, Op3, Op3);
    test_opcode!(parse_op_16, Op16, Op16);
    // Test aliases
    test_opcode!(parse_checksig_no_prefix, CheckSig, OpCheckSig);
    test_opcode!(parse_sha256_no_prefix, SHA256, OpSHA256);
    // Test special cases
    test_opcode!(parse_nop2, OpNOP2, OpCheckLockTimeVerify);
    test_opcode!(parse_nop3, OpNOP3, OpCheckSequenceVerify);
    test_opcode!(parse_debug, DEBUG, OpReserved);
    // Test invalid opcodes
    test_invalid_opcode!(parse_invalid_opcode, InvalidOpCode);
    test_invalid_opcode!(parse_unknown_identifier, UNKNOWN);
    // Test complex scripts
    #[test]
    fn parse_complex_script() {
        let syntax = parse(quote! {
            OpDup OpSHA256 0x20 0x89abcdef89abcdef89abcdef89abcdef89abcdef89abcdef89abcdef89abcdef OpEqualVerify OpCheckSig
        });
        assert_eq!(syntax.len(), 6);
        assert!(matches!(syntax[0].0, Syntax::Opcode(OpDup)));
        assert!(matches!(syntax[1].0, Syntax::Opcode(OpSHA256)));
        assert!(matches!(syntax[2].0, Syntax::Int(32))); // 0x20 = 32
        assert!(matches!(syntax[3].0, Syntax::Bytes(_)));
        assert!(matches!(syntax[4].0, Syntax::Opcode(OpEqualVerify)));
        assert!(matches!(syntax[5].0, Syntax::Opcode(OpCheckSig)));
    }
    #[test]
    fn parse_p2pkh_script() {
        let syntax = parse(quote! {
            OpDup
            OpBlake3
            <pubkey_hash>
            OpEqualVerify
            OpCheckSig
        });
        assert_eq!(syntax.len(), 5);
        assert!(matches!(syntax[0].0, Syntax::Opcode(OpDup)));
        assert!(matches!(syntax[1].0, Syntax::Opcode(OpBlake3)));
        assert!(matches!(syntax[2].0, Syntax::Escape(_)));
        assert!(matches!(syntax[3].0, Syntax::Opcode(OpEqualVerify)));
        assert!(matches!(syntax[4].0, Syntax::Opcode(OpCheckSig)));
    }
    #[test]
    fn parse_opcodes() {
        let syntax = parse(quote!(OpCheckSig OpSHA256));
        if let Syntax::Opcode(opcode) = syntax[0].0 {
            assert_eq!(opcode, opcodes::OpCheckSig);
        } else {
            panic!();
        }
        if let Syntax::Opcode(opcode) = syntax[1].0 {
            assert_eq!(opcode, opcodes::OpSHA256);
        } else {
            panic!();
        }
    }
    #[test]
    #[should_panic(expected = "unterminated escape")]
    fn parse_unterminated_escape() {
        parse(quote!(OpCheckSig < abc));
    }
    #[test]
    fn parse_escape() {
        let syntax = parse(quote!(OpCheckSig<abc>));
        if let Syntax::Escape(tokens) = &syntax[1].0 {
            let tokens = tokens.clone().into_iter().collect::<Vec<TokenTree>>();
            assert_eq!(tokens.len(), 1);
            if let TokenTree::Ident(_) = tokens[0] {
                assert_eq!(tokens[0].to_string(), "abc");
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }
    #[test]
    #[should_panic(expected = "invalid number literal (invalid digit found in string)")]
    fn parse_invalid_int() {
        parse(quote!(OpCheckSig 12g34));
    }
    #[test]
    fn parse_int() {
        let syntax = parse(quote!(OpCheckSig 1234));
        if let Syntax::Int(n) = syntax[1].0 {
            assert_eq!(n, 1234i64);
        } else {
            panic!()
        }
    }
    #[test]
    #[should_panic(expected = "expected negative sign to be followed by number literal")]
    fn parse_invalid_negative_sign() {
        parse(quote!(OpCheckSig - OpSHA256));
    }
    #[test]
    fn parse_negative_int() {
        let syntax = parse(quote!(OpCheckSig - 1234));
        if let Syntax::Int(n) = syntax[1].0 {
            assert_eq!(n, -1234i64);
        } else {
            panic!()
        }
    }
    #[test]
    fn parse_hex() {
        let syntax = parse(quote!(OpCheckSig 0x123456789abcde));
        if let Syntax::Bytes(bytes) = &syntax[1].0 {
            assert_eq!(bytes, &vec![0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde]);
        } else {
            panic!("Unable to cast Syntax as Syntax::Bytes")
        }
    }
}