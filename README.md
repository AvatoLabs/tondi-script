# Tondi Script Library

A high-level scripting interface library for the Tondi blockchain, providing a procedural macro for compile-time script generation and a builder pattern for runtime script construction.

## Features

- **Procedural Macro**: `script!` macro for compile-time script generation
- **Builder Pattern**: Fluent API for building scripts at runtime
- **Type Safety**: Compile-time validation of script structure
- **Performance**: Efficient script compilation and execution
- **Complete Support**: Support for all Tondi opcodes and script operations

## Installation

Add the dependency to your `Cargo.toml`:

```toml
[dependencies]
tondi-script = { path = "path/to/tondi-script" }
```

## Usage

### Basic Script Creation

```rust
use tondi_script::{script, Script};

// Create a simple script
let script = script! {
    OpDup
    OpSHA256
    0x20
    0x89abcdef89abcdef89abcdef89abcdef89abcdef89abcdef89abcdef89abcdef
    OpEqualVerify
    OpCheckSig
};

let compiled_script = script.compile();
```

### Conditional Script Generation

```rust
let condition = true;
let script = script! {
    if condition {
        OpTrue
        OpCheckSig
    } else {
        OpFalse
        OpReturn
    }
};
```

### Loop Script Generation

```rust
let count = 3;
let script = script! {
    for _ in 0..count {
        OpAdd
    }
};
```

### Dynamic Data Insertion

```rust
let pubkey_hash = vec![0x12, 0x34, 0x56, 0x78];
let script = script! {
    OpDup
    OpBlake3
    { pubkey_hash }
    OpEqualVerify
    OpCheckSig
};
```

## Syntax

### Opcodes

All Tondi opcodes are available in the following formats:
- `OpCheckSig` - Standard format
- `CheckSig` - No prefix format (automatically recognized)
- `TRUE` / `FALSE` - Boolean aliases

### Integer Literals

Supports positive and negative 64-bit integers with automatic optimal encoding:
- `2` → `Op2` (0x52)
- `0` → `OpFalse` (0x00)
- `-1` → `Op1Negate` (0x4f)

### Hexadecimal Literals

Hexadecimal strings prefixed with `0x`:
```rust
let script = script! {
    0x0102030405060708090a0b0c0d0e0f
    OpSHA256
};
```

### Escape Sequences

Supports Rust expression insertion enclosed in angle brackets:
```rust
let bytes = vec![1, 2, 3, 4];
let script = script! {
    <bytes>
    OpCheckSigVerify
    <2016 * 5>
    OpCheckLockTimeVerify
};
```

## Supported Data Types

- `i64`, `i32`, `u32`, `usize`
- `Vec<u8>`
- `tondi_wallet_keys::PublicKey`
- `tondi_wallet_keys::XOnlyPublicKey`
- `Script` / `StructuredScript`

## Performance Features

- **Compile-time Optimization**: Scripts are generated at compile time with zero runtime overhead
- **Memory Efficiency**: Smart memory management and script caching
- **Structured Scripts**: Support for script reuse and nesting

## Relationship with Tondi Core

This library is an enhanced wrapper around Tondi's core script functionality, providing:
- More developer-friendly API interfaces
- Compile-time script validation
- Advanced script building capabilities
- Type-safe script operations

## Project Structure

```
tondi-script/
├── Cargo.toml          # Workspace configuration
├── macro/              # Procedural macro crate
│   ├── Cargo.toml      # Macro dependencies
│   └── src/
│       ├── lib.rs      # Macro entry point
│       ├── parse.rs    # Script parsing logic
│       └── generate.rs # Code generation
├── src/
│   ├── lib.rs          # Main library exports
│   └── builder.rs      # Script builder implementation
└── tests/
    └── test.rs         # Test suite
```

## Examples

See the `tests/test.rs` file for comprehensive usage examples including:
- Basic script operations
- Conditional and loop constructs
- Performance benchmarks
- Complex script compositions

## License

MIT License

## Contributing

Issues and Pull Requests are welcome to improve this project.

## Chinese Documentation

For Chinese documentation, see [(README_CN.md](README_CN.md).
