//! # Tondi Script Library
//! 
//! This library provides a high-level interface for building and manipulating
//! Tondi blockchain scripts. It includes a procedural macro for compile-time
//! script generation and a builder pattern for runtime script construction.
//! 
//! ## Features
//! - **Procedural Macro**: `script!` macro for compile-time script generation
//! - **Builder Pattern**: Fluent API for building scripts at runtime
//! - **Type Safety**: Compile-time validation of script structure
//! - **Performance**: Efficient script compilation and execution

/// Module containing the script builder implementation
/// Provides the StructuredScript type and related functionality for
/// constructing complex scripts with a fluent API
pub mod builder;

/// Re-export the main StructuredScript type as Script for convenience
/// This allows users to use `Script` instead of the longer `StructuredScript`
pub use crate::builder::StructuredScript as Script;

/// Re-export the procedural macro for script generation
/// The `script!` macro allows compile-time script construction with
/// a domain-specific language syntax
pub use script_macro::script;

/// Re-export the function_name macro from stdext
/// Provides access to the current function name at compile time
pub use stdext::function_name;
