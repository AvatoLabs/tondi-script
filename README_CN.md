# Tondi Script Library

这是一个为Tondi区块链提供高级脚本接口的Rust库。该库包含一个过程宏用于编译时脚本生成，以及一个构建器模式用于运行时脚本构建。

## 功能特性

- **过程宏**: `script!` 宏用于编译时脚本生成
- **构建器模式**: 流畅的API用于运行时构建脚本
- **类型安全**: 编译时验证脚本结构
- **性能**: 高效的脚本编译和执行
- **完整支持**: 支持所有Tondi opcode和脚本操作

## 安装

在 `Cargo.toml` 中添加依赖：

```toml
[dependencies]
tondi-script = { path = "path/to/tondi-script" }
```

## 使用方法

### 基本脚本创建

```rust
use tondi_script::{script, Script};

// 创建简单的脚本
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

### 条件脚本生成

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

### 循环脚本生成

```rust
let count = 3;
let script = script! {
    for _ in 0..count {
        OpAdd
    }
};
```

### 动态数据插入

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

## 语法说明

### Opcode

所有Tondi opcode都可用，支持以下格式：
- `OpCheckSig` - 标准格式
- `CheckSig` - 无前缀格式（自动识别）
- `TRUE` / `FALSE` - 布尔值别名

### 整数字面量

支持正负64位整数，自动选择最高效的编码：
- `2` → `Op2` (0x52)
- `0` → `OpFalse` (0x00)
- `-1` → `Op1Negate` (0x4f)

### 十六进制字面量

以 `0x` 前缀的十六进制字符串：
```rust
let script = script! {
    0x0102030405060708090a0b0c0d0e0f
    OpSHA256
};
```

### 转义序列

支持Rust表达式插入，用尖括号包围：
```rust
let bytes = vec![1, 2, 3, 4];
let script = script! {
    <bytes>
    OpCheckSigVerify
    <2016 * 5>
    OpCheckLockTimeVerify
};
```

## 支持的数据类型

- `i64`, `i32`, `u32`, `usize`
- `Vec<u8>`
- `tondi_wallet_keys::PublicKey`
- `tondi_wallet_keys::XOnlyPublicKey`
- `Script` / `StructuredScript`

## 性能特性

- **编译时优化**: 脚本在编译时生成，运行时零开销
- **内存效率**: 智能的内存管理和脚本缓存
- **结构化脚本**: 支持脚本复用和嵌套

## 与Tondi核心的关系

该库是对Tondi核心脚本功能的增强封装，提供了：
- 更友好的API接口
- 编译时脚本验证
- 高级脚本构建功能
- 类型安全的脚本操作

## 许可证

MIT License

## 贡献

欢迎提交Issue和Pull Request来改进这个项目。
