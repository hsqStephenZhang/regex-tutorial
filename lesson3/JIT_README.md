# JIT Compilation Feature

## Overview

This lesson now includes JIT (Just-In-Time) compilation support using Cranelift, which compiles regex patterns to native x86/ARM machine code for faster execution.

## What's New

### Dependencies Added
- `cranelift` - Core IR and code generation
- `cranelift-jit` - JIT compilation support
- `cranelift-module` - Module abstraction
- `cranelift-native` - Native ISA detection

### Features Supported

The JIT compiler supports the following regex features:

1. **Basic Matching**
   - Literal characters: `abc`
   - Any character (dot): `.`
   - Character classes: `[a-z]`, `[0-9]`, `[a-zA-Z0-9]`

2. **Quantifiers**
   - Plus (`+`): One or more - `a+`
   - Star (`*`): Zero or more - `a*`
   - Question (`?`): Zero or one - `a?`

3. **Alternation**
   - Pipe (`|`): `hello|world`

4. **Concatenation**
   - Multiple patterns: `a+b+`, `[0-9]+[a-z]*`

## Usage

```rust
use lesson3::compile::compile;
use lesson3::jit::{execute_jit, JitCompiler};
use lesson3::syntax::parse;

// Parse and compile the pattern
let ast = parse("a+b+").unwrap();
let insts = compile(&ast).unwrap();

// JIT compile to native code
let mut compiler = JitCompiler::new().unwrap();
let jit_fn = compiler.compile_bytecode(&insts).unwrap();

// Execute the JIT-compiled function
assert!(execute_jit(jit_fn, "aabbb"));
assert!(!execute_jit(jit_fn, "xyz"));
```

## Running Examples

Run the JIT demo to see various patterns in action:

```bash
cargo run --example jit_demo
```

Run the tests:

```bash
cargo test --lib jit
```

## Implementation Details

### Architecture

The JIT compiler translates regex bytecode instructions into native machine code:

- **OpCode::Char(c)** → Native character comparison and pointer advancement
- **OpCode::AnyChar** → Bounds check and pointer advancement
- **OpCode::CharClass** → Range checking with branch instructions
- **OpCode::Jmp** → Direct jump to target instruction block
- **OpCode::Split** → Greedy branching (tries left path first)
- **OpCode::Match** → Success return with value 1

### Code Generation Process

1. **Function Setup**: Create function with signature `(*const u8, usize) -> i32`
2. **Block Creation**: Pre-create basic blocks for each bytecode instruction
3. **State Management**: Use stack slot to track current position in text
4. **Instruction Translation**: Convert each bytecode instruction to Cranelift IR
5. **Finalization**: Compile IR to native code and return function pointer

### Performance

JIT compilation provides significant speedup for patterns that are executed many times:

- **Compilation overhead**: One-time cost when creating the JIT function
- **Runtime performance**: Near-native speed, eliminates bytecode interpretation overhead
- **Best for**: Patterns used repeatedly (e.g., in validation loops, text processing)

### Limitations

1. **Simplified Backtracking**: The current implementation uses greedy matching for `Split` instructions
   - Works well for most common patterns (`a+`, `a*`, `a?`, simple alternation)
   - May not handle complex alternations with full backtracking correctly

2. **No Capture Groups**: Currently focuses on matching only (returns boolean)

3. **Limited Optimization**: Basic translation without advanced optimizations like:
   - Loop unrolling
   - String matching algorithms (Boyer-Moore, etc.)
   - Instruction fusion

## Future Enhancements

Potential improvements:

1. **Full Backtracking**: Implement proper NFA simulation with saved states
2. **Capture Groups**: Return match positions and captured substrings
3. **Optimization Passes**: Add Cranelift optimization passes for better code
4. **Lazy Compilation**: Compile patterns on-demand
5. **Caching**: Cache compiled patterns by regex string
6. **DFA Mode**: For patterns without backtracking, generate DFA code

## Examples

The `jit_demo.rs` example demonstrates:
- Simple character matching
- Character classes
- Quantifiers (`+`, `*`, `?`)
- Alternation
- Complex patterns
- Performance benchmarks

See [`examples/jit_demo.rs`](examples/jit_demo.rs) for complete examples.
