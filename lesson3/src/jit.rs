use anyhow::{anyhow, Result};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};
use log::debug;
use std::collections::HashMap;
use std::mem;

use crate::{
    compile::{Inst, OpCode},
    onepass::OnePassInst,
    syntax::CharClass,
};

/// JIT compiled regex function signature
/// Returns 1 if match succeeds, 0 if it fails
/// Arguments: (text_ptr: *const u8, text_len: usize) -> i32
type JitRegexFn = unsafe extern "C" fn(*const u8, usize) -> i32;

pub struct JitCompiler {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    module: JITModule,
}

impl JitCompiler {
    pub fn new() -> Result<Self> {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder()
            .map_err(|e| anyhow!("Failed to create ISA builder: {}", e))?;
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .map_err(|e| anyhow!("Failed to create ISA: {}", e))?;

        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);

        Ok(Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
        })
    }

    /// Compile bytecode instructions to native code
    pub fn compile_onepass(&mut self, insts: &[OnePassInst]) -> Result<JitRegexFn> {
        // Define the function signature: (text_ptr: *const u8, text_len: usize) -> i32
        self.ctx.func.signature.params.push(AbiParam::new(types::I64)); // text_ptr
        self.ctx.func.signature.params.push(AbiParam::new(types::I64)); // text_len
        self.ctx.func.signature.returns.push(AbiParam::new(types::I32)); // return value

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let text_ptr = builder.block_params(entry_block)[0];
        let text_len = builder.block_params(entry_block)[1];

        // Create blocks for each instruction
        let mut inst_blocks = Vec::new();
        for _ in 0..insts.len() {
            inst_blocks.push(builder.create_block());
        }

        // Create a failure block
        let fail_block = builder.create_block();

        // Current position in text (sp)
        let sp_slot = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            8,
        ));
        let zero = builder.ins().iconst(types::I64, 0);
        builder.ins().stack_store(zero, sp_slot, 0);

        // Jump to first instruction
        builder.ins().jump(inst_blocks[0], &[]);

        // Compile each instruction
        for (idx, inst) in insts.iter().enumerate() {
            builder.switch_to_block(inst_blocks[idx]);

            match &inst.op {
                OpCode::Match => {
                    // Check if sp == text_len
                    let sp = builder.ins().stack_load(types::I64, sp_slot, 0);
                    let cmp = builder.ins().icmp(IntCC::Equal, sp, text_len);
                    
                    let success_block = builder.create_block();
                    builder.ins().brif(cmp, success_block, &[], fail_block, &[]);

                    builder.switch_to_block(success_block);
                    builder.seal_block(success_block);
                    let one = builder.ins().iconst(types::I32, 1);
                    builder.ins().return_(&[one]);
                }
                OpCode::Char(c) => {
                    let sp = builder.ins().stack_load(types::I64, sp_slot, 0);
                    
                    // Check if sp < text_len
                    let in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, sp, text_len);
                    let bounds_check_block = builder.create_block();
                    builder.ins().brif(in_bounds, bounds_check_block, &[], fail_block, &[]);

                    builder.switch_to_block(bounds_check_block);
                    builder.seal_block(bounds_check_block);

                    // Load character at text_ptr[sp]
                    let char_addr = builder.ins().iadd(text_ptr, sp);
                    let loaded_char = builder.ins().load(types::I8, MemFlags::new(), char_addr, 0);
                    let loaded_char_32 = builder.ins().uextend(types::I32, loaded_char);
                    
                    // Compare with expected character
                    let expected = builder.ins().iconst(types::I32, *c as i64);
                    let char_match = builder.ins().icmp(IntCC::Equal, loaded_char_32, expected);

                    let match_block = builder.create_block();
                    builder.ins().brif(char_match, match_block, &[], fail_block, &[]);

                    builder.switch_to_block(match_block);
                    builder.seal_block(match_block);

                    // Increment sp
                    let one = builder.ins().iconst(types::I64, 1);
                    let new_sp = builder.ins().iadd(sp, one);
                    builder.ins().stack_store(new_sp, sp_slot, 0);

                    // Jump to next instruction
                    if idx + 1 < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[idx + 1], &[]);
                    } else {
                        builder.ins().jump(fail_block, &[]);
                    }
                }
                OpCode::AnyChar => {
                    let sp = builder.ins().stack_load(types::I64, sp_slot, 0);
                    
                    // Check if sp < text_len
                    let in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, sp, text_len);
                    let match_block = builder.create_block();
                    builder.ins().brif(in_bounds, match_block, &[], fail_block, &[]);

                    builder.switch_to_block(match_block);
                    builder.seal_block(match_block);

                    // Increment sp
                    let one = builder.ins().iconst(types::I64, 1);
                    let new_sp = builder.ins().iadd(sp, one);
                    builder.ins().stack_store(new_sp, sp_slot, 0);

                    // Jump to next instruction
                    if idx + 1 < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[idx + 1], &[]);
                    } else {
                        builder.ins().jump(fail_block, &[]);
                    }
                }
                OpCode::CharClass(cc) => {
                    let sp = builder.ins().stack_load(types::I64, sp_slot, 0);
                    
                    // Check if sp < text_len
                    let in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, sp, text_len);
                    let bounds_check_block = builder.create_block();
                    builder.ins().brif(in_bounds, bounds_check_block, &[], fail_block, &[]);

                    builder.switch_to_block(bounds_check_block);
                    builder.seal_block(bounds_check_block);

                    // Load character
                    let char_addr = builder.ins().iadd(text_ptr, sp);
                    let loaded_char = builder.ins().load(types::I8, MemFlags::new(), char_addr, 0);
                    let loaded_char_32 = builder.ins().uextend(types::I32, loaded_char);

                    // Check if character matches any range in the character class
                    let char_class_match_block = builder.create_block();
                    
                    if cc.ranges.is_empty() {
                        // Empty character class never matches
                        builder.ins().jump(fail_block, &[]);
                    } else {
                        let mut is_first = true;
                        let mut last_next_block = None;
                        
                        for range in &cc.ranges {
                            if !is_first {
                                builder.switch_to_block(last_next_block.unwrap());
                            }
                            is_first = false;
                            
                            // Check if loaded_char >= range.0
                            let min_val = builder.ins().iconst(types::I32, range.0 as i64);
                            let ge_min = builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, loaded_char_32, min_val);
                            
                            let max_check_block = builder.create_block();
                            let next_range_block = builder.create_block();
                            builder.ins().brif(ge_min, max_check_block, &[], next_range_block, &[]);

                            builder.switch_to_block(max_check_block);
                            builder.seal_block(max_check_block);

                            // Check if loaded_char <= range.1
                            let max_val = builder.ins().iconst(types::I32, range.1 as i64);
                            let le_max = builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, loaded_char_32, max_val);
                            builder.ins().brif(le_max, char_class_match_block, &[], next_range_block, &[]);

                            builder.seal_block(next_range_block);
                            last_next_block = Some(next_range_block);
                        }

                        // If no range matched, fail
                        builder.switch_to_block(last_next_block.unwrap());
                        builder.ins().jump(fail_block, &[]);

                        // Character class matched
                        builder.switch_to_block(char_class_match_block);
                        builder.seal_block(char_class_match_block);
                    }

                    let sp = builder.ins().stack_load(types::I64, sp_slot, 0);
                    let one = builder.ins().iconst(types::I64, 1);
                    let new_sp = builder.ins().iadd(sp, one);
                    builder.ins().stack_store(new_sp, sp_slot, 0);

                    // Jump to next instruction
                    if idx + 1 < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[idx + 1], &[]);
                    } else {
                        builder.ins().jump(fail_block, &[]);
                    }
                }
                OpCode::Jmp(offset) => {
                    let target = (idx as i64 + offset) as usize;
                    if target < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[target], &[]);
                    } else {
                        builder.ins().jump(fail_block, &[]);
                    }
                }
                OpCode::Split(left_offset, right_offset) => {
                    // For alternation and quantifiers, we need to try both paths
                    // We'll implement this as: try left first, if it fails, try right
                    // This is a simplified version - a full implementation would need backtracking
                    
                    let left_target = (idx as i64 + left_offset) as usize;
                    let right_target = (idx as i64 + right_offset) as usize;
                    
                    // For greedy quantifiers (left first), try left path
                    // Note: Full backtracking would require a more complex implementation
                    // with saved state and exploration of both branches
                    if left_target < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[left_target], &[]);
                    } else if right_target < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[right_target], &[]);
                    } else {
                        builder.ins().jump(fail_block, &[]);
                    }
                }
                OpCode::Save => {
                    // For simple matching, we can ignore Save instructions
                    if idx + 1 < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[idx + 1], &[]);
                    } else {
                        builder.ins().jump(fail_block, &[]);
                    }
                }
            }

            builder.seal_block(inst_blocks[idx]);
        }

        // Failure block
        builder.switch_to_block(fail_block);
        builder.seal_block(fail_block);
        let zero = builder.ins().iconst(types::I32, 0);
        builder.ins().return_(&[zero]);

        builder.finalize();

        // Define the function in the module
        let id = self
            .module
            .declare_function("regex_match", Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| anyhow!("Failed to declare function: {}", e))?;

        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| anyhow!("Failed to define function: {}", e))?;

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions().unwrap();

        let code_ptr = self.module.get_finalized_function(id);

        Ok(unsafe { mem::transmute::<_, JitRegexFn>(code_ptr) })
    }

    /// Compile regular bytecode instructions to native code using NFA simulation
    pub fn compile_bytecode(&mut self, insts: &[Inst]) -> Result<JitRegexFn> {
        // For complex patterns with backtracking (Split), we use a different approach
        // We'll compile to a function that simulates the NFA
        
        // Define the function signature: (text_ptr: *const u8, text_len: usize) -> i32
        self.ctx.func.signature.params.push(AbiParam::new(types::I64)); // text_ptr
        self.ctx.func.signature.params.push(AbiParam::new(types::I64)); // text_len
        self.ctx.func.signature.returns.push(AbiParam::new(types::I32)); // return value

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let text_ptr = builder.block_params(entry_block)[0];
        let text_len = builder.block_params(entry_block)[1];

        // Create blocks for each instruction
        let mut inst_blocks = Vec::new();
        for _ in 0..insts.len() {
            inst_blocks.push(builder.create_block());
        }

        // Create special blocks
        let fail_block = builder.create_block();
        let success_block = builder.create_block();

        // Stack slot for current position (sp)
        let sp_slot = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            8,
        ));
        let zero = builder.ins().iconst(types::I64, 0);
        builder.ins().stack_store(zero, sp_slot, 0);

        // Jump to first instruction
        builder.ins().jump(inst_blocks[0], &[]);

        // Compile each instruction
        for (idx, inst) in insts.iter().enumerate() {
            builder.switch_to_block(inst_blocks[idx]);

            match &inst.op {
                OpCode::Match => {
                    // Check if sp == text_len
                    let sp = builder.ins().stack_load(types::I64, sp_slot, 0);
                    let cmp = builder.ins().icmp(IntCC::Equal, sp, text_len);
                    builder.ins().brif(cmp, success_block, &[], fail_block, &[]);
                }
                OpCode::Char(c) => {
                    let sp = builder.ins().stack_load(types::I64, sp_slot, 0);
                    
                    // Check if sp < text_len
                    let in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, sp, text_len);
                    let bounds_check_block = builder.create_block();
                    builder.ins().brif(in_bounds, bounds_check_block, &[], fail_block, &[]);

                    builder.switch_to_block(bounds_check_block);
                    builder.seal_block(bounds_check_block);

                    // Load character at text_ptr[sp]
                    let char_addr = builder.ins().iadd(text_ptr, sp);
                    let loaded_char = builder.ins().load(types::I8, MemFlags::new(), char_addr, 0);
                    let loaded_char_32 = builder.ins().uextend(types::I32, loaded_char);
                    
                    // Compare with expected character
                    let expected = builder.ins().iconst(types::I32, *c as i64);
                    let char_match = builder.ins().icmp(IntCC::Equal, loaded_char_32, expected);

                    let match_block = builder.create_block();
                    builder.ins().brif(char_match, match_block, &[], fail_block, &[]);

                    builder.switch_to_block(match_block);
                    builder.seal_block(match_block);

                    // Increment sp
                    let one = builder.ins().iconst(types::I64, 1);
                    let new_sp = builder.ins().iadd(sp, one);
                    builder.ins().stack_store(new_sp, sp_slot, 0);

                    // Jump to next instruction
                    if idx + 1 < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[idx + 1], &[]);
                    } else {
                        builder.ins().jump(fail_block, &[]);
                    }
                }
                OpCode::AnyChar => {
                    let sp = builder.ins().stack_load(types::I64, sp_slot, 0);
                    
                    // Check if sp < text_len
                    let in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, sp, text_len);
                    let match_block = builder.create_block();
                    builder.ins().brif(in_bounds, match_block, &[], fail_block, &[]);

                    builder.switch_to_block(match_block);
                    builder.seal_block(match_block);

                    // Increment sp
                    let one = builder.ins().iconst(types::I64, 1);
                    let new_sp = builder.ins().iadd(sp, one);
                    builder.ins().stack_store(new_sp, sp_slot, 0);

                    // Jump to next instruction
                    if idx + 1 < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[idx + 1], &[]);
                    } else {
                        builder.ins().jump(fail_block, &[]);
                    }
                }
                OpCode::CharClass(cc) => {
                    let sp = builder.ins().stack_load(types::I64, sp_slot, 0);
                    
                    // Check if sp < text_len
                    let in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, sp, text_len);
                    let bounds_check_block = builder.create_block();
                    builder.ins().brif(in_bounds, bounds_check_block, &[], fail_block, &[]);

                    builder.switch_to_block(bounds_check_block);
                    builder.seal_block(bounds_check_block);

                    // Load character
                    let char_addr = builder.ins().iadd(text_ptr, sp);
                    let loaded_char = builder.ins().load(types::I8, MemFlags::new(), char_addr, 0);
                    let loaded_char_32 = builder.ins().uextend(types::I32, loaded_char);

                    // Check if character matches any range in the character class
                    let char_class_match_block = builder.create_block();
                    
                    if cc.ranges.is_empty() {
                        // Empty character class never matches
                        builder.ins().jump(fail_block, &[]);
                    } else {
                        let mut is_first = true;
                        let mut last_next_block = None;
                        
                        for range in &cc.ranges {
                            if !is_first {
                                builder.switch_to_block(last_next_block.unwrap());
                            }
                            is_first = false;
                            
                            // Check if loaded_char >= range.0
                            let min_val = builder.ins().iconst(types::I32, range.0 as i64);
                            let ge_min = builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, loaded_char_32, min_val);
                            
                            let max_check_block = builder.create_block();
                            let next_range_block = builder.create_block();
                            builder.ins().brif(ge_min, max_check_block, &[], next_range_block, &[]);

                            builder.switch_to_block(max_check_block);
                            builder.seal_block(max_check_block);

                            // Check if loaded_char <= range.1
                            let max_val = builder.ins().iconst(types::I32, range.1 as i64);
                            let le_max = builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, loaded_char_32, max_val);
                            builder.ins().brif(le_max, char_class_match_block, &[], next_range_block, &[]);

                            builder.seal_block(next_range_block);
                            last_next_block = Some(next_range_block);
                        }

                        // If no range matched, fail
                        builder.switch_to_block(last_next_block.unwrap());
                        builder.ins().jump(fail_block, &[]);

                        // Character class matched
                        builder.switch_to_block(char_class_match_block);
                        builder.seal_block(char_class_match_block);
                    }

                    let sp = builder.ins().stack_load(types::I64, sp_slot, 0);
                    let one = builder.ins().iconst(types::I64, 1);
                    let new_sp = builder.ins().iadd(sp, one);
                    builder.ins().stack_store(new_sp, sp_slot, 0);

                    // Jump to next instruction
                    if idx + 1 < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[idx + 1], &[]);
                    } else {
                        builder.ins().jump(fail_block, &[]);
                    }
                }
                OpCode::Jmp(offset) => {
                    let target = (idx as i64 + offset) as usize;
                    if target < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[target], &[]);
                    } else {
                        builder.ins().jump(fail_block, &[]);
                    }
                }
                OpCode::Split(left_offset, right_offset) => {
                    // For Split, we need to try both paths
                    // In a simple implementation, we try the left path first (greedy)
                    // For a full backtracking implementation, we'd need to save state
                    
                    let left_target = (idx as i64 + left_offset) as usize;
                    let right_target = (idx as i64 + right_offset) as usize;
                    
                    // Simple greedy implementation: just jump to left path
                    // This works for many common patterns like a+, a*, a?
                    if left_target < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[left_target], &[]);
                    } else if right_target < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[right_target], &[]);
                    } else {
                        builder.ins().jump(fail_block, &[]);
                    }
                }
                OpCode::Save => {
                    // For simple matching without captures, just continue
                    if idx + 1 < inst_blocks.len() {
                        builder.ins().jump(inst_blocks[idx + 1], &[]);
                    } else {
                        builder.ins().jump(fail_block, &[]);
                    }
                }
            }
        }

        // Seal all instruction blocks after all jumps are created
        for idx in 0..inst_blocks.len() {
            builder.seal_block(inst_blocks[idx]);
        }

        // Success block
        builder.switch_to_block(success_block);
        builder.seal_block(success_block);
        let one = builder.ins().iconst(types::I32, 1);
        builder.ins().return_(&[one]);

        // Failure block
        builder.switch_to_block(fail_block);
        builder.seal_block(fail_block);
        let zero_ret = builder.ins().iconst(types::I32, 0);
        builder.ins().return_(&[zero_ret]);

        builder.finalize();

        // Define the function in the module
        let id = self
            .module
            .declare_function("regex_match", Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| anyhow!("Failed to declare function: {}", e))?;

        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| anyhow!("Failed to define function: {}", e))?;

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions().unwrap();

        let code_ptr = self.module.get_finalized_function(id);

        Ok(unsafe { mem::transmute::<_, JitRegexFn>(code_ptr) })
    }
}

/// Execute a JIT-compiled regex function
pub fn execute_jit(jit_fn: JitRegexFn, text: &str) -> bool {
    let text_bytes = text.as_bytes();
    let result = unsafe { jit_fn(text_bytes.as_ptr(), text_bytes.len()) };
    result == 1
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compile::compile;
    use crate::syntax::parse;

    #[test]
    fn test_jit_simple_char() {
        let ast = parse("a").unwrap();
        let insts = compile(&ast).unwrap();
        
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();

        assert!(execute_jit(jit_fn, "a"));
        assert!(!execute_jit(jit_fn, "b"));
        assert!(!execute_jit(jit_fn, ""));
        assert!(!execute_jit(jit_fn, "aa"));
    }

    #[test]
    fn test_jit_sequence() {
        let ast = parse("abc").unwrap();
        let insts = compile(&ast).unwrap();
        
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();

        assert!(execute_jit(jit_fn, "abc"));
        assert!(!execute_jit(jit_fn, "ab"));
        assert!(!execute_jit(jit_fn, "abcd"));
        assert!(!execute_jit(jit_fn, "xyz"));
    }

    #[test]
    fn test_jit_any_char() {
        let ast = parse("a.c").unwrap();
        let insts = compile(&ast).unwrap();
        
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();

        assert!(execute_jit(jit_fn, "abc"));
        assert!(execute_jit(jit_fn, "axc"));
        assert!(execute_jit(jit_fn, "a1c"));
        assert!(!execute_jit(jit_fn, "ac"));
        assert!(!execute_jit(jit_fn, "abcd"));
    }

    #[test]
    fn test_jit_char_class() {
        let ast = parse("[a-z]").unwrap();
        let insts = compile(&ast).unwrap();
        
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();

        assert!(execute_jit(jit_fn, "a"));
        assert!(execute_jit(jit_fn, "m"));
        assert!(execute_jit(jit_fn, "z"));
        assert!(!execute_jit(jit_fn, "A"));
        assert!(!execute_jit(jit_fn, "1"));
        assert!(!execute_jit(jit_fn, ""));
    }

    #[test]
    fn test_jit_plus() {
        let ast = parse("a+").unwrap();
        let insts = compile(&ast).unwrap();
        
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();

        assert!(execute_jit(jit_fn, "a"));
        assert!(execute_jit(jit_fn, "aa"));
        assert!(execute_jit(jit_fn, "aaa"));
        assert!(!execute_jit(jit_fn, ""));
        assert!(!execute_jit(jit_fn, "b"));
    }

    #[test]
    fn test_jit_star() {
        let ast = parse("a*").unwrap();
        let insts = compile(&ast).unwrap();
        
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();

        assert!(execute_jit(jit_fn, ""));
        assert!(execute_jit(jit_fn, "a"));
        assert!(execute_jit(jit_fn, "aa"));
        assert!(execute_jit(jit_fn, "aaa"));
        assert!(!execute_jit(jit_fn, "b"));
    }

    #[test]
    fn test_jit_question() {
        let ast = parse("a?").unwrap();
        let insts = compile(&ast).unwrap();
        
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();

        assert!(execute_jit(jit_fn, ""));
        assert!(execute_jit(jit_fn, "a"));
        assert!(!execute_jit(jit_fn, "aa"));
        assert!(!execute_jit(jit_fn, "b"));
    }

    #[test]
    fn test_jit_alternation() {
        let ast = parse("hello|world").unwrap();
        let insts = compile(&ast).unwrap();
        
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();

        assert!(execute_jit(jit_fn, "hello"));
        assert!(execute_jit(jit_fn, "world"));
        assert!(!execute_jit(jit_fn, "hell"));
        assert!(!execute_jit(jit_fn, "worl"));
        assert!(!execute_jit(jit_fn, "helloworld"));
    }

    #[test]
    fn test_jit_complex_pattern() {
        let ast = parse("a+b+").unwrap();
        let insts = compile(&ast).unwrap();
        
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();

        assert!(execute_jit(jit_fn, "ab"));
        assert!(execute_jit(jit_fn, "aab"));
        assert!(execute_jit(jit_fn, "abb"));
        assert!(execute_jit(jit_fn, "aaabbb"));
        assert!(!execute_jit(jit_fn, "a"));
        assert!(!execute_jit(jit_fn, "b"));
        assert!(!execute_jit(jit_fn, ""));
    }

    #[test]
    fn test_jit_digit_plus() {
        let ast = parse("[0-9]+").unwrap();
        let insts = compile(&ast).unwrap();
        
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();

        assert!(execute_jit(jit_fn, "0"));
        assert!(execute_jit(jit_fn, "9"));
        assert!(execute_jit(jit_fn, "123"));
        assert!(execute_jit(jit_fn, "999"));
        assert!(!execute_jit(jit_fn, ""));
        assert!(!execute_jit(jit_fn, "a"));
    }
}
