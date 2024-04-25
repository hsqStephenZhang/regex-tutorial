use anyhow::{anyhow, Result};
use bitflags::Flag;
use log::{debug, info};
use std::collections::{HashMap, HashSet};

use crate::{
    compile::{Inst, OpCode},
    syntax::{parse, simplify, Ast, CharClass, Flags, Node, Op},
};

// simply judge if the pattern matches the string
// but it won't return any details like capture groups
// if you wish, it's easy to add these infos to the return value
pub(crate) fn dfs(
    insts: &[Inst],
    chars: &[char],
    mut memory: HashSet<(usize, usize)>,
    mut pc: usize,
    mut sp: usize,
    round: &mut usize,
    thread_id: &mut usize,
    saved: &mut Vec<usize>,
) -> bool {
    loop {
        // avoid the endless loop
        if memory.contains(&(pc, sp)) {
            return false;
        }
        memory.insert((pc, sp));

        if pc >= insts.len() {
            debug!(
                "round:{}: thread:{},  pc: {}, sp:{}, pc out of range",
                round, thread_id, pc, sp
            );
            return false;
        }
        let inst = &insts[pc];
        // debug the execution process
        debug!(
            "round:{}: thread:{},  pc: {}, sp:{}, inst:{:?}",
            round, thread_id, pc, sp, inst
        );
        match &inst.op {
            OpCode::Match => return sp == chars.len(),
            OpCode::Char(c) => {
                // only check the validation sp when executing OpCode::Char
                if sp < chars.len() && *c == chars[sp] {
                    pc = pc + 1;
                    sp = sp + 1;
                } else {
                    return false;
                }
            }
            OpCode::CharClass(cc) => {
                if sp < chars.len() && cc.is_match(chars[sp]) {
                    pc = pc + 1;
                    sp = sp + 1;
                } else {
                    return false;
                }
            }
            OpCode::AnyChar => {
                if sp < chars.len() {
                    pc = pc + 1;
                    sp = sp + 1;
                } else {
                    return false;
                }
            }
            OpCode::Jmp(offset) => {
                let new_pc = pc as i64 + offset;
                if new_pc < 0 {
                    return false;
                }
                pc = new_pc as usize;
            }
            OpCode::Split(left, right) => {
                // recursive match
                *round += 1;
                let left = pc as i64 + left;
                if left < 0 {
                    return false;
                }
                if dfs(
                    insts,
                    chars,
                    memory.clone(),
                    left as _,
                    sp,
                    round,
                    thread_id,
                    saved,
                ) {
                    return true;
                }
                *thread_id += 1;
                let right = pc as i64 + right;
                if right < 0 {
                    return false;
                }
                pc = right as _;
            }
            OpCode::Save => {
                saved.push(sp);
                let r = dfs(
                    insts,
                    chars,
                    memory.clone(),
                    pc + 1,
                    sp,
                    round,
                    thread_id,
                    saved,
                );
                if !r {
                    saved.pop();
                }
                return r;
            }
        }
        *round += 1;
    }
}

#[derive(Debug, Clone)]
struct Thread {
    pc: usize,
    sp: usize,
    saved: Vec<usize>,
}

// similar to width first search
pub(crate) fn bfs(insts: &[Inst], chars: &[char]) -> bool {
    let mut runq: Vec<Thread> = Vec::new();
    runq.push(Thread {
        pc: 0,
        sp: 0,
        saved: Vec::new(),
    });
    while !runq.is_empty() {
        let mut thread = runq.pop().unwrap();
        loop {
            // the thread dies
            if thread.pc >= insts.len() {
                break;
            }

            let inst = &insts[thread.pc];

            match &inst.op {
                OpCode::Match => return thread.sp == chars.len(),
                OpCode::Char(c) => {
                    // only check the validation sp when executing OpCode::Char
                    if thread.sp < chars.len() && *c == chars[thread.sp] {
                        thread.pc += 1;
                        thread.sp += 1;
                    } else {
                        break;
                    }
                }
                OpCode::CharClass(cc) => {
                    // only check the validation sp when executing OpCode::Char
                    if thread.sp < chars.len() && cc.is_match(chars[thread.sp]) {
                        thread.pc += 1;
                        thread.sp += 1;
                    } else {
                        break;
                    }
                }
                OpCode::AnyChar => {
                    if thread.sp < chars.len() {
                        thread.pc += 1;
                        thread.sp += 1;
                    } else {
                        break;
                    }
                }
                OpCode::Jmp(offset) => {
                    let new_pc = thread.pc as i64 + offset;
                    if new_pc < 0 {
                        return false;
                    }
                    thread.pc = new_pc as usize;
                }
                OpCode::Split(left, right) => {
                    let left = thread.pc as i64 + left;
                    let right = thread.pc as i64 + right;
                    // actually, it should be a compile error
                    if right < 0 || left < 0 {
                        return false;
                    }
                    // recursive match
                    runq.push(Thread {
                        pc: right as _,
                        sp: thread.sp,
                        saved: thread.saved.clone(),
                    });
                    // continue the current thread
                    thread.pc = left as _;
                }
                OpCode::Save => {
                    let mut new_saved: Vec<usize> = thread.saved.clone();
                    new_saved.push(thread.sp);
                    runq.push(Thread {
                        pc: thread.pc + 1,
                        sp: thread.sp,
                        saved: new_saved,
                    });
                    break;
                }
            }
        }
    }
    false
}
