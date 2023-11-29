#![allow(dead_code)]

use anyhow::{anyhow, Result};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
enum OpCode {
    Match,
    Char(char),
    // the number of instruction to jump
    Jmp(usize),
    // the numbers of two branches
    Split(usize, usize),
}

#[derive(Clone, Debug)]
struct Inst {
    op: OpCode,
}

impl Inst {
    pub fn new(op: OpCode) -> Self {
        Self { op }
    }
}

/*

a		char a

e1e2	codes for e1
        codes for e2

e1|e2	split L1, L2
        L1: codes for e1
        jmp L3
        L2: codes for e2
        L3:

e?		split L1, L2
        L1: codes for e
        L2:

e*		L1: split L2, L3
        L2: codes for e
        jmp L1
        L3:

e+		L1: codes for e
        split L1, L3
        L3:

e

let's just take a-zA-Z0-9 into considration, and ignore the special characters
thus, the compile method can also be simplified

*/

// compile the pattern into the instruction set directly(without syntax tree)
/// simplified
fn compile(pattern: &str) -> Result<Vec<Inst>> {
    let mut insts = Vec::new();

    let mut table: HashMap<char, fn(Vec<Inst>) -> Result<Vec<Inst>>> = HashMap::new();
    let (f1, f2, f3) = (e_quest, e_star, e_plus);
    table.insert('?', f1);
    table.insert('*', f2);
    table.insert('+', f3);

    for (idx, c) in pattern.chars().enumerate() {
        match c {
            '?' | '*' | '+' => {
                let func = table.get(&c).ok_or(anyhow!(""))?;
                insts = func(insts).map_err(|e| anyhow!("err: {} at idx:{}", e, idx))?;
            }
            c => {
                insts.push(Inst::new(OpCode::Char(c)));
            }
        }
    }
    insts.push(Inst::new(OpCode::Match));

    Ok(insts)
}

fn e_quest(mut insts: Vec<Inst>) -> Result<Vec<Inst>> {
    // pop the char and transfer it into the e+/e*/e? instructions

    let should_pop = insts
        .last()
        .map(|inst| {
            if let OpCode::Char(_) = inst.op {
                true
            } else {
                false
            }
        })
        .unwrap_or(false);
    if should_pop {
        let origin = insts.pop().unwrap();
        let n = insts.len();
        let i1 = Inst::new(OpCode::Split(n + 1, n + 2));
        let i2 = origin;
        insts.push(i1);
        insts.push(i2);
    } else {
        anyhow::bail!("syntax error")
    }
    Ok(insts)
}

fn e_star(mut insts: Vec<Inst>) -> Result<Vec<Inst>> {
    let should_pop = insts
        .last()
        .map(|inst| {
            if let OpCode::Char(_) = inst.op {
                true
            } else {
                false
            }
        })
        .unwrap_or(false);
    if should_pop {
        let origin = insts.pop().unwrap();
        let n = insts.len();
        let i1 = Inst::new(OpCode::Split(n + 2, n + 3));
        let i2 = origin;
        let i3: Inst = Inst::new(OpCode::Jmp(n + 1));
        insts.push(i1);
        insts.push(i2);
        insts.push(i3);
    } else {
        anyhow::bail!("syntax error")
    }
    Ok(insts)
}

fn e_plus(mut insts: Vec<Inst>) -> Result<Vec<Inst>> {
    let should_pop = insts
        .last()
        .map(|inst| {
            if let OpCode::Char(_) = inst.op {
                true
            } else {
                false
            }
        })
        .unwrap_or(false);
    if should_pop {
        let origin = insts.pop().unwrap();
        let n = insts.len();
        let i1 = origin;
        let i2 = Inst::new(OpCode::Split(n, n + 2));
        insts.push(i1);
        insts.push(i2);
    } else {
        anyhow::bail!("syntax error")
    }
    Ok(insts)
}

// similar to depth first search
fn execute(
    insts: &[Inst],
    chars: &[char],
    mut memory: HashSet<(usize, usize)>,
    mut pc: usize,
    mut sp: usize,
    round: &mut usize,
    thread_id: &mut usize,
) -> bool {
    loop {
        // avoid the endless loop
        if memory.contains(&(pc, sp)) {
            return false;
        }
        memory.insert((pc, sp));

        if pc >= insts.len() {
            return false;
        }
        let inst = &insts[pc];
        // debug the execution process
        println!(
            "round:{}: thread:{},  pc: {}, sp:{}, inst:{:?}",
            round, thread_id, pc, sp, inst
        );
        match inst.op {
            OpCode::Match => return sp == chars.len(),
            OpCode::Char(c) => {
                // only check the validation sp when executing OpCode::Char
                if sp < chars.len() && c == chars[sp] {
                    pc = pc + 1;
                    sp = sp + 1;
                } else {
                    return false;
                }
            }
            OpCode::Jmp(new_pc) => {
                pc = new_pc;
            }
            OpCode::Split(left, right) => {
                // recursive match
                *round += 1;
                if execute(insts, chars, memory.clone(), left, sp, round, thread_id) {
                    return true;
                }
                *thread_id += 1;
                pc = right;
            }
        }
        *round += 1;
    }
}

#[derive(Debug, Clone)]
struct Thread {
    pc: usize,
    sp: usize,
}

// similar to width first search
fn execute2(insts: &[Inst], chars: &[char]) -> bool {
    let mut runq: Vec<Thread> = Vec::new();
    runq.push(Thread { pc: 0, sp: 0 });
    while !runq.is_empty() {
        let mut thread = runq.pop().unwrap();
        loop {
            // the thread dies
            if thread.pc >= insts.len() {
                break;
            }

            let inst = &insts[thread.pc];

            match inst.op {
                OpCode::Match => return thread.sp == chars.len(),
                OpCode::Char(c) => {
                    // only check the validation sp when executing OpCode::Char
                    if thread.sp < chars.len() && c == chars[thread.sp] {
                        thread.pc += 1;
                        thread.sp += 1;
                    } else {
                        break;
                    }
                }
                OpCode::Jmp(new_pc) => {
                    thread.pc = new_pc;
                }
                OpCode::Split(left, right) => {
                    // recursive match
                    runq.push(Thread {
                        pc: right,
                        sp: thread.sp,
                    });
                    // continue the current thread
                    thread.pc = left;
                }
            }
        }
    }
    false
}

fn test_util(pattern: &str, s: &str, expect_result: bool) {
    // pattern and the to be matched string
    let pattern = pattern;
    let insts = compile(pattern).unwrap();
    let chars = s.chars().collect::<Vec<_>>();

    // context infos
    let memory = HashSet::new();
    let mut round = 0;
    let mut thread_id = 0;

    let r = execute(&insts, &chars, memory, 0, 0, &mut round, &mut thread_id);
    assert_eq!(r, expect_result);
}

fn test_util2(pattern: &str, s: &str, expect_result: bool) {
    // pattern and the to be matched string
    let pattern = pattern;
    let insts = compile(pattern).unwrap();
    let chars = s.chars().collect::<Vec<_>>();

    // context infos
    let r = execute2(&insts, &chars);
    assert_eq!(r, expect_result);
}

#[test]
fn t1() {
    test_util("a+b+", "aab", true);
    test_util("a+b+", "aabbbbb", true);
    test_util("a+b+", "aabc", false);
}

#[test]
fn t2() {
    test_util2("a+b+", "aab", true);
    test_util2("a+b+", "aabbbbb", true);
    test_util2("a+b+", "aabc", false);
}

fn main() {}
