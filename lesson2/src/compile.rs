use anyhow::{anyhow, Result};
use std::collections::{HashMap, HashSet};

use crate::syntax::{parse, Op, Parser, Regexp};

#[derive(Clone, Debug)]
enum OpCode {
    Match,
    Char(char),
    // the number of instruction to jump
    // NOTICE: here the jump is relative
    Jmp(i64),
    // the numbers of two branches
    // NOTICE: here the jump is relative
    Split(i64, i64),
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

*/

fn compile(parser: &Parser) -> Result<Vec<Inst>> {
    assert_eq!(parser.stack.len(), 1);
    let mut insts = compile_once(&parser.stack[0])?;
    insts.push(Inst::new(OpCode::Match));
    Ok(insts)
}

fn compile_once(re: &Regexp) -> Result<Vec<Inst>> {
    let mut insts = Vec::new();
    match re.op {
        Op::OpEmptyMatch => {}
        Op::Literal => {
            for &c in &re.chars {
                insts.push(Inst::new(OpCode::Char(c)));
            }
        }
        Op::Alternation => {
            let branches = re
                .sub
                .iter()
                .filter_map(|re| compile_once(&re).map_err(|_| anyhow!("compile error")).ok())
                .collect::<Vec<_>>();
            if branches.len() < 2 {
                panic!("alternation should have at least two branches");
            }

            for window in branches.windows(2) {
                let first = &window[0];
                let second = &window[1];

                insts.push(Inst::new(OpCode::Split(1, (first.len() + 2) as _)));
                insts.extend(first.iter().cloned());
                insts.push(Inst::new(OpCode::Jmp(1 + second.len() as i64)));
            }
            insts.extend(branches.last().unwrap().iter().cloned());
        }
        Op::Plus | Op::Star | Op::Question => {
            let handler = REPEAT_HANDLERS.get(&re.op).unwrap();
            handler(re, &mut insts);
        }
        Op::Concat => {
            let all_parts = re
                .sub
                .iter()
                .filter_map(|re| compile_once(&re).map_err(|_| anyhow!("compile error")).ok())
                .flatten();
            insts.extend(all_parts);
        }
        Op::Capture => {
            // TODO: how to convert it as a group
            let all_parts = re
                .sub
                .iter()
                .filter_map(|re| compile_once(&re).map_err(|_| anyhow!("compile error")).ok())
                .flatten();
            insts.extend(all_parts);
        }
        _ => panic!("unreachable"),
    }

    Ok(insts)
}

lazy_static::lazy_static! {
    static ref REPEAT_HANDLERS: HashMap<Op, RepeatHandler> = {
        let mut m = HashMap::new();
        m.insert(Op::Question, e_quest as RepeatHandler);
        m.insert(Op::Star, e_star as RepeatHandler);
        m.insert(Op::Plus, e_plus as RepeatHandler);
        m
    };
}

type RepeatHandler = fn(&Regexp, &mut Vec<Inst>);

fn e_quest(re: &Regexp, insts: &mut Vec<Inst>) {
    let l1 = compile_once(&re.sub[0]).unwrap();
    insts.push(Inst::new(OpCode::Split(1, l1.len() as i64 + 1)));
    insts.extend(l1);
}

fn e_star(re: &Regexp, insts: &mut Vec<Inst>) {
    let l2_1 = compile_once(&re.sub[0]).unwrap();
    let l2_2 = Inst::new(OpCode::Jmp(-(l2_1.len() as i64 + 1)));
    insts.push(Inst::new(OpCode::Split(1, l2_1.len() as i64 + 2)));
    insts.extend(l2_1);
    insts.push(l2_2);
}

fn e_plus(re: &Regexp, insts: &mut Vec<Inst>) {
    let l1 = compile_once(&re.sub[0]).unwrap();
    let len1 = l1.len() as i64;
    insts.extend(l1);
    insts.push(Inst::new(OpCode::Split(-len1, 1)));
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
            println!(
                "round:{}: thread:{},  pc: {}, sp:{}, pc out of range",
                round, thread_id, pc, sp
            );
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
                if execute(
                    insts,
                    chars,
                    memory.clone(),
                    left as _,
                    sp,
                    round,
                    thread_id,
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
        }
        *round += 1;
    }
}

#[test]
fn test_parse_and_compile1() {
    /*
       Inst { op: Char('a') }
       Inst { op: Split(-1, 1) }
       Inst { op: Char('b') }
       Inst { op: Split(-1, 1) }
       Inst { op: Match }
    */
    let pattern = "a+b+";
    let parser = parse(pattern).unwrap();
    let insts = compile(&parser).unwrap();
    for inst in insts {
        println!("{:?}", inst);
    }
}

#[test]
fn test_parse_and_compile() {
    /*
       Inst { op: Split(1, 6) }
       Inst { op: Char('h') }
       Inst { op: Char('e') }
       Inst { op: Char('l') }
       Inst { op: Char('l') }
       Inst { op: Char('o') }
       Inst { op: Jmp(6) }
       Inst { op: Char('w') }
       Inst { op: Char('o') }
       Inst { op: Char('r') }
       Inst { op: Char('l') }
       Inst { op: Char('d') }
       Inst { op: Match }
    */
    let pattern = "hello|world";
    let parser = parse(pattern).unwrap();
    let insts = compile(&parser).unwrap();
    for inst in insts {
        println!("{:?}", inst);
    }
}

#[test]
fn test_parse_and_compile_and_execute() {
    fn test_util(pattern: &str, s: &str, expect_result: bool) {
        // pattern and the to be matched string
        let parser = parse(pattern).unwrap();
        let insts = compile(&parser).unwrap();
        let chars = s.chars().collect::<Vec<_>>();

        // context infos
        let memory = HashSet::new();
        let mut round = 0;
        let mut thread_id = 0;

        let r = execute(&insts, &chars, memory, 0, 0, &mut round, &mut thread_id);
        assert_eq!(r, expect_result);
    }

    // test_util("a+b+", "aab", true);
    // test_util("a+b+", "aabbbbb", true);
    // test_util("a+b+", "aabc", false);
    // test_util("hello|world", "hello", true);
    // test_util("hello|world", "world", true);
    // test_util("hello|world", "hellw", false);
    // test_util("(hell|worl)d", "hello", false);
    // test_util("(hell|worl)d", "helld", true);
    test_util("(hell|worl)*d(demo|damn)*", "hellworlhellddemodamn", true);
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
                    });
                    // continue the current thread
                    thread.pc = left as _;
                }
            }
        }
    }
    false
}

#[test]
fn t2() {
    fn test_util2(pattern: &str, s: &str, expect_result: bool) {
        // pattern and the to be matched string
        let parser = parse(pattern).unwrap();
        let insts = compile(&parser).unwrap();
        let chars = s.chars().collect::<Vec<_>>();

        // context infos
        let r = execute2(&insts, &chars);
        assert_eq!(r, expect_result);
    }

    test_util2("a+b+", "aab", true);
    test_util2("a+b+", "aabbbbb", true);
    test_util2("a+b+", "aabc", false);
}
