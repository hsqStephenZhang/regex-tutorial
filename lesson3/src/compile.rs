use anyhow::{anyhow, Result};
use bitflags::Flag;
use log::{debug, info};
use std::collections::{HashMap, HashSet};

use crate::syntax::{parse, simplify, Ast, CharClass, Flags, Node, Op};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OpCode {
    Match,
    Char(char),
    AnyChar,
    CharClass(CharClass),
    // the number of instruction to jump
    // NOTICE: here the jump is relative
    Jmp(i64),
    // the numbers of two branches
    // NOTICE: here the jump is relative
    Split(i64, i64),
    // one save instruction is corresponding to one parenthesis, but ignore the direction
    Save,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Inst {
    pub op: OpCode,
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

pub fn compile(parser: &Ast) -> Result<Vec<Inst>> {
    assert_eq!(parser.stack.len(), 1);
    let mut insts = compile_once(&parser.stack[0])?;
    insts.push(Inst::new(OpCode::Match));
    Ok(insts)
}

fn compile_once(re: &Node) -> Result<Vec<Inst>> {
    let mut insts = Vec::new();
    match re.op {
        Op::OpEmptyMatch => {}
        Op::Literal => {
            insts.push(Inst::new(OpCode::Char(re.char)));
        }
        Op::OpAnyChar => {
            insts.push(Inst::new(OpCode::AnyChar));
        }
        Op::CharClass => {
            insts.push(Inst::new(OpCode::CharClass(re.class.clone())));
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
            insts.push(Inst::new(OpCode::Save));
            insts.extend(all_parts);
            insts.push(Inst::new(OpCode::Save));
        }
        _ => panic!("unreachable inst:{:?}", re),
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

type RepeatHandler = fn(&Node, &mut Vec<Inst>);

fn e_quest(re: &Node, insts: &mut Vec<Inst>) {
    let l1 = compile_once(&re.sub[0]).unwrap();
    insts.push(if re.flag.contains(Flags::NON_GREEDY) {
        Inst::new(OpCode::Split(l1.len() as i64 + 1, 1))
    } else {
        Inst::new(OpCode::Split(1, l1.len() as i64 + 1))
    });
    insts.extend(l1);
}

fn e_star(re: &Node, insts: &mut Vec<Inst>) {
    let l2_1 = compile_once(&re.sub[0]).unwrap();
    let l2_2 = Inst::new(OpCode::Jmp(-(l2_1.len() as i64 + 1)));
    insts.push(if re.flag.contains(Flags::NON_GREEDY) {
        Inst::new(OpCode::Split(l2_1.len() as i64 + 2, 1))
    } else {
        Inst::new(OpCode::Split(1, l2_1.len() as i64 + 2))
    });
    insts.extend(l2_1);
    insts.push(l2_2);
}

fn e_plus(re: &Node, insts: &mut Vec<Inst>) {
    let l1 = compile_once(&re.sub[0]).unwrap();
    let len1 = l1.len() as i64;
    insts.extend(l1);
    insts.push(if re.flag.contains(Flags::NON_GREEDY) {
        Inst::new(OpCode::Split(1, -len1))
    } else {
        Inst::new(OpCode::Split(-len1, 1))
    });
}


// simply judge if the pattern matches the string
// but it won't return any details like capture groups
// if you wish, it's easy to add these infos to the return value
fn dfs(
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

#[cfg(test)]
mod test_compile {
    use crate::onepass::compile_onepass;

    use super::*;

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
        let actual = compile(&parser).unwrap();

        let expected = vec![
            Inst::new(OpCode::Char('a')),
            Inst::new(OpCode::Split(-1, 1)),
            Inst::new(OpCode::Char('b')),
            Inst::new(OpCode::Split(-1, 1)),
            Inst::new(OpCode::Match),
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_and_compile2() {
        /*
            Inst { op: Save }
            Inst { op: Char('a') }
            Inst { op: Split(-1, 1) }
            Inst { op: Save }
            Inst { op: Save }
            Inst { op: Char('b') }
            Inst { op: Split(-1, 1) }
            Inst { op: Save }
            Inst { op: Match }
        */
        let pattern = "(a+)(b+)";
        let parser = parse(pattern).unwrap();
        let actual = compile(&parser).unwrap();

        let expected = vec![
            Inst::new(OpCode::Save),
            Inst::new(OpCode::Char('a')),
            Inst::new(OpCode::Split(-1, 1)),
            Inst::new(OpCode::Save),
            Inst::new(OpCode::Save),
            Inst::new(OpCode::Char('b')),
            Inst::new(OpCode::Split(-1, 1)),
            Inst::new(OpCode::Save),
            Inst::new(OpCode::Match),
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_and_compile3() {
        /*
           Inst { op: Split(1, 7) }
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
        let actual = compile(&parser).unwrap();

        let expected = vec![
            Inst::new(OpCode::Split(1, 7)),
            Inst::new(OpCode::Char('h')),
            Inst::new(OpCode::Char('e')),
            Inst::new(OpCode::Char('l')),
            Inst::new(OpCode::Char('l')),
            Inst::new(OpCode::Char('o')),
            Inst::new(OpCode::Jmp(6)),
            Inst::new(OpCode::Char('w')),
            Inst::new(OpCode::Char('o')),
            Inst::new(OpCode::Char('r')),
            Inst::new(OpCode::Char('l')),
            Inst::new(OpCode::Char('d')),
            Inst::new(OpCode::Match),
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_and_compile4() {
        /*
            Inst { op: Char('a') }
            Inst { op: Char('a') }
            Inst { op: Split(1, 4) }
            Inst { op: Char('a') }
            Inst { op: Split(1, 2) }
            Inst { op: Char('a') }
            Inst { op: Match }
        */
        let pattern = "a{2,4}";
        let mut parser = parse(pattern).unwrap();
        simplify(&mut parser);
        let actual = compile(&parser).unwrap();

        let expected = vec![
            Inst::new(OpCode::Char('a')),
            Inst::new(OpCode::Char('a')),
            Inst::new(OpCode::Split(1, 4)),
            Inst::new(OpCode::Char('a')),
            Inst::new(OpCode::Split(1, 2)),
            Inst::new(OpCode::Char('a')),
            Inst::new(OpCode::Match),
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_and_compile5() {
        let pattern = "[a-z0-9A-Z]@gmail\\.com";
        let mut parser = parse(pattern).unwrap();
        simplify(&mut parser);
        let actual = compile(&parser).unwrap();

        let expected = vec![
            Inst::new(OpCode::CharClass(CharClass::new(
                vec![
                    ('0' as u32, '9' as u32),
                    ('A' as u32, 'Z' as u32),
                    ('a' as u32, 'z' as u32),
                ],
                false,
            ))),
            Inst::new(OpCode::Char('@')),
            Inst::new(OpCode::Char('g')),
            Inst::new(OpCode::Char('m')),
            Inst::new(OpCode::Char('a')),
            Inst::new(OpCode::Char('i')),
            Inst::new(OpCode::Char('l')),
            Inst::new(OpCode::CharClass(CharClass::new(
                vec![('.' as _, '.' as _)],
                false,
            ))),
            Inst::new(OpCode::Char('c')),
            Inst::new(OpCode::Char('o')),
            Inst::new(OpCode::Char('m')),
            Inst::new(OpCode::Match),
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_and_compile6() {
        let pattern = "a|ab|abc";
        let mut parser = parse(pattern).unwrap();
        simplify(&mut parser);
        let actual = compile(&parser).unwrap();

        let expected = vec![
            Inst::new(OpCode::Char('a')),
            Inst::new(OpCode::Split(1, 2)),
            Inst::new(OpCode::Jmp(5)),
            Inst::new(OpCode::Char('b')),
            Inst::new(OpCode::Split(1, 2)),
            Inst::new(OpCode::Jmp(2)),
            Inst::new(OpCode::Char('c')),
            Inst::new(OpCode::Match),
        ];
        assert_eq!(actual, expected);
    }
}

#[derive(Debug, Clone)]
struct Thread {
    pc: usize,
    sp: usize,
    saved: Vec<usize>,
}

// similar to width first search
fn bfs(insts: &[Inst], chars: &[char]) -> bool {
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

#[cfg(test)]
mod test_execute {

    use super::*;

    use std::sync::Once;
    static INIT: Once = Once::new();

    #[cfg(test)]
    pub fn setup_logger() -> () {
        INIT.call_once(|| {
            env_logger::builder()
                .filter_level(log::LevelFilter::Debug)
                .init();
        });
    }

    #[test]
    fn test_dfs() {
        setup_logger();
        fn test_util(pattern: &str, s: &str, expect_result: bool) {
            // pattern and the to be matched string
            let mut parser = parse(pattern).unwrap();
            simplify(&mut parser);
            let insts = compile(&parser).unwrap();
            let chars = s.chars().collect::<Vec<_>>();

            // context infos
            let memory = HashSet::new();
            let mut round = 0;
            let mut thread_id = 0;
            let mut saved = Vec::new();

            let r = dfs(
                &insts,
                &chars,
                memory,
                0,
                0,
                &mut round,
                &mut thread_id,
                &mut saved,
            );
            info!("captured groups: {:?}", saved);
            assert_eq!(r, expect_result);
        }

        // test_util("a+b+", "aab", true);
        // test_util("(a+)(b+)", "aab", true);
        // test_util("a+b+", "aabbbbb", true);
        // test_util("a+b+", "aabc", false);
        // test_util("hello|world", "hello", true);
        // test_util("hello|world", "world", true);
        // test_util("hello|world", "hellw", false);
        // test_util("(hell|worl)d", "hello", false);
        // test_util("(hell|worl)d", "helld", true);
        // test_util("(hell|worl)*d(demo|damn)*", "hellworlhellddemodamn", true);

        // test_util("a{2,4}", "aa", true);
        // test_util("a{2,4}", "aaa", true);
        // test_util("a{2,4}", "aaaa", true);
        // test_util("a{2,4}", "aaaaa", false);

        // test_util("(hello|world){2,4}", "helloworld", true);
        // test_util("(hello|world){2,4}", "helloworldhello", true);
        // test_util("(hello|world){2,4}", "helloworldhelloworld", true);
        // test_util("(hello|world){2,4}", "helloworldhelloworldhello", false);

        // test_util("a.", "ab", true);
        // test_util("a.c", "ab", false);
        // test_util("a.c", "ab", false);
        // test_util("(a.)+", "abacad", true);
        // test_util("(a.z)+", "abzaczadz", true);

        // test_util("[a-z0-9]+@gmail\\.com", "test@gmail.com", true);
        // test_util("[a-z0-9]+@gmail\\.com", "test@gmail@com", false);
        // test_util("[^\\D]+", "123456789", true);
        // test_util("[^\\D]+", "123456789a", false);
        // test_util("[^\\D]+a", "123456789a", true);

        test_util("a|ab|abc", "a", true);
        test_util("a|ab|abc", "ab", true);
        test_util("a|ab|abc", "abc", true);
        test_util("a|ab|abc", "abcd", false);
        test_util("hello(a|ab|abc)world", "helloabcworld", true);
        test_util("hello(a|ab|abc)world", "helloabcdworld", false);
    }

    #[test]
    fn t2() {
        setup_logger();
        fn test_util2(pattern: &str, s: &str, expect_result: bool) {
            // pattern and the to be matched string
            let mut parser = parse(pattern).unwrap();
            simplify(&mut parser);
            let insts = compile(&parser).unwrap();
            let chars = s.chars().collect::<Vec<_>>();

            // context infos
            let r = bfs(&insts, &chars);
            assert_eq!(r, expect_result);
        }

        test_util2("a+b+", "aab", true);
        test_util2("(a+)(b+)", "aab", true);
        test_util2("a+b+", "aabbbbb", true);
        test_util2("a+b+", "aabc", false);
        test_util2("hello|world", "hello", true);
        test_util2("hello|world", "world", true);
        test_util2("hello|world", "hellw", false);
        test_util2("(hell|worl)d", "hello", false);
        test_util2("(hell|worl)d", "helld", true);
        test_util2("(hell|worl)*d(demo|damn)*", "hellworlhellddemodamn", true);
    }
}
