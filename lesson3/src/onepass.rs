use std::collections::HashSet;

use log::debug;

use crate::{
    compile::{Inst, OpCode},
    syntax::Op,
};

type CharRange = (usize, usize);

#[derive(Debug, Clone)]
pub struct OnePassInst {
    /// the original opcode in `Inst`
    pub op: OpCode,
    /// the matched char range and the next inst's index
    pub next_inst: Vec<(CharRange, usize)>,
}

impl OnePassInst {
    // given the current char, return the next inst's index
    // if the char doesn't match any range, return None
    pub fn step(&self, cur: char) -> Option<usize> {
        match self
            .next_inst
            .iter()
            .find(|(range, _)| range.0 <= cur as usize && cur as usize <= range.1)
        {
            Some((_, next)) => Some(*next),
            None => None,
        }
    }
}

// merge non-overlapping nexts
// return None if the nexts are overlapping

pub fn try_merge_nexts(
    nexts1: &[(CharRange, usize)],
    nexts2: &[(CharRange, usize)],
    next1_dst: usize,
    next2_dst: usize,
) -> Option<Vec<(CharRange, usize)>> {
    let mut nexts = Vec::with_capacity(nexts1.len() + nexts2.len());
    let mut i = 0;
    let mut j = 0;
    while i < nexts1.len() && j < nexts2.len() {
        let (range1, _) = &nexts1[i];
        let (range2, _) = &nexts2[j];
        if range1.1 < range2.0 {
            nexts.push((range1.clone(), next1_dst));
            i += 1;
        } else if range2.1 < range1.0 {
            nexts.push((range2.clone(), next2_dst));
            j += 1;
        } else {
            return None;
        }
    }
    while i < nexts1.len() {
        let (range1, _) = &nexts1[i];
        nexts.push((range1.clone(), next1_dst));
        i += 1;
    }
    while j < nexts2.len() {
        let (range2, _) = &nexts2[j];
        nexts.push((range2.clone(), next2_dst));
        j += 1;
    }

    return Some(nexts);
}

// check if we can simplify the insts by one pass
// ignore the check first
pub(crate) fn compile_onepass(insts: Vec<Inst>) -> anyhow::Result<Vec<OnePassInst>> {
    // TODO: add checks, if not passed, return the original insts directly
    // for inst in &insts{
    //     if matches!(inst.op, OpCode::Split(off1, off2)){
    //         // if the split is the last instruction, we can remove it
    //         if inst == insts.last().unwrap(){
    //             insts.pop();
    //         }
    //     }
    // }
    let insts = copy_insts(insts);
    make_onepass(insts)
}

// 1. A:BC + B:CD => A:CD + B:CD
// 2. A:BC + B:AD => A:BC + B:CD
pub fn copy_insts(mut insts: Vec<Inst>) -> Vec<OnePassInst> {
    // make it a absolute address lookup
    for (idx, inst) in insts.iter_mut().enumerate() {
        match &mut inst.op {
            OpCode::Jmp(off) => {
                *off = idx as i64 + *off;
            }
            OpCode::Split(off1, off2) => {
                *off1 = idx as i64 + *off1;
                *off2 = idx as i64 + *off2;
            }
            _ => {}
        }
    }

    // for inst in &insts {
    //     println!("{:?}", inst);
    // }

    let mut i = 0;
    while i < insts.len() {
        let inst = &mut insts[i];

        if let OpCode::Split(off1, off2) = inst.op {
            let mut inst_b_idx = off1 as usize;
            let mut inst_b = insts[inst_b_idx].clone();
            let mut inst_c_idx = off2 as usize;
            let mut inst_c = insts[inst_c_idx].clone();
            if !matches!(inst_b.op, OpCode::Split(_, _))
                && !matches!(inst_c.op, OpCode::Split(_, _))
            {
                // doesn't need to handle
                i += 1;
                continue;
            } else if matches!(inst_b.op, OpCode::Split(_, _))
                && matches!(inst_c.op, OpCode::Split(_, _))
            {
                // too complex to handle
                i += 1;
                continue;
            } else if !matches!(inst_b.op, OpCode::Split(_, _))
                && matches!(inst_c.op, OpCode::Split(_, _))
            {
                // swap the inst_b and inst_c
                std::mem::swap(&mut inst_b_idx, &mut inst_c_idx);
                std::mem::swap(&mut inst_b, &mut inst_c);
            }

            if let OpCode::Split(off_c, off_d) = inst_b.op {
                let mut inst_c_idx_same = off_c as usize;
                let mut inst_d_idx = off_d as usize;
                if inst_d_idx == inst_c_idx {
                    std::mem::swap(&mut inst_c_idx_same, &mut inst_d_idx);
                }
                // 1. A:BC + B:CD => A:CD + B:CD
                if inst_c_idx == inst_c_idx_same {
                    println!("match rule1");
                    // insts[inst_b_idx].op = OpCode::Split(0, 0);
                    i += 1;
                    continue;
                }

                // 2. A:BC + B:AD => A:BC + B:AD
                let mut inst_a_idx_same = off_c as usize;
                let mut inst_d_idx = off_d as usize;
                if inst_d_idx == i {
                    std::mem::swap(&mut inst_a_idx_same, &mut inst_d_idx);
                }
                if inst_a_idx_same == i {
                    println!("match rule2");
                    // insts[inst_b_idx].op = OpCode::Split(0, 0);
                    i += 1;
                    continue;
                }
            }
        }

        i += 1;
    }
    return insts
        .into_iter()
        .map(|inst| OnePassInst {
            op: inst.op,
            next_inst: vec![],
        })
        .collect::<Vec<_>>();
}

fn make_onepass_helper(
    visited_pc: &mut HashSet<usize>,
    insts: &mut [OnePassInst],
    pc: usize,
) -> bool {
    if visited_pc.contains(&pc) {
        return true;
    }
    visited_pc.insert(pc);
    let mut ok = true;

    let mut inst = insts[pc].clone();
    match &mut inst.op {
        OpCode::Char(c) => {
            inst.next_inst = vec![((*c as usize, *c as usize), pc + 1)];
            insts[pc] = inst;
        }
        OpCode::AnyChar => {
            inst.next_inst = vec![((0, char::MAX as usize), pc + 1)];
            insts[pc] = inst;
        }
        OpCode::Split(off1, off2) => {
            if !(make_onepass_helper(visited_pc, insts, *off1 as usize)
                && make_onepass_helper(visited_pc, insts, *off2 as usize))
            {
                return false;
            }
            let inst_1 = insts[*off1 as usize].clone();
            let inst_2 = insts[*off2 as usize].clone();
            match try_merge_nexts(
                &inst_1.next_inst,
                &inst_2.next_inst,
                *off1 as usize,
                *off2 as usize,
            ) {
                Some(next_next) => {
                    inst.next_inst = next_next;
                    insts[pc] = inst;
                }
                None => {
                    ok = false;
                }
            }
        }
        OpCode::Jmp(off) => {
            if !make_onepass_helper(visited_pc, insts, *off as usize) {
                return false;
            }
            inst.next_inst = insts[*off as usize].next_inst.clone();
            for (_, next_pc) in &mut inst.next_inst {
                *next_pc = *off as usize;
            }
            insts[pc] = inst;
        }
        OpCode::Save => {
            make_onepass_helper(visited_pc, insts, pc + 1);
            inst.next_inst = insts[pc + 1].next_inst.clone();
            for (_, next_pc) in &mut inst.next_inst {
                *next_pc = pc + 1;
            }
            insts[pc] = inst;
        }
        OpCode::Match => {}
        OpCode::CharClass(char_class) => {
            if char_class.is_negative {
                char_class.flip();
            }
            inst.next_inst = char_class
                .ranges
                .iter()
                .map(|&r| ((r.0 as usize, r.1 as usize), pc + 1))
                .collect::<Vec<_>>();
            insts[pc] = inst;
        }
    }
    ok
}

fn make_onepass(mut insts: Vec<OnePassInst>) -> anyhow::Result<Vec<OnePassInst>> {
    let mut visited_pcs = HashSet::new();

    for pc in 0..insts.len() {
        if !make_onepass_helper(&mut visited_pcs, &mut insts, pc) {
            anyhow::bail!("failed to make onepass")
        }
    }
    Ok(insts)
}

// simply judge if the pattern matches the string
// but it won't return any details like capture groups
// if you wish, it's easy to add these infos to the return value
fn do_onepass(
    insts: &[OnePassInst],
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
        if sp == chars.len() && inst.op != OpCode::Match {
            return false;
        }
        match &inst.op {
            OpCode::Match => return sp == chars.len(),
            OpCode::Char(_c) => {
                // only check the validation sp when executing OpCode::Char
                if let Some(next_pc) = inst.step(chars[sp]) {
                    pc = next_pc;
                    sp = sp + 1;
                } else {
                    return false;
                }
            }
            OpCode::CharClass(_cc) => {
                if let Some(next_pc) = inst.step(chars[sp]) {
                    pc = next_pc;
                    sp = sp + 1;
                } else {
                    return false;
                }
            }
            OpCode::AnyChar => {
                if let Some(next_pc) = inst.step(chars[sp]) {
                    pc = next_pc;
                    sp = sp + 1;
                } else {
                    return false;
                }
            }
            OpCode::Jmp(_dst) => {
                if let Some(next_pc) = inst.step(chars[sp]) {
                    pc = next_pc;
                } else {
                    return false;
                }
            }
            OpCode::Split(_left, _right) => {
                if let Some(next_pc) = inst.step(chars[sp]) {
                    pc = next_pc;
                } else {
                    return false;
                }
            }
            OpCode::Save => {
                saved.push(sp);
                if let Some(next_pc) = inst.step(chars[sp]) {
                    pc = next_pc;
                } else {
                    return false;
                }
            }
        }
        *round += 1;
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        compile::compile,
        syntax::{parse, simplify},
    };

    use super::*;

    #[test]
    fn test_merge() {
        let nexts1 = vec![((0, 9), 1), ((11, char::MAX as usize), 2)];
        let nexts2 = vec![((10, 10), 3)];
        assert!(try_merge_nexts(&nexts1, &nexts2, 1, 2).is_some());

        let nexts2 = vec![((10, 12), 3)];
        assert!(try_merge_nexts(&nexts1, &nexts2, 1, 2).is_none());
    }

    #[test]
    fn test_onepass() {
        let pattern = "x*yx*";
        let mut parser = parse(pattern).unwrap();
        simplify(&mut parser);
        let actual = compile(&parser).unwrap();
        let expected = vec![
            Inst::new(OpCode::Split(1, 3)),
            Inst::new(OpCode::Char('x')),
            Inst::new(OpCode::Jmp(-2)),
            Inst::new(OpCode::Char('y')),
            Inst::new(OpCode::Split(1, 3)),
            Inst::new(OpCode::Char('x')),
            Inst::new(OpCode::Jmp(-2)),
            Inst::new(OpCode::Match),
        ];
        assert_eq!(actual, expected);
        let res = compile_onepass(actual);
        assert!(res.is_ok());
        let actual = res.unwrap();
        for (idx, inst) in actual.iter().enumerate() {
            println!("idx: {}, {:?}", idx, inst);
        }
    }

    /*
        idx: 0, OnePassInst { op: Split(1, 13), next_inst: [((97, 97), 1), ((98, 98), 1), ((99, 99), 13), ((120, 120), 1)] }
        idx: 1, OnePassInst { op: Save, next_inst: [((97, 97), 2), ((98, 98), 2), ((120, 120), 2)] }
        idx: 2, OnePassInst { op: Split(3, 10), next_inst: [((97, 97), 3), ((98, 98), 10), ((120, 120), 3)] }
        idx: 3, OnePassInst { op: Save, next_inst: [((97, 97), 4), ((120, 120), 4)] }
        idx: 4, OnePassInst { op: Split(5, 7), next_inst: [((97, 97), 7), ((120, 120), 5)] }
        idx: 5, OnePassInst { op: Char('x'), next_inst: [((120, 120), 6)] }
        idx: 6, OnePassInst { op: Jmp(4), next_inst: [((97, 97), 4), ((120, 120), 4)] }
        idx: 7, OnePassInst { op: Char('a'), next_inst: [((97, 97), 8)] }
        idx: 8, OnePassInst { op: Save, next_inst: [((97, 97), 9), ((98, 98), 9), ((120, 120), 9)] }
        idx: 9, OnePassInst { op: Jmp(2), next_inst: [((97, 97), 2), ((98, 98), 2), ((120, 120), 2)] }
        idx: 10, OnePassInst { op: Char('b'), next_inst: [((98, 98), 11)] }
        idx: 11, OnePassInst { op: Save, next_inst: [((97, 97), 12), ((98, 98), 12), ((99, 99), 12), ((120, 120), 12)] }
        idx: 12, OnePassInst { op: Jmp(0), next_inst: [((97, 97), 0), ((98, 98), 0), ((99, 99), 0), ((120, 120), 0)] }
        idx: 13, OnePassInst { op: Char('c'), next_inst: [((99, 99), 14)] }
        idx: 14, OnePassInst { op: Match, next_inst: [] }
    *
    */
    #[test]
    fn test_onepass2() {
        let pattern = "((x*a)*b)*c";
        let mut parser = parse(pattern).unwrap();
        simplify(&mut parser);
        let actual = compile(&parser).unwrap();
        let res = compile_onepass(actual);
        assert!(res.is_ok());
        let actual = res.unwrap();
        for (idx, inst) in actual.iter().enumerate() {
            println!("idx: {}, {:?}", idx, inst);
        }

        // context infos
        let memory = HashSet::new();
        let mut round = 0;
        let mut thread_id = 0;
        let mut saved = Vec::new();
        let chars = "xaxabxaxabc".chars().collect::<Vec<_>>();

        let r = do_onepass(
            &actual,
            &chars,
            memory,
            0,
            0,
            &mut round,
            &mut thread_id,
            &mut saved,
        );
        assert_eq!(r, true);
    }
}
