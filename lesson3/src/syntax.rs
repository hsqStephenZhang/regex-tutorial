use std::fmt;

use anyhow::Result;

#[derive(Clone, Debug)]
pub enum SyntaxError {
    ErrParentheseNotMatch,
    ErrBraceNotMatch,
    ErrRepeatFormat,
}

// syntax tree(expr) -> instructions -> execute
#[derive(Clone, Debug, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Op {
    OpEmptyMatch = 0,
    OpAnyChar,
    Literal,
    Alternation,
    Concat,
    Star,
    Plus,
    Repeat,
    Question,
    Capture,

    Sep = 128,
    LeftP,
    VerticalChar,
}

#[derive(Clone, Debug)]
pub struct Regexp {
    pub op: Op,
    pub cap: usize,
    pub min: i64,
    pub max: i64,
    pub sub: Vec<Regexp>,
    pub char: char,
}

impl Regexp {
    pub fn new(op: Op) -> Self {
        Self {
            op,
            cap: 0,
            min: 0,
            max: 0,
            sub: Vec::new(),
            char: '\0',
        }
    }
}

#[derive(Clone, Debug)]
pub struct Parser {
    pub stack: Vec<Regexp>,
    pub num_capture: usize,
}

// print the syntax tree with a pretty format and indentations
impl fmt::Display for Regexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut stack = vec![(self, 0)];
        while let Some(pair) = stack.pop() {
            let r = pair.0;
            let indent = pair.1;
            for _ in 0..pair.1 {
                write!(f, "  ")?;
            }
            match r.op {
                Op::Literal => {
                    write!(f, "Literal: {}", r.char)?;
                    writeln!(f)?;
                }
                Op::Alternation => {
                    writeln!(f, "Alternation:")?;
                    for r in r.sub.iter().rev() {
                        stack.push((r, indent + 1));
                    }
                }
                Op::Concat => {
                    writeln!(f, "Concat:")?;
                    for r in r.sub.iter().rev() {
                        stack.push((r, indent + 1));
                    }
                }
                Op::Star => {
                    writeln!(f, "Star:")?;
                    for r in r.sub.iter().rev() {
                        stack.push((r, indent + 1));
                    }
                }
                Op::Plus => {
                    writeln!(f, "Plus:")?;
                    for r in r.sub.iter().rev() {
                        stack.push((r, indent + 1));
                    }
                }
                Op::Question => {
                    writeln!(f, "Question:")?;
                    for r in r.sub.iter().rev() {
                        stack.push((r, indent + 1));
                    }
                }
                Op::Capture => {
                    writeln!(f, "Capture:")?;
                    for r in r.sub.iter().rev() {
                        stack.push((r, indent + 1));
                    }
                }
                Op::OpEmptyMatch => {
                    writeln!(f, "OpEmptyMatch:")?;
                }
                Op::Sep => {
                    writeln!(f, "Sep:")?;
                }
                Op::LeftP => {
                    writeln!(f, "LeftP:")?;
                }
                Op::VerticalChar => {
                    writeln!(f, "VerticalChar:")?;
                }
                Op::OpAnyChar => {
                    writeln!(f, "OpAnyChar:")?;
                }
                Op::Repeat => {
                    writeln!(f, "Repeat, min:{}, max:{}", self.min, self.max)?;
                    for r in &r.sub {
                        stack.push((r, indent + 1));
                    }
                }
            }
        }
        Ok(())
    }
}

impl Parser {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            num_capture: 0,
        }
    }

    pub fn op(&mut self, op: Op) {
        let r = Regexp::new(op);
        self.stack.push(r);
    }

    pub fn literal(&mut self, c: char) {
        let mut r = Regexp::new(Op::Literal);
        r.char = c;
        self.stack.push(r);
    }

    pub fn op_with_num_cap(&mut self, op: Op, num_cap: usize) {
        let mut r = Regexp::new(op);
        r.cap = num_cap;
        self.stack.push(r);
    }

    pub fn repeat(&mut self, op: Op, min: i64, max: i64) {
        let mut r = Regexp::new(op);
        r.min = min;
        r.max = max;
        r.sub.push(self.stack.pop().unwrap());
        self.stack.push(r);
    }

    // return the index of the last Op that is VerticalChar/LeftP
    fn get_last_sep_op_index(&self) -> usize {
        let mut idx = self.stack.len();
        for (i, r) in self.stack.iter().enumerate().rev() {
            if r.op < Op::Sep {
                idx = i;
            } else {
                break;
            }
        }
        idx
    }

    pub fn parse_vertical_char(&mut self) {
        self.concat();

        if !self.swap_vertical_char() {
            self.op(Op::VerticalChar)
        }
    }

    pub fn collapse(subs: Vec<Regexp>, op: Op) -> Regexp {
        if subs.len() == 1 {
            return subs[0].clone();
        }

        let mut r = Regexp::new(op);
        r.sub.push(subs[0].clone());

        for regexp in subs.into_iter().skip(1) {
            if regexp.op == op {
                r.sub.extend(regexp.sub);
            } else {
                r.sub.push(regexp);
            }
        }

        if op == Op::Alternation {
            Self::build_trie_alternation();
        }
        r
    }

    pub fn concat(&mut self) {
        // string: abc|def|efg
        // we are at:     ^
        // and we should combine the literals of "def" together (the "abc|"" is already parsed)
        let sep_index = self.get_last_sep_op_index();
        let subs = self.stack.drain(sep_index..).collect::<Vec<_>>();

        if subs.len() == 0 {
            self.op(Op::OpEmptyMatch);
            return;
        }

        // TODO: handle the case that subs is empty
        let current_alt = Self::collapse(subs, Op::Concat);
        self.stack.push(current_alt);
    }

    pub fn alternate(&mut self) {
        let sep_index = self.get_last_sep_op_index();
        let subs = self.stack.drain(sep_index..).collect::<Vec<_>>();

        if subs.len() == 0 {
            self.op(Op::OpEmptyMatch);
            return;
        }

        let current_alt = Self::collapse(subs, Op::Alternation);
        self.stack.push(current_alt);
    }

    // transform the alternation into a trie by prefix
    // ABC|ABD|ABE -> AB(C|D|E)
    fn build_trie_alternation() {
        
    }

    // alwasy keep the Op::VerticalChar at the end of the op stack
    pub fn swap_vertical_char(&mut self) -> bool {
        let n = self.stack.len();
        // TODO: adjust the range to improve effieciency (by making sure the more complex regexp is at the end)

        if n >= 3 {
            // merge literals
        }

        if n >= 2 {
            if self.stack[self.stack.len() - 2].op == Op::VerticalChar {
                let last = self.stack.pop().unwrap();
                let second_last = self.stack.pop().unwrap();
                self.stack.push(last);
                self.stack.push(second_last);
                return true;
            }
        }
        false
    }

    pub fn parse_scope_end(&mut self) {
        self.concat();
        if self.swap_vertical_char() {
            self.stack.pop();
        }
        self.alternate();
    }

    pub fn parse_right_parenthese(&mut self) -> Result<(), SyntaxError> {
        self.parse_scope_end();

        if self.stack.len() < 2 || self.stack[self.stack.len() - 2].op != Op::LeftP {
            return Err(SyntaxError::ErrParentheseNotMatch);
        }

        if self.stack[self.stack.len() - 2].cap == 0 {
            // just for group
            let re1 = self.stack.pop().unwrap();
            let _ = self.stack.pop();
            self.stack.push(re1);
        } else {
            // capture
            let re1 = self.stack.pop().unwrap();
            let mut re2: Regexp = self.stack.pop().unwrap();
            re2.op = Op::Capture;
            re2.sub.resize(1, re1);
            self.stack.push(re2);
        }

        Ok(())

        // let re1 = self.stack.pop().unwrap();
        // let er2 = self.stack.pop().unwrap();
    }

    // parses {min} (max=min) or {min,} (max=-1) or {min,max}.
    // and returns the span of the next char after the repeat
    pub fn parse_repeat(&self, chars: &[char]) -> Result<(usize, i64, i64), SyntaxError> {
        let close_brace_index = chars.iter().position(|c: &char| *c == '}');
        let idx = match close_brace_index {
            Some(idx) => idx,
            None => return Err(SyntaxError::ErrBraceNotMatch),
        };

        let substr = &chars[..idx].iter().collect::<String>();

        // parse the substr in 3 formats: {min}, {min,}, {min,max}
        let parts = substr.split(',').collect::<Vec<_>>();

        let (min, max) = if parts.len() == 1 {
            let min_str = parts[0];
            let min_val = min_str
                .parse::<i64>()
                .map_err(|_| SyntaxError::ErrRepeatFormat)?;
            (min_val, min_val)
        } else if parts.len() == 2 {
            let min_str = parts[0];
            let min_val = min_str
                .parse::<i64>()
                .map_err(|_| SyntaxError::ErrRepeatFormat)?;

            let max_val = if parts[1] == "" {
                -1
            } else {
                parts[1]
                    .parse::<i64>()
                    .map_err(|_| SyntaxError::ErrRepeatFormat)?
            };
            (min_val, max_val)
        } else {
            return Err(SyntaxError::ErrRepeatFormat);
        };

        Ok((idx, min, max))
    }
}

pub fn parse(pattern: &str) -> Result<Parser, SyntaxError> {
    let chars = pattern.chars().collect::<Vec<_>>();
    let mut p = Parser::new();

    let mut idx = 0;

    loop {
        if idx >= chars.len() {
            break;
        }

        let c = chars[idx];
        match c {
            '(' => {
                // left parenthese
                p.num_capture += 1;
                p.op_with_num_cap(Op::LeftP, p.num_capture);
            }
            '|' => {
                // alternation
                p.parse_vertical_char();
            }
            ')' => {
                // right parenthese
                p.parse_right_parenthese()?;
            }
            '+' | '?' | '*' => {
                // repetition
                let op = match c {
                    '*' => Op::Star,
                    '+' => Op::Plus,
                    '?' => Op::Question,
                    _ => panic!("unreachable"),
                };
                p.repeat(op, 0, 0);
            }
            '.' => {
                // any character
                p.op(Op::OpAnyChar);
            }
            '{' => {
                // repeat starts
                let (advance_num, min, max) = p.parse_repeat(&chars[idx + 1..])?;
                p.repeat(Op::Repeat, min, max);
                idx += 2 + advance_num;
                continue;
            }
            // just ignore some escape characters
            c => {
                // if captures
                p.literal(c);
            }
        }
        idx += 1;
    }

    p.parse_scope_end();
    Ok(p)
}

pub fn simplify(parser: &mut Parser) {
    for r in parser.stack.iter_mut() {
        simplify_inner(r);
    }
}

// simplify the repeat into other expressions
pub fn simplify_inner(re: &mut Regexp) {
    // only handle repeat here
    if re.op != Op::Repeat {
        return;
    }

    let sub = re.sub[0].clone();

    // 1. x{n,}
    if re.max == -1 {
        // x{0,} -> x*
        if re.min == 0 {
            re.op = Op::Star;
            return;
        }

        // x{1,} -> x+
        if re.min == 1 {
            re.op = Op::Plus;
            return;
        }

        // x{n,} -> x{n}x*
        let mut prefix = Regexp::new(Op::Concat);
        prefix.sub = vec![sub.clone(); re.min as _];
        let mut suffix = Regexp::new(Op::Star);
        suffix.sub.push(sub);
        prefix.sub.push(suffix);

        std::mem::swap(re, &mut prefix);
        return;
    }

    // 2. x{0}
    if re.min == 0 && re.max == 0 {
        re.op = Op::OpEmptyMatch;
        re.sub.clear();
        return;
    }

    // 3. x{1}
    if re.min == 1 && re.max == 1 {
        let mut sub = re.sub.pop().unwrap();
        std::mem::swap(re, &mut sub);
        return;
    }

    // 4. x{n,m} -> x{n}(x(x)? ...)   (n may equal to m)

    let mut prefix = Regexp::new(Op::Concat);
    if re.min > 0 {
        prefix.sub = vec![sub.clone(); re.min as _];
    }

    if re.max > re.min {
        let mut suffix = Regexp::new(Op::Question);
        suffix.sub = vec![sub.clone()];

        for _ in re.min + 1..re.max {
            let mut r = Regexp::new(Op::Concat);
            r.sub = vec![sub.clone(), suffix.clone()];
            let mut new_suffix = Regexp::new(Op::Question);
            new_suffix.sub.push(r);
            suffix = new_suffix;
        }

        prefix.sub.push(suffix);
    } else {
        // re.max == re.min
        debug_assert_eq!(re.max, re.min);
    }

    std::mem::swap(re, &mut prefix);
}

#[test]
fn t1() {
    // let pattern = "abc|def|ghi";
    let pattern = "(abc|def)*ghi+";
    let parser = parse(pattern).unwrap();
    for r in parser.stack.iter() {
        println!("{}", r);
    }
}

#[test]
fn t2() {
    let patterns = ["a*", "a{4}", "a{4,}", "a{2,4}"];
    for pattern in patterns.iter() {
        let mut parser = parse(pattern).unwrap();
        simplify(&mut parser);
        for r in parser.stack.iter() {
            println!("{}", r);
        }
    }
}
