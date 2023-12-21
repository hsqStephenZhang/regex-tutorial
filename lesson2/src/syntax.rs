use std::fmt;

use anyhow::Result;

#[derive(Clone, Debug)]
pub enum SyntaxError {
    ErrParentheseNotMatch,
}

// syntax tree(expr) -> instructions -> execute
#[derive(Clone, Debug, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Op {
    OpEmptyMatch = 0,
    Literal,
    Alternation,
    Concat,
    Star,
    Plus,
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
    pub min: usize,
    pub max: usize,
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
                    writeln!(f, "OpEmptyMatch")?;
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

    pub fn repeat(&mut self, op: Op, min: usize, max: usize) {
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
        // TODO: optimize the case that op==Op::ALternation
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
}

pub fn parse(pattern: &str) -> Result<Parser, SyntaxError> {
    let chars = pattern.chars().collect::<Vec<_>>();
    let mut p = Parser::new();

    // let mut
    for (_idx, &c) in chars.iter().enumerate() {
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
            // just ignore some escape characters
            c => {
                // if captures
                p.literal(c);
            }
        }
    }
    p.parse_scope_end();
    Ok(p)
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
