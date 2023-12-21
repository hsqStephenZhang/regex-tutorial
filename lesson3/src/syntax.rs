use std::{collections::HashMap, fmt};

use anyhow::Result;

#[derive(Clone, Debug)]
pub enum SyntaxError {
    ErrParentheseNotMatch,
    ErrBraceNotMatch,
    ErrRepeatFormat,
    ErrBracketNotClose,
    ErrInvalidCharClass(char, char),
    ErrEmptyEscapeChar,
}

// the type of the operation of a node in the syntax tree
#[derive(Clone, Debug, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Op {
    OpEmptyMatch = 0,
    OpAnyChar,
    Literal,
    CharClass,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CharClass {
    // (low, high)
    pub ranges: Vec<(u32, u32)>,
    pub is_negative: bool,
}

impl CharClass {
    pub fn new(mut ranges: Vec<(u32, u32)>, is_negative: bool) -> Self {
        Self::re_range(&mut ranges);
        Self {
            ranges,
            is_negative,
        }
    }

    pub fn push_range(&mut self, r: (char, char)) {
        self.ranges.push((r.0 as u32, r.1 as u32));
        // for convenience, we can get the right index and insert instead
        Self::re_range(&mut self.ranges);
    }

    pub fn neg(&mut self) -> &mut Self {
        self.is_negative = true;
        self
    }

    pub fn with_ranges(&mut self, ranges: &[(char, char)]) -> &mut Self {
        let mut new_ranges = ranges
            .into_iter()
            .map(|(l, h)| (*l as u32, *h as u32))
            .collect::<Vec<_>>();
        Self::re_range(&mut new_ranges);
        self.ranges = new_ranges;
        self
    }

    fn re_range(ranges: &mut Vec<(u32, u32)>) {
        if ranges.len() <= 1 {
            return;
        }
        ranges.sort_by(|a, b| match a.0.cmp(&b.0) {
            std::cmp::Ordering::Equal => a.1.cmp(&b.1),
            other => other,
        });

        let mut cur = ranges[0].clone();
        let mut new_ranges = vec![];
        for r in ranges.iter() {
            if r.0 > cur.1 + 1 {
                new_ranges.push(cur);
                cur = r.clone();
            } else {
                cur.1 = cur.1.max(r.1);
            }
        }
        new_ranges.push(cur);
        std::mem::swap(ranges, &mut new_ranges);
        // sort and merge the ranges
    }

    fn flip(&mut self) {
        // flip the ranges with another representation
        if self.is_negative {
            self.is_negative = false;
            let mut new_ranges = vec![];
            let mut start = 0;

            for (lo, hi) in self.ranges.iter() {
                if *lo > start {
                    new_ranges.push((start, *lo as u32 - 1));
                }
                start = *hi as u32 + 1;
            }
            if let Some(last) = self.ranges.last() {
                if last.1 < char::MAX as _ {
                    new_ranges.push((last.1 + 1, char::MAX as _));
                }
            }
            self.ranges = new_ranges;
        }
    }

    pub fn merge(mut left: Self, mut other: Self) -> Self {
        left.flip();
        other.flip();
        left.ranges.extend(other.ranges);
        Self::re_range(&mut left.ranges);
        left
    }
}

#[test]
fn test_re_range() {
    let mut r = CharClass::default();
    r.with_ranges(&[('a', 'z'), ('x', 'z'), ('e', 'f'), ('a', 'b')][..]);
    assert_eq!(r.ranges, vec![('a' as _, 'z' as _)]);

    let mut not_digit = CharClass::default();
    not_digit.with_ranges(&[('0', '9')][..]).neg();

    let merged = CharClass::merge(CharClass::default(), not_digit);
    assert_eq!(
        merged.ranges,
        vec![(0, '0' as u32 - 1), ('9' as u32 + 1, char::MAX as _)]
    );

    // the merge of empty char class is not allowed
    let empty1 = CharClass::default();
    let empty2 = CharClass::default();
    let empty3 = CharClass::merge(empty1, empty2);
    assert_eq!(empty3.ranges, vec![]);
    assert_eq!(empty3.is_negative, false);
}

impl Default for CharClass {
    fn default() -> Self {
        Self {
            ranges: vec![],
            is_negative: false,
        }
    }
}

impl CharClass {
    pub fn is_match(&self, c: char) -> bool {
        let c = c as u32;
        if self.is_negative {
            self.ranges.iter().all(|(lo, hi)| c < *lo || c > *hi)
        } else {
            self.ranges.iter().any(|(lo, hi)| c >= *lo && c <= *hi)
        }
    }
}

use bitflags::bitflags;

bitflags! {
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Flags: u32 {
        const DEFAULT = 0;
        const NON_GREEDY = 0b00000001;
    }
}

// a node in the syntax tree
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Node {
    pub op: Op,
    pub cap: usize,
    pub min: i64,
    pub max: i64,
    pub sub: Vec<Node>,
    pub char: char,
    pub class: CharClass,
    pub flag: Flags,
}

impl Node {
    pub fn new(op: Op) -> Self {
        Self {
            op,
            cap: 0,
            min: 0,
            max: 0,
            sub: Vec::new(),
            char: '\0',
            class: Default::default(),
            flag: Flags::DEFAULT,
        }
    }

    pub fn next_char(&self) -> Option<char> {
        match self.op {
            Op::Literal => Some(self.char),
            Op::Concat => {
                if self.sub.is_empty() {
                    None
                } else {
                    self.sub[0].next_char()
                }
            }
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ast {
    // the stack is useful during the parsing
    // but the num of stack elements is actually 1 when finishing the process
    pub stack: Vec<Node>,
    pub num_capture: usize,
}

// print the syntax tree with a pretty format and indentations
impl fmt::Display for Node {
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
                Op::OpAnyChar => {
                    writeln!(f, "OpAnyChar:")?;
                }
                Op::Repeat => {
                    writeln!(f, "Repeat, min:{}, max:{}", self.min, self.max)?;
                    for r in &r.sub {
                        stack.push((r, indent + 1));
                    }
                }
                Op::CharClass => {
                    writeln!(
                        f,
                        "CharClass, range:{:?}, is_neg:{}",
                        r.class.ranges, r.class.is_negative
                    )?;
                    for r in &r.sub {
                        stack.push((r, indent + 1));
                    }
                }
            }
        }
        Ok(())
    }
}

impl Ast {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            num_capture: 0,
        }
    }

    pub fn push(&mut self, r: Node) {
        self.stack.push(r);
    }

    pub fn op(&mut self, op: Op) {
        let r = Node::new(op);
        self.stack.push(r);
    }

    pub fn literal(&mut self, c: char) {
        let mut r = Node::new(Op::Literal);
        r.char = c;
        self.stack.push(r);
    }

    pub fn op_with_num_cap(&mut self, op: Op, num_cap: usize) {
        let mut r = Node::new(op);
        r.cap = num_cap;
        self.stack.push(r);
    }

    pub fn repeat(&mut self, op: Op, min: i64, max: i64, left_chars: &[char]) -> usize {
        let mut forward = 0;
        let mut r = Node::new(op);
        r.min = min;
        r.max = max;
        r.sub.push(self.stack.pop().unwrap());

        // not greedy
        if let Some(&'?') = left_chars.first() {
            r.flag |= Flags::NON_GREEDY;
            forward = 1
        }
        self.stack.push(r);
        forward
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

    // collapse the sub nodes with the same op into one node
    pub fn collapse(subs: Vec<Node>, op: Op) -> Node {
        if subs.len() == 1 {
            return subs[0].clone();
        }

        let mut r = Node::new(op);
        r.sub.push(subs[0].clone());

        for regexp in subs.into_iter().skip(1) {
            if regexp.op == op {
                r.sub.extend(regexp.sub);
            } else {
                r.sub.push(regexp);
            }
        }

        if op == Op::Alternation {
            Self::build_trie_alternation(&mut r);
        }
        r
    }

    pub fn concat(&mut self) {
        // string: abc|def|efg
        // we are at:     ^
        // and we should combine the literals of "def" together (the "abc|" is already parsed)
        let sep_index = self.get_last_sep_op_index();
        let subs = self.stack.drain(sep_index..).collect::<Vec<_>>();

        if subs.len() == 0 {
            self.op(Op::OpEmptyMatch);
            return;
        }

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
    fn build_trie_alternation(_r: &mut Node) {
        // TODO: optimize the alternation
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

    // the end of one scope, we should concat the nodes in the scope with Op::Concat
    // and then deal with the branches
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
            let mut re2: Node = self.stack.pop().unwrap();
            re2.op = Op::Capture;
            re2.sub.resize(1, re1);
            self.stack.push(re2);
        }

        Ok(())
    }

    // parses {min} (max=min) or {min,} (max=-1) or {min,max}.
    // and returns the span of the next char after the repeat
    pub fn parse_repeat(chars: &[char]) -> Result<(usize, i64, i64), SyntaxError> {
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

    /*
    parse the character class, the syntax to be supported:

    class 1. [aeiou]
    class 2. [a-z]
    class 3. [7#1-9,x] (combination of class 1-2)
    class 4. [^a-z] (negation of class 3)
    */
    pub fn parse_char_class(chars: &[char]) -> Result<(usize, CharClass), SyntaxError> {
        let origin_len = chars.len();
        let (is_negative, mut chars) = if let Some('^') = chars.first() {
            (true, &chars[1..])
        } else {
            (false, chars)
        };

        let mut class_all = CharClass::default();

        // the chars must move forward, or it will cause endless loop
        fn parse_class_char<'a>(chars: &'a [char]) -> Result<(&'a [char], char), SyntaxError> {
            return match chars.first() {
                Some(&c) => Ok((&chars[1..], c)),
                None => Err(SyntaxError::ErrBracketNotClose),
            };
        }

        while !chars.is_empty() && chars[0] != ']' {
            if chars[0] == '\\' {
                let (_, other) = Ast::parse_escape(&chars[1..])?;
                chars = &chars[1..];
                class_all = CharClass::merge(class_all, other);
                continue;
            }

            let (new_chars, lo) = parse_class_char(chars)?;
            chars = new_chars;

            let mut hi = lo;

            // patterns like [a-z] will match the condition below
            // patterns like [a-] will be parsed as a|-, and will not match the condition below
            if chars.len() >= 2 && chars[0] == '-' && chars[1] != ']' {
                let (new_chars, new_hi) = parse_class_char(&chars[1..])?;

                if new_hi < lo {
                    return Err(SyntaxError::ErrInvalidCharClass(lo, hi));
                }
                chars = new_chars;
                hi = new_hi;
            }
            class_all.push_range((lo, hi));
        }
        if chars.is_empty() {
            return Err(SyntaxError::ErrBracketNotClose);
        }
        chars = &chars[1..];
        if is_negative {
            class_all.neg();
        }

        Ok((origin_len - chars.len(), class_all))
    }

    fn parse_escape(chars: &[char]) -> Result<(usize, CharClass), SyntaxError> {
        if chars.is_empty() {
            return Err(SyntaxError::ErrEmptyEscapeChar);
        }

        const WHITE_SPACE: &[(char, char)] =
            &[(' ', ' '), ('\t', '\t'), ('\n', '\n'), ('\r', '\r')];
        const DIGIT: &[(char, char)] = &[('0', '9')];
        const WORD: &[(char, char)] = &[('a', 'z'), ('A', 'Z'), ('0', '9'), ('_', '_')];

        let mut class = CharClass::default();

        match chars[0] {
            's' => {
                class.with_ranges(WHITE_SPACE);
            }
            'S' => {
                class.with_ranges(WHITE_SPACE).neg();
            }
            'd' => {
                class.with_ranges(DIGIT);
            }
            'D' => {
                class.with_ranges(DIGIT).neg();
            }
            'w' => {
                class.with_ranges(WORD);
            }
            'W' => {
                class.with_ranges(WORD).neg();
            }
            _ => class.push_range((chars[0], chars[0])),
        }

        Ok((1, class))
    }
}

pub fn parse(pattern: &str) -> Result<Ast, SyntaxError> {
    let chars = pattern.chars().collect::<Vec<_>>();
    let mut p = Ast::new();

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
            '[' => {
                let (advance_num, class) = Ast::parse_char_class(&chars[idx + 1..])?;
                let mut node = Node::new(Op::CharClass);
                node.class = class;
                p.stack.push(node);
                idx += 1 + advance_num;
                continue;
            }
            '+' | '?' | '*' => {
                // repetition
                let op = match c {
                    '*' => Op::Star,
                    '+' => Op::Plus,
                    '?' => Op::Question,
                    _ => panic!("unreachable"),
                };
                let advance_num = p.repeat(op, 0, 0, &chars[idx + 1..]);
                idx += advance_num + 1;
                continue;
            }
            '.' => {
                // any character
                p.op(Op::OpAnyChar);
            }
            '{' => {
                // repeat starts
                let (advance_num, min, max) = Ast::parse_repeat(&chars[idx + 1..])?;
                idx += 2 + advance_num;
                let forward = p.repeat(Op::Repeat, min, max, &chars[idx..]);
                idx += forward;
                continue;
            }
            '\\' => {
                // escape
                let (advance_num, class) = Ast::parse_escape(&chars[idx + 1..])?;
                let mut node = Node::new(Op::CharClass);
                node.class = class;
                p.stack.push(node);
                idx += 1 + advance_num;
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

pub fn simplify(parser: &mut Ast) {
    for r in parser.stack.iter_mut() {
        simplify_repeat(r);
    }
    for r in parser.stack.iter_mut() {
        simplify_alternation(r);
    }
}

// simplify the repeat into other expressions
fn simplify_repeat(re: &mut Node) {
    // not a repeat node, simplify the children instead
    if re.op != Op::Repeat {
        for sub in re.sub.iter_mut() {
            simplify_repeat(sub);
        }
        return;
    }

    let sub = re.sub[0].clone();

    // half closed range

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
        let mut prefix = Node::new(Op::Concat);
        prefix.sub = vec![sub.clone(); re.min as _];
        let mut suffix = Node::new(Op::Star);
        suffix.sub.push(sub);
        prefix.sub.push(suffix);

        std::mem::swap(re, &mut prefix);
        return;
    }

    // closed range

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

    let mut prefix = Node::new(Op::Concat);
    if re.min > 0 {
        prefix.sub = vec![sub.clone(); re.min as _];
    }

    if re.max > re.min {
        let mut suffix = Node::new(Op::Question);
        suffix.sub = vec![sub.clone()];

        for _ in re.min + 1..re.max {
            let mut r = Node::new(Op::Concat);
            r.sub = vec![sub.clone(), suffix.clone()];
            let mut new_suffix = Node::new(Op::Question);
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

fn simplify_alternation(re: &mut Node) {
    if re.op != Op::Alternation {
        for sub in re.sub.iter_mut() {
            simplify_alternation(sub);
        }
        return;
    }

    let mut new_subs: Vec<Node> = vec![];
    std::mem::swap(&mut new_subs, &mut re.sub);

    let mut groups = HashMap::new();
    for sub in new_subs.into_iter() {
        let key = sub.next_char();
        groups.entry(key).or_insert(vec![]).push(sub);
    }

    let mut new_node = Node::new(Op::Alternation);

    for (next_char, group) in groups.into_iter() {
        let new_sub = if let Some(c) = next_char {
            let mut new_sub = Node::new(Op::Concat);
            let mut prefix_char = Node::new(Op::Literal);
            prefix_char.char = c;

            new_sub.sub.push(prefix_char);

            let mut nexts = Node::new(Op::Alternation);
            for mut sub in group.into_iter() {
                if sub.op == Op::Literal || sub.sub.len() == 1 {
                    nexts.sub.push(Node::new(Op::OpEmptyMatch));
                } else if sub.sub[0].op == Op::Literal {
                    sub.sub.remove(0);
                    nexts.sub.push(sub);
                }
            }

            simplify_alternation(&mut nexts);

            if nexts.sub.len() == 1 {
                new_sub.sub.push(nexts.sub.pop().unwrap());
            } else {
                new_sub.sub.push(nexts);
            }

            new_sub
        } else {
            if group.len() == 1 && group[0].op == Op::OpEmptyMatch {
                group[0].clone()
            } else {
                let mut n = Node::new(Op::Alternation);
                n.sub = group;
                n
            }
        };
        new_node.sub.push(new_sub);
    }

    new_node.sub.sort_by(|a, b| a.op.cmp(&b.op));

    std::mem::swap(
        re,
        &mut if new_node.sub.len() == 1 {
            new_node.sub.pop().unwrap()
        } else {
            new_node
        },
    );
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
    let patterns = ["123a*", "hea{4}llo", "a{4,}", "1234a{2,4}"];
    for pattern in patterns.iter() {
        let mut parser = parse(pattern).unwrap();
        simplify(&mut parser);
        for r in parser.stack.iter() {
            println!("{}", r);
        }
    }
}

#[test]
fn t3() {
    let patterns = ["[a-z]", "[^a-z]", "[-a]", "[a-]", "[aeiou]", "[a-zA-Z0-9]"];
    for pattern in patterns.iter() {
        let mut ast = parse(pattern).unwrap();
        simplify(&mut ast);
        println!("{}", ast.stack[0]);
    }

    let wrong_patterns = ["[9-0]"];
    for pattern in wrong_patterns.iter() {
        assert!(parse(pattern).is_err());
    }

    let neg_neg_patterns = ["[\\D]", "[^\\D]"];
    for pattern in neg_neg_patterns.iter() {
        let mut ast = parse(pattern).unwrap();
        simplify(&mut ast);
        println!("{}", ast.stack[0]);
    }
}

#[test]
fn t4() {
    // "a|ab|abc|abcd|b|bc|bcd|bcde",
    let patterns = ["hello(a|ab|abc)world"];
    for pattern in patterns.iter() {
        let mut parser = parse(pattern).unwrap();
        simplify(&mut parser);
        for r in parser.stack.iter() {
            println!("{}", r);
        }
    }
}
