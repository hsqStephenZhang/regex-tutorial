#[cfg(test)]
mod tests {
    use nom::{IResult, Parser, branch::alt, bytes::complete::tag, multi::many0};

    fn regex_parser(input: &str) -> IResult<&str, (&str, Vec<&str>, &str)> {
        (tag("a"), many0(alt((tag("b"), tag("c")))), tag("d")).parse(input)
    }

    #[test]
    fn test_regex_parser() {
        let input = "abcbcd";
        let expected = ("d", vec!["b", "c", "b", "c"], "a");
        let result = regex_parser(input);
        assert_eq!(result, Ok(("", expected)));
    }

    #[derive(Debug, Clone)]
    enum RegexAST {
        Char(char),
        Seq(Vec<RegexAST>),
        Alt(Box<RegexAST>, Box<RegexAST>),
        Star(Box<RegexAST>),
    }

    fn generate_nom_code(ast: &RegexAST) -> String {
        match ast {
            RegexAST::Char(c) => format!("tag(\"{}\")", c),
            RegexAST::Star(inner) => format!("many0({})", generate_nom_code(inner)),
            RegexAST::Alt(left, right) => {
                format!(
                    "alt(({}, {}))",
                    generate_nom_code(left),
                    generate_nom_code(right)
                )
            }
            RegexAST::Seq(list) => {
                let parts: Vec<String> = list.iter().map(generate_nom_code).collect();
                format!("tuple(({}))", parts.join(", "))
            }
        }
    }

    #[test]
    fn t1() {
        let ast = RegexAST::Seq(vec![
            RegexAST::Char('a'),
            RegexAST::Star(Box::new(RegexAST::Alt(
                Box::new(RegexAST::Char('b')),
                Box::new(RegexAST::Char('c')),
            ))),
            RegexAST::Char('d'),
        ]);

        let nom_code = generate_nom_code(&ast);
        assert_eq!(
            nom_code,
            r#"tuple((tag("a"), many0(alt((tag("b"), tag("c")))), tag("d")))"#
        );
    }
}
