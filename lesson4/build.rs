use std::io::Write;
use std::{env, path::Path};

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

fn main() -> std::io::Result<()> {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("generated.rs");
    let mut file = std::fs::File::create(&dest_path).expect("Unable to create file");

    let generated_code = r#"
#[allow(deprecated)]
use nom::{
    branch::alt,
    bytes::complete::tag,
    multi::many0,
    sequence::tuple,
    IResult,
};
"#;

    write!(&mut file, "{}\n", generated_code)?;

    let ast = RegexAST::Seq(vec![
        RegexAST::Char('a'),
        RegexAST::Star(Box::new(RegexAST::Alt(
            Box::new(RegexAST::Char('b')),
            Box::new(RegexAST::Char('c')),
        ))),
        RegexAST::Char('d'),
    ]);
    let nom_code = generate_nom_code(&ast);

    writeln!(
        &mut file,
        "{}",
        r#"#[allow(deprecated)]
fn regex_parser(input: &str) -> IResult<&str, (&str, Vec<&str>, &str)> {"#
    )?;
    writeln!(&mut file, "    {}(input)", nom_code)?;
    writeln!(&mut file, "{}", "}")?;
    Ok(())
}
