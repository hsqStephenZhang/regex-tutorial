include!(concat!(env!("OUT_DIR"), "/generated.rs"));

fn main () {
    let res = regex_parser("abcbcd");
    println!("{:?}", res);
}