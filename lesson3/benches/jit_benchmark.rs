use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use lesson3::compile::compile;
use lesson3::exec::dfs;
use lesson3::jit::{execute_jit, JitCompiler};
use lesson3::syntax::parse;
use std::collections::HashSet;

fn benchmark_jit(c: &mut Criterion) {
    let mut group = c.benchmark_group("JIT Execution");

    let test_cases = vec![
        ("abc", "abc", "Simple literal match"),
        ("a+", "aaaaaaaaaa", "Greedy quantifier"),
        ("[0-9]+", "1234567890", "Character class with +"),
        ("a*b+c?", "aaaabbbbc", "Multiple quantifiers"),
        ("[a-z]+@[a-z]+.[a-z]+", "user@example.com", "Email-like pattern"),
        ("a+b+c+d+", "aaaabbbbccccdddd", "Multiple consecutive +"),
        (".+", "any string here!", "Wildcard with +"),
    ];

    for (pattern, text, description) in test_cases {
        let ast = parse(pattern).unwrap();
        let insts = compile(&ast).unwrap();
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();

        group.bench_with_input(
            BenchmarkId::new("JIT", description),
            &(jit_fn, text),
            |b, (jit_fn, text)| {
                b.iter(|| execute_jit(black_box(*jit_fn), black_box(text)));
            },
        );
    }

    group.finish();
}

fn benchmark_interpreter(c: &mut Criterion) {
    let mut group = c.benchmark_group("Interpreter Execution");

    let test_cases = vec![
        ("abc", "abc", "Simple literal match"),
        ("a+", "aaaaaaaaaa", "Greedy quantifier"),
        ("[0-9]+", "1234567890", "Character class with +"),
        ("a*b+c?", "aaaabbbbc", "Multiple quantifiers"),
        ("[a-z]+@[a-z]+.[a-z]+", "user@example.com", "Email-like pattern"),
        ("a+b+c+d+", "aaaabbbbccccdddd", "Multiple consecutive +"),
        (".+", "any string here!", "Wildcard with +"),
    ];

    for (pattern, text, description) in test_cases {
        let ast = parse(pattern).unwrap();
        let insts = compile(&ast).unwrap();
        let chars: Vec<char> = text.chars().collect();

        group.bench_with_input(
            BenchmarkId::new("Interpreter", description),
            &(insts.clone(), chars.clone()),
            |b, (insts, chars)| {
                b.iter(|| {
                    let mut round = 0;
                    let mut thread_id = 0;
                    let mut saved = Vec::new();
                    dfs(
                        black_box(insts),
                        black_box(chars),
                        HashSet::new(),
                        0,
                        0,
                        &mut round,
                        &mut thread_id,
                        &mut saved,
                    )
                });
            },
        );
    }

    group.finish();
}

fn benchmark_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("JIT vs Interpreter");

    let test_cases = vec![
        ("abc", "abc", "Simple literal"),
        ("a+", "aaaaaaaaaa", "Greedy quantifier"),
        ("[0-9]+", "1234567890", "Character class"),
        ("a*b+c?", "aaaabbbbc", "Multiple quantifiers"),
    ];

    for (pattern, text, description) in test_cases {
        let ast = parse(pattern).unwrap();
        let insts = compile(&ast).unwrap();
        
        // JIT version
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();
        
        group.bench_with_input(
            BenchmarkId::new("JIT", description),
            &(jit_fn, text),
            |b, (jit_fn, text)| {
                b.iter(|| execute_jit(black_box(*jit_fn), black_box(text)));
            },
        );

        // Interpreter version
        let chars: Vec<char> = text.chars().collect();
        group.bench_with_input(
            BenchmarkId::new("Interpreter", description),
            &(insts.clone(), chars.clone()),
            |b, (insts, chars)| {
                b.iter(|| {
                    let mut round = 0;
                    let mut thread_id = 0;
                    let mut saved = Vec::new();
                    dfs(
                        black_box(insts),
                        black_box(chars),
                        HashSet::new(),
                        0,
                        0,
                        &mut round,
                        &mut thread_id,
                        &mut saved,
                    )
                });
            },
        );
    }

    group.finish();
}

fn benchmark_non_matching(c: &mut Criterion) {
    let mut group = c.benchmark_group("Non-matching patterns");

    let test_cases = vec![
        ("abc", "xyz", "Simple literal"),
        ("a+", "bbbbbbbbbb", "Greedy quantifier"),
        ("[0-9]+", "abcdefghij", "Character class"),
        ("a*b+c?", "xyzzzzzzzz", "Multiple quantifiers"),
    ];

    for (pattern, text, description) in test_cases {
        let ast = parse(pattern).unwrap();
        let insts = compile(&ast).unwrap();
        
        // JIT version
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();
        
        group.bench_with_input(
            BenchmarkId::new("JIT", description),
            &(jit_fn, text),
            |b, (jit_fn, text)| {
                b.iter(|| execute_jit(black_box(*jit_fn), black_box(text)));
            },
        );

        // Interpreter version
        let chars: Vec<char> = text.chars().collect();
        group.bench_with_input(
            BenchmarkId::new("Interpreter", description),
            &(insts.clone(), chars.clone()),
            |b, (insts, chars)| {
                b.iter(|| {
                    let mut round = 0;
                    let mut thread_id = 0;
                    let mut saved = Vec::new();
                    dfs(
                        black_box(insts),
                        black_box(chars),
                        HashSet::new(),
                        0,
                        0,
                        &mut round,
                        &mut thread_id,
                        &mut saved,
                    )
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    benchmark_jit,
    benchmark_interpreter,
    benchmark_comparison,
    benchmark_non_matching
);
criterion_main!(benches);
