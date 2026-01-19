use lesson3::compile::compile;
use lesson3::jit::{execute_jit, JitCompiler};
use lesson3::syntax::parse;
use std::time::Instant;

fn main() {
    env_logger::init();

    println!("=== Regex JIT Compilation Demo ===\n");

    // Example 1: Simple character matching
    demo_pattern("abc", &["abc", "ab", "abcd", "xyz"]);

    // Example 2: Any character
    demo_pattern("a.c", &["abc", "axc", "a1c", "ac", "abcd"]);

    // Example 3: Character class
    demo_pattern("[a-z]", &["a", "m", "z", "A", "1", "ab"]);

    // Example 4: Multiple character classes
    demo_pattern("[0-9][a-z]", &["1a", "9z", "a1", "11", "aa"]);

    // Example 5: Plus quantifier
    demo_pattern("a+", &["a", "aa", "aaa", "", "b"]);

    // Example 6: Star quantifier
    demo_pattern("a*", &["", "a", "aa", "aaa", "b"]);

    // Example 7: Question mark quantifier
    demo_pattern("a?b", &["b", "ab", "aab", "bb"]);

    // Example 8: Alternation
    demo_pattern("hello|world", &["hello", "world", "hell", "helloworld"]);

    // Example 9: Complex pattern with multiple quantifiers
    demo_pattern("a+b+", &["ab", "aab", "abb", "aaabbb", "a", "b"]);

    // Example 10: Digit matching
    demo_pattern("[0-9]+", &["0", "123", "999", "", "abc"]);

    // Example 11: Email-like pattern (simplified)
    demo_pattern("[a-z]+@[a-z]+", &["user@domain", "a@b", "test@example", "nodomain", "@missing"]);

    // Example 12: Mixed quantifiers
    demo_pattern("a*b+c?", &["abc", "bc", "aabc", "aaabbb", "aabbbc", "b", "bbc"]);

    // Performance comparison
    println!("\n=== Performance Comparison ===");
    benchmark_jit_vs_interpreter();
}

fn demo_pattern(pattern: &str, test_cases: &[&str]) {
    println!("Pattern: '{}'", pattern);
    
    let ast = parse(pattern).unwrap();
    let insts = compile(&ast).unwrap();
    
    let mut compiler = JitCompiler::new().unwrap();
    let jit_fn = compiler.compile_bytecode(&insts).unwrap();

    for text in test_cases {
        let result = execute_jit(jit_fn, text);
        println!("  '{}' => {}", text, if result { "✓ MATCH" } else { "✗ NO MATCH" });
    }
    println!();
}

fn benchmark_jit_vs_interpreter() {
    let test_cases = vec![
        ("abc", "abc", "xyz"),
        ("a+", "aaaa", "bbbb"),
        ("[0-9]+", "12345", "abcde"),
        ("a*b+c?", "aaabbc", "xyzzz"),
    ];

    for (pattern, text_match, text_no_match) in test_cases {
        let iterations = 1_000_000;

        // Compile pattern
        let ast = parse(pattern).unwrap();
        let insts = compile(&ast).unwrap();
        
        // JIT compilation
        let mut compiler = JitCompiler::new().unwrap();
        let jit_fn = compiler.compile_bytecode(&insts).unwrap();

        // Benchmark JIT (matching)
        let start = Instant::now();
        for _ in 0..iterations {
            execute_jit(jit_fn, text_match);
        }
        let jit_match_duration = start.elapsed();

        // Benchmark JIT (non-matching)
        let start = Instant::now();
        for _ in 0..iterations {
            execute_jit(jit_fn, text_no_match);
        }
        let jit_no_match_duration = start.elapsed();

        println!("\nPattern: '{}', Iterations: {}", pattern, iterations);
        println!("  JIT (match):     {:?}", jit_match_duration);
        println!("  JIT (no match):  {:?}", jit_no_match_duration);
        println!(
            "  Throughput:      {:.2} million ops/sec",
            iterations as f64 / jit_match_duration.as_secs_f64() / 1_000_000.0
        );
    }
}
