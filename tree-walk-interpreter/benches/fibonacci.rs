use tree_walk_interpreter::interpreter::Interpreter;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use parser::lexer::Lexer;
use parser::parser::Parser;
use parser::resolver::Resolver;
use parser::types::Pass;

const PROGRAM: &str = "
fun fibonacci(a) {
    if (a < 2) return a;
    return fibonacci(a-1) + fibonacci(a-2);
}
fibonacci";

fn fibonacci(extra: usize) {
    let program = format!("{}({});", PROGRAM, extra);
    let mut lexer = Lexer::new(program.as_str(), "stdin");
    let ss = lexer
        .parse()
        .and_then(|ts| {
            let parser = Parser::new(ts.into_iter().peekable());
            parser.parse()
        }).unwrap();
    let mut resolver = Resolver::new();
    let locals = resolver.run(&ss).unwrap();
    let paths = vec![];
    let mut interpreter = Interpreter::new(&paths, "");
    interpreter.locals = locals;
    interpreter.run(&ss).unwrap();
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fibonacci 5", |b| {
        b.iter(|| fibonacci(black_box(5)))
    });
    c.bench_function("fibonacci 10", |b| {
        b.iter(|| fibonacci(black_box(10)))
    });
    c.bench_function("fibonacci 20", |b| {
        b.iter(|| fibonacci(black_box(20)))
    });
    c.bench_function("fibonacci 40", |b| {
        b.iter(|| fibonacci(black_box(20)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
