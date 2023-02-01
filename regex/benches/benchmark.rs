use criterion::{criterion_group, criterion_main, Criterion};
use regex::do_matching;
use std::time::Duration;

const INPUTS: &[(&str, &str, &str)] = &[("n = 2", "a?a?aa", "aa")];

fn depth_first(c: &mut Criterion) {
    let mut g = c.benchmark_group("Depth First");
    g.measurement_time(Duration::from_secs(12));

    for &(id, expr, text) in INPUTS {
        g.bench_with_input(id, &(expr, text), |b, args| {
            b.iter(|| do_matching(args.0, args.1, true))
        });
    }
}

criterion_group!(benches, depth_first);
criterion_main!(benches);
