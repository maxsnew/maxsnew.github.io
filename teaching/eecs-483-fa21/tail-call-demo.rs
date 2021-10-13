fn count_up(i: u64) -> u64 {
    if i == 0 {
        0
    } else {
        count_up(i - 1) + i
    }
}

fn count_loop(mut i: u64) -> u64 {
    let mut sum = 0;
    while i > 0 {
        sum += i;
        i -= 1;
    }
    sum
}

fn count_tail(i: u64, sum: u64) -> u64 {
    if i > 0 {
        let i2 = i - 1;
        let sum2 = i + sum;
        count_tail(i2, sum2)
    } else {
        sum
    }
}

/* Stack overflow if you do `rustc tail-call-demo.rs`, but doesn't if you do
 * `rustc -O tail-call-demo.rs`
 *
 *
 * */
fn main() {
    println!("{}", count_tail(10, 0));
    println!("{}", count_tail(100000000, 0));
}
