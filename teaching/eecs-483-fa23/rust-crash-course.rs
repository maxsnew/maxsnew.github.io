#![allow(dead_code)]

fn main() {
    println!("Hello, {}!", 483);
    let x = 5;
    let y = imp_fact(x);
    println!("factorial({}) = {}", x, y);

    let mut v : Vec<i64> = vec![2, 7, 4];
    println!("the first element: {}", first_element2(&mut v));
    println!("a totally new vec: {:?}", own_it(&v));
    println!("a totally old vec: {:?}", v);

    let a = example();
    println!("here's an arith: {:?}", a);
    println!("it evaluates to {}", interp(&a))
}

#[derive(Debug, Clone)]
enum Arith {
    Leaf(i32),
    Plus(Box<Arith>, Box<Arith>),
    Times(Box<Arith>, Box<Arith>),
}

fn is_plus(a: Arith) -> i32 {
    match a {
        Arith::Leaf(n) => n,
        _ => 7
    }
}

fn foo(a: &Arith) -> &i32 {
    match a {
        Arith::Leaf(n) => n,
        _ => panic!("wth")
    }
}

fn interp(a: &Arith) -> i32 {
    match a {
        Arith::Leaf(n) => *n,
        Arith::Plus(l, r) => interp(l) + interp(r),
        Arith::Times(l, r) => interp(l) * interp(r),
    }
}









fn example() -> Arith {
    Arith::Plus(Box::new(Arith::Leaf(5)),
                Box::new(Arith::Times(Box::new(Arith::Leaf(3)),
                                      Box::new(Arith::Leaf(4))))
    )
}













// fn interp(t: &Arith) -> i32 {
//     match t {
//         Arith::Leaf(n) => *n,
//         Arith::Plus(l, r) => interp(l) + interp(r),
//         Arith::Times(l, r) => interp(l) * interp(r),
//     }
// }

// fn example1() -> Arith {
//     Arith::Leaf(2)
// }

// fn example2() -> Arith {
//     Arith::Plus(Box::new(Arith::Leaf(2)), Box::new(Arith::Leaf(6)))
// }

// fn example3() -> Arith {
//     Arith::Plus(
//         Box::new(Arith::Times(
//             Box::new(Arith::Leaf(2)),
//             Box::new(Arith::Leaf(3)),
//         )),
//         Box::new(Arith::Leaf(6)),
//     )
// }







// Returns the first element of a vector
fn first_element(v: Vec<i64>) -> i64 {
    v[0]
}

fn first_element2(v: &mut[i64]) -> i64 {
    v[0] = 1;
    v[0]
}

fn own_it(v: &Vec<i64>) -> Vec<i64> {
    v.clone()
}

fn fun_fact(n : i64) -> i64 {
    if n <= 1 {
        1
    } else {
        n * fun_fact(n - 1)
    }
}

fn imp_fact(n : i64) -> i64 {
    let mut res = 1;
    for i in 1..n+1 {
        res *= i
    }
    return res;
}

fn iter_fact(n : i64) -> i64 {
    (1..=n).fold(1, |x, res| x * res)
}

#[cfg(test)]
mod factorial_tests {
    use super::*;
    #[test]
    fn fact_0() {
        assert_eq!(iter_fact(0), 1);
    }

    #[test]
    fn fact_5() {
        assert_eq!(iter_fact(5), 120);
    }
}
