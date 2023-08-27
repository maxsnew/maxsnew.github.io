#![allow(unused)]
/* Part I: basic syntax */

// fn main() {
//     println!("factorial(5) = {}", factorial(5));
//     println!("factorial(0) = {}", factorial(0))
// }

fn nerd_factorial(n: u64) -> u64 {
    if n == 0 {
	1
    } else 
    if n == 1 {
	1
    } else {
	n * factorial(n - 1)
    }
}

fn professional_factorial(n: u64) -> u64 {
    let mut acc = 1;
    for i in 1..n+1 {
	acc *= i
    }
    acc
}

fn factorial(n: u64) -> u64 {
    (1..n+1).fold(1, |x, acc| x * acc)
}

#[cfg(test)]
mod factorial_tests {
    use super::*;
    #[test]
    fn fact_0() {
        assert_eq!(factorial(0), 1);
    }

    #[test]
    fn fact_5() {
        assert_eq!(factorial(5), 120);
    }
}

































































/* for reference
fn factorial(x: u32) -> u32 {
    if x == 0 {
        1
    } else {
        x * factorial(x - 1)
    }
}

fn iter_factorial(mut x: u32) -> u32 {
    let mut tmp = 1;
    // while x > 0 {
    //     tmp *= x;
    //     x -= 1;
    // }

    for i in x..1 {
        println!("{}", i);
        tmp *= i;
    }
    
    return tmp;
}

*/































































/* Part II: Ownership, Borrowing, Vecs and Slices */

// fn main() {
//     let mut v: Vec<i64> = vec![0,1,2,3];
//     v.push(0);
//     v.push(17);
//     v.push(-178);
//     println!("here's a vec: {:?}", add1_borrowed_vec(&v));

//     println!("here's a vec: {:?}", v);
//     add1_in_place(&mut v);
//     println!("here's a vec: {:?}", v);
//     v.push(18);
//     println!("here's a vec: {:?}", v);
// }

fn add1_vec(v : Vec<i64>) -> Vec<i64> {
    let mut new_vec = Vec::new();
    for x in v {
	new_vec.push(x + 1);
    }
    new_vec
}

fn add1_borrowed_vec(v_ref : &[i64]) -> Vec<i64> {
    let mut new_vec = Vec::new();
    for x in v_ref {
	new_vec.push(x + 1);
    }
    new_vec
}

fn add1_in_place(v_r: &mut Vec<i64>) {
    for i in 0..v_r.len() {
	v_r[i] += 1;
    }
}































































/* Part III: Abstract Syntax Trees */

#[derive(Debug)]
enum AST {
    Num(i64),
    Plus(Box<AST>, Box<AST>),
    Times(Box<AST>, Box<AST>)
}

fn main() {
    let t0 = AST::Num(64);
    let t1 = AST::Plus(Box::new(AST::Num(5)), Box::new(AST::Num(6)));
    println!("Here's a tree: {:?}", t0);
    println!("Here's another tree: {:?}", t1);
    println!("Here's its evaluation: {:?}", interp(&t1));
}

fn interp(t: &AST) -> i64 {
    match t {
	AST::Num(n) => *n,
	AST::Plus(tl, tr) => interp(tl) + interp(tr),
	AST::Times(tl, tr) => interp(tl) * interp(tr),
    }
}





























































































// Part IV: Why Mutable borrows are not aliasable

// fn main() {
//     let mut v: Vec<i32> = vec![1,2,3];
//     let i: &i32 = &v[1];
//     v.push(4);
//     v.push(5);
//     println!("{}", i);
// }
