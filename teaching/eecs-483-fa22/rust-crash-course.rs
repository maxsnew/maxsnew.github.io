#![allow(dead_code)]

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

#[cfg(test)]
mod factorial_tests {
    use super::*;
    #[test]
    fn fact_0() {
        assert_eq!(iter_factorial(0), 1);
    }

    #[test]
    fn fact_5() {
        assert_eq!(iter_factorial(5), 120);
    }
}

fn add1_vec(v: Vec<i32>) -> Vec<i32> {
    let mut out = Vec::new();
    for i in 0..v.len() {
        out.push(v[i] + 1);
    }
    out
}

fn add1_vec_ref(v: &Vec<i32>) -> Vec<i32> {
    let mut out = Vec::new();
    for i in 0..v.len() {
        out.push(v[i] + 1);
    }
    out
}

fn add1_vec_slice(v: &[i32]) -> Vec<i32> {
    let mut out = Vec::new();
    for i in 0..v.len() {
        out.push(v[i] + 1);
    }
    out
}

fn add1_in_place(v: &mut Vec<i32>) {
    for i in 0..v.len() {
        v[i] += 1;
    }
}

fn main() {

    // Vec<T>

    // let x = 1;
    // let y = factorial(x);
    // println!("fac({}) = {}", x, y);

    // let v = vec![0,1,2,3];
    // let borrowed = &v;
    // let v2 = add1_vec(v);
    // println!("borrowed: {:?}", borrowed);
    // println!("v2: {:?}", v2);

    // let v2 = add1_vec_ref(&v);

    // println!("{:?}", v);
    // add1_in_place(&mut v);
    // println!("{:?}", v);
    // let slice: &[i32] = &v[1..3];
    // println!("slice: {:?}", slice);
    // println!("add1_vec({0:?}) = {1:?}", v, v2);
    
    // println!("factorial(0) = {}", factorial(0));
    // println!("factorial(5) = {}", factorial(5));
    // println!("iter_factorial(0) = {}", iter_factorial(0));
    // println!("iter_factorial(5) = {}", iter_factorial(5));

    let t = example1();
    let t2 = example2();
    let t2_again: Arith = t2.clone();
    let t3 = example3();
    println!("interp({:?}) = {}", t, interp(&t));
    println!("interp({:?}) = {}", t2, interp(&t2));
    println!("interp({:?}) = {}", t2_again, interp(&t2_again));
    println!("interp({:?}) = {}", t3, interp(&t3));
}

#[derive(Debug, Clone)]
enum Arith {
    Leaf(i32),
    Plus(Box<Arith>, Box<Arith>),
    Times(Box<Arith>, Box<Arith>)
}

fn interp(t: &Arith) -> i32 {
    match t {
        Arith::Leaf(n) => {
            *n
        }
        Arith::Plus(l, r) => {
            interp(l) + interp(r)
        }
        Arith::Times(l, r) => {
            interp(l) * interp(r)
        }
    }
}


fn example1() -> Arith {
    Arith::Leaf(2)
}

fn example2() -> Arith {
    Arith::Plus(Box::new(Arith::Leaf(2)),
                Box::new(Arith::Leaf(6)))
}

fn example3() -> Arith {
    Arith::Plus(Box::new(Arith::Times(Box::new(Arith::Leaf(2)),
                                      Box::new(Arith::Leaf(3)))),
                Box::new(Arith::Leaf(6)))
}
