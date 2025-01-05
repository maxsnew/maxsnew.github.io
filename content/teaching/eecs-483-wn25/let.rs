/*

------------------------------------------------------
Simple interpreter to accompany Lecture 3 of EECS 483.
Eric Giovannini
Sept 14, 2022
-----------------------------------------------------

Concrete syntax of our language...

<expr> ::= NUMBER
    | add1 ( <expr> )
    | sub1 ( <expr> )
    | let IDENTIFIER = <expr> in <expr>
    | IDENTIFIER

---------------------------------------------------------------

Important conventions:

let x = 3 in let x = 4 in x
    => 4

The inner let "takes precedence" and overwrites the outer
binding for x.

---------------------------------------------------------------

let x = add1(x) in 5
             ^ x is not in scope

    => compile-time error: x is not in scope

Compare this to:

let x = 3 in let x = x in x
                     ^ in scope!
    => 3

Here the x *is* in scope in the inner let, unlike
in the first example.

---------------------------------------------------------------


*/

enum Exp {
    Num(i64),
    Add1(Box<Exp>),
    Sub1(Box<Exp>),
    Let(String, Box<Exp>, Box<Exp>), // let x = e1 in e2
    Id(String),
}

fn lookup(s: &String, v: &[(String, i64)]) -> Result<i64, String> {
    for (id, val) in v.iter().rev() {
        if id == s {
            return Ok(*val);
        }
    }
    Err("Not found".to_string())
}

// Notice that interpret takes a Vec, not a &Vec
fn interpret(e: &Exp, mut v: Vec<(String, i64)>) -> Result<i64, String> {
    match e {
        // "Base case" -- a number
        Exp::Num(n) => Ok(*n),

        // Arithmetic expressions
        Exp::Add1(e) => {
            match interpret(e,v) {
                Ok(n) =>
                    Ok(n + 1),
                Err(message) => {
                    Err(format!("Error in evaluating expression argument to Add1: {}", message))
                }
            }
        }

        // Error handling is done differently here for the sake of variety
        Exp::Sub1(e) => {
            interpret(e, v)
              .and_then(|n| Ok(n - 1))
              .or_else(|message|
                Err(format!("Error in evaluating expression argument to Sub1: {}", message)))
        }

        // Identifiers
        Exp::Id(s) => lookup(s, &v)
        //.unwrap_or()
          .or( Err(format!("Unknown variable {}", s))),

        // Let statements
        Exp::Let(id, e, e_bod) => {
            let n = interpret(e, v.clone())?; // note the call to clone!
            v.push((id.clone(), n)); // note the call to clone!
            interpret(e_bod, v)
        }
    }
}

fn main() {
    println!("Hello, world!");

    let p1 = Exp::Add1(Box::new(Exp::Num(3)));

    // let x = add1(3) in x
    let p_let_1 = Exp::Let(
        String::from("x"),
        Box::new(Exp::Add1(Box::new(Exp::Num(7)))),
        Box::new(Exp::Id(String::from("x"))),
    );


    // let x = add1(3) in add1(x)
    let p_let_2 = Exp::Let(
        String::from("x"),
        Box::new(Exp::Add1(Box::new(Exp::Num(3)))),
        Box::new(Exp::Sub1(Box::new(Exp::Id(String::from("x"))))),
    );

    // let x = add1(3) in add1(y)
    let p_error = Exp::Let(
        String::from("x"),
        Box::new(Exp::Add1(Box::new(Exp::Num(3)))),
        Box::new(Exp::Sub1(Box::new(Exp::Id(String::from("y"))))),
    );

    // Prints 4
    println!(
        "Result of p1: {}",
        interpret(&p1, vec![]).expect("Evaluation failed")
    );

    // Prints 8
    println!(
        "Result of p_let_1: {}",
        interpret(&p_let_1, vec![]).expect("Evaluation failed")
    );

    // Prints 3
    println!(
        "Result of p_let_2: {}",
        interpret(&p_let_2, vec![]).expect("Evaluation failed")
    );

    // Errors due to unknown variable "y"
    println!(
        "Result of p_err: {}",
        interpret(&p_error, vec![]).expect("Evaluation failed")
    );

}
