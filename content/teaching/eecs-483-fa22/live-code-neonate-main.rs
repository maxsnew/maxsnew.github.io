use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
	let ast = line.unwrap().parse::<i64>().unwrap();
        println!("{}", compile(&AST::Num(ast)));
    }
}

enum AST {
    Num(i64)
}

// X64 assembly instructions
enum X64 {
    Mov(Reg, i64),
    Ret()
}

enum Reg {
    RAX,
    // RBX,
    // RDX,
    // RCX,
}

fn code_gen(t: &AST) -> Vec<X64> {
    match t {
	AST::Num(n) =>
	    vec![X64::Mov(Reg::RAX, *n),
		 X64::Ret()
	    ]
    }
}

// Vec<i64> vs &[i64]
// String   vs &str
fn display_X64(is : &[X64]) -> String {
    let mut buf = String::new();
    for i in is {
	buf.push_str(&match i {
	    X64::Mov(Reg::RAX, n) => format!("mov rax, {}\n", n),
	    X64::Ret()            => format!("ret\n")
	}) 
    }
    buf
}

fn compile(t: &AST) -> String {
    format!("\
        section .text
        global start_here
start_here:
{}",
	    display_X64(&code_gen(t)))
}
