use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
	let num = line.unwrap().parse::<i64>().unwrap();
        println!("{}", compile(&Ast::Num(num)));
    }
}

// Abstract Syntax Trees
enum Ast {
    Num(i64)
}

// X64 assembly instructions
enum X64 {
    Mov(Reg, i64),
    Ret()
}

enum Reg {
    Rax,
    // Rbx,
    // Rcx,
    // Rdx,
    // Rdi,
    // Rsi,
    // Rdi,
    // Rbp,
    // Rsp,
    // R08,
    // R09,
    // R10,
    // R11,
    // R12,
    // R13,
    // R14,
    // R15,
}

// 
fn code_gen(t: &Ast) -> Vec<X64> {
    match t {
	Ast::Num(n) =>
	    vec![X64::Mov(Reg::Rax, *n),
		 X64::Ret()
	    ]
    }
}

fn display_x64(is : &[X64]) -> String {
    let mut buf = String::new();
    for i in is {
	buf.push_str(&match i {
	    X64::Mov(Reg::Rax, n) => format!("mov rax, {}\n", n),
	    X64::Ret()            => String::from("ret\n")
	}) 
    }
    buf
}

fn compile(t: &Ast) -> String {
    format!("\
        section .text
        global start_here
start_here:
{}",
	    display_x64(&code_gen(t)))
}
