#[derive(Debug)]
enum Expr {
    Number(i64),
    Add1(Box<Expr>),
    Sub1(Box<Expr>),
}

fn main() {
}
enum Reg {
    Rax
}

enum Instr {
    Mov(Reg, i64),
    Add(Reg, i32),
}

// if code_gen(t) ~> is
// then running the instruction is, should put the result of evaluating t in rax
fn code_gen(t: &Expr) -> Vec<Instr> {
    match t {
        Expr::Number(n) => {
            vec![Instr::Mov(Reg::Rax, *n)]
        }
        Expr::Add1(e) => {
            let mut is = code_gen(e);
            is.push(Instr::Add(Reg::Rax, 1));
            is
        }
        Expr::Sub1(e) => {
            let mut is = code_gen(e);
            is.push(Instr::Add(Reg::Rax, -1));
            is
        }
    }
}

fn asm_to_string(v: &[Instr]) -> String {
//     format!("\
//         section .text
//         global _start_here
// _start_here:
//         mov rax, {}
//         ret
// ", t)
    panic!("nyi")
}
