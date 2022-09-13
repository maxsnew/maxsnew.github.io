#[link(name = "compiled_code", kind = "static")]
extern "C" {
    #[link_name = "\x01start_here"]
    fn start_here() -> i64;
}

fn main() {
    let output = unsafe { start_here() };
    println!("Assembly code returned: {}", output);
}
