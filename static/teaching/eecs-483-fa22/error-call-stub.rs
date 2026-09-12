#[repr(C)]
#[derive(PartialEq, Eq, Copy, Clone)]
struct SnakeVal(u64);

static BOOL_TAG: u64 = 0x00_00_00_00_00_00_00_01;
static SNAKE_TRU: SnakeVal = SnakeVal(0xFF_FF_FF_FF_FF_FF_FF_FF);
static SNAKE_FLS: SnakeVal = SnakeVal(0x7F_FF_FF_FF_FF_FF_FF_FF);


#[link(name = "compiled_code", kind = "static")]
extern "C" {

    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    #[link_name = "\x01start_here"]
    fn start_here() -> SnakeVal;
}

// reinterprets the bytes of an unsigned number to a signed number
fn unsigned_to_signed(x: u64) -> i64 {
    i64::from_le_bytes(x.to_le_bytes())
}

fn sprint_snake_val(x: SnakeVal) -> String {
    if x.0 & BOOL_TAG == 0 { // it's a number
        format!("{}", unsigned_to_signed(x.0) >> 1)
    } else if x == SNAKE_TRU {
        String::from("true")
    } else if x == SNAKE_FLS {
        String::from("false")
    } else {
        format!("Invalid snake value: 0x{:016x}", x.0)
    }
}

fn main() {
    let output = unsafe { start_here() };
    // println!("As unsigned number: {}", output.0);
    // println!("As signed   number: {}", i64::from_le_bytes(output.0.to_le_bytes()));
    // println!("As binary:           0b{:064b}", output.0);
    // println!("As hex:              0x{:016x}", output.0);
    println!("{}", sprint_snake_val(output));
}

type ErrorCode = u64;
static ADD1_ERROR: ErrorCode = 0;
static ADD_ERROR: ErrorCode  = 1;

#[export_name = "\x01snake_error"]
extern "C" fn snake_error(err_code: u64, v: SnakeVal) {
    if err_code == ADD1_ERROR {
        eprintln!("add1 expected a number but got a boolean {}", sprint_snake_val(v));
    } else if err_code == ADD_ERROR {
        eprintln!("+ expected a number but got a boolean {}", sprint_snake_val(v));
    } else {
        eprintln!("I apologize to you, dear user. I made a bug. Here's a snake value: {}", sprint_snake_val(v));
    }
    std::process::exit(1);
}
