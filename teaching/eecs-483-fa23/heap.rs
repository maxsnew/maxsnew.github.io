#[repr(C)]
#[derive(PartialEq, Eq, Copy, Clone)]
struct SnakeVal(u64);

#[repr(u64)]
pub enum SnakeErr {
    Overflow = 0,
    ArithExpectedNum = 1,
    CmpExpectedNum = 2,
    LogExpectedBool = 3,
    IfExpectedBool = 4,
}

static BOOL_TAG: u64 = 0x00_00_00_00_00_00_00_01;
static SNAKE_TRU: SnakeVal = SnakeVal(0xFF_FF_FF_FF_FF_FF_FF_FF);
static SNAKE_FLS: SnakeVal = SnakeVal(0x7F_FF_FF_FF_FF_FF_FF_FF);

static mut HEAP: [u64; 50] = [0; 50];

#[link(name = "compiled_code", kind = "static")]
extern "C" {

    #[link_name = "\x01start_here"]
    fn start_here(heap: *mut u64) -> SnakeVal;
}

fn main() {
    let output = unsafe { start_here(HEAP.as_mut_ptr()) };
    println!("Val: {}", sprint_snake_val(output));
    println!("Heap: {:?}", unsafe { HEAP });
}

// reinterprets the bytes of an unsigned number to a signed number
fn unsigned_to_signed(x: u64) -> i64 {
    i64::from_le_bytes(x.to_le_bytes())
}

fn sprint_snake_val(x: SnakeVal) -> String {
    if x.0 & BOOL_TAG == 0 {
        // it's a number
        format!("{}", unsigned_to_signed(x.0) >> 1)
    } else if x == SNAKE_TRU {
        String::from("true")
    } else if x == SNAKE_FLS {
        String::from("false")
    } else {
        format!("Invalid snake value 0x{:x}", x.0)
    }
}

#[export_name = "\x01print_snake_val"]
extern "C" fn print_snake_val(v: SnakeVal) -> SnakeVal {
    println!("{}", sprint_snake_val(v));
    v
}

#[export_name = "\x01snake_error"]
extern "C" fn snake_error(ecode: SnakeErr, v1: SnakeVal) -> SnakeVal {
    match ecode {
        SnakeErr::Overflow => eprintln!("Operation overflowed"),
        SnakeErr::ArithExpectedNum => {
            eprintln!("arithmetic expected a number, got {}", sprint_snake_val(v1))
        }
        SnakeErr::CmpExpectedNum => {
            eprintln!("comparison expected a number, got {}", sprint_snake_val(v1))
        }
        SnakeErr::LogExpectedBool => {
            eprintln!("logic expected a boolean, got {}", sprint_snake_val(v1))
        }
        SnakeErr::IfExpectedBool => {
            eprintln!("if expected a boolean, got {}", sprint_snake_val(v1))
        }
    }
    std::process::exit(1)
}
