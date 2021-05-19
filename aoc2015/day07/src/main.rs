use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");

    day07p01(&contents);
    day07p02(&contents);
}

fn day07p01(_content: &str) {
    let e123 = Emitter { value: 123 };
    let wX = Wire { source: e123 };
    let e456 = Emitter { value: 456 };
    let wY = Wire {source: e456 };
    let gXandY = Gate { source: [wX, wY] };
}

fn day07p02(_content: &str) {}

struct Emitter {
    value: u16,
}

struct Wire {
    source: ?,
    value: u16
}

struct Gate {
    source: ?,
    operation: ?,
    value: u16
}
