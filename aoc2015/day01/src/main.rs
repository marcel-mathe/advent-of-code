use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");

    day01p01(&contents);
    day01p02(&contents);
}

fn day01p01(contents: &String) {
    let up = contents.chars().filter(|&x| x == '(').count();
    let down = contents.chars().filter(|&x| x == ')').count();

    println!("Santa has to go to the {}th floor.", up - down);
}

fn day01p02(contents: &String) {
    let mut position = 0;
    let mut floor = 0;

    for c in contents.chars() {
        position = position + 1;
        if c == '(' { floor = floor + 1; }
        if c == ')' { floor = floor - 1; }

        if floor == -1 { break; }
    }

    println!("Santa steps into the basement at position {}.", position);
}
