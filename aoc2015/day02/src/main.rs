use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");

    day02p01(&contents);
    day02p02(&contents);
}

fn min2(x: i32, y: i32) -> i32 {
    if x < y { x } else {y }
}

fn day02p01(contents: &String) {
    let mut total = 0;

    for s in contents.lines() {
        let dimensions = s.split("x").collect::<Vec<&str>>();
        let l = dimensions[0].parse::<i32>().unwrap();
        let w = dimensions[1].parse::<i32>().unwrap();
        let h = dimensions[2].parse::<i32>().unwrap();

        let lw = l * w;
        let wh = w * h;
        let hl = h * l;

        let extra = min2(min2(lw, wh), hl);

        total = total + extra + 2 * (lw + wh + hl);
    }

    println!("Total wrapping paper: {}", total);
}

fn day02p02(contents: &String) {
    let mut total = 0;

    for s in contents.lines() {
        let dimensions = s.split("x").collect::<Vec<&str>>();
        let l = dimensions[0].parse::<i32>().unwrap();
        let w = dimensions[1].parse::<i32>().unwrap();
        let h = dimensions[2].parse::<i32>().unwrap();

        let lw = l + w;
        let wh = w + h;
        let hl = h + l;

        let wrap = 2 * min2(min2(lw, wh), hl);
        let bow = l * w * h;

        total = total + bow + wrap;
    }

    println!("Total ribbon: {}", total);
}

