use bit_vec::BitVec;
use regex::Regex;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");

    day06p01(contents.to_string(), 1000);
    day06p02(contents.to_string());
}

fn day06p01(contents: String, n: i32) {
    let re = Regex::new(r"(\w+) (\d+),(\d+) through (\d+),(\d+)").unwrap();
    let size = (n * n) as usize;
    let mut bv = BitVec::from_elem(size, false);

    for line in contents.lines() {
        let captures = re.captures(line).unwrap();

        let command: String = captures[1].to_string();
        let start_x: i32 = captures[2].parse().unwrap();
        let start_y: i32 = captures[3].parse().unwrap();
        let end_x: i32 = captures[4].parse().unwrap();
        let end_y: i32 = captures[5].parse().unwrap();

        let lamps = xy2lamp(n, start_x, start_y, end_x, end_y);

        for lamp in lamps.iter() {
            match command.as_ref() {
                "toggle" => {
                    if bv[*lamp] {
                        bv.set(*lamp, false);
                    } else {
                        bv.set(*lamp, true);
                    }
                }
                "on" => bv.set(*lamp, true),
                "off" => bv.set(*lamp, false),
                _ => {}
            }
        }
    }

    println!("   Total lamps: {}", bv.iter().count());
    println!(" Enabled lamps: {}", bv.iter().filter(|x| *x).count());
    println!("Disabled lamps: {}", bv.iter().filter(|x| !*x).count());
}

fn xy2lamp(n: i32, x1: i32, y1: i32, x2: i32, y2: i32) -> Vec<usize> {
    let mut result: Vec<usize> = vec![];

    for x in x1..x2 + 1 {
        for y in y1..y2 + 1 {
            // x * n + y
            let index = (x * n + y) as usize;
            result.push(index);
        }
    }

    return result;
}

fn day06p02(_contents: String) {}
