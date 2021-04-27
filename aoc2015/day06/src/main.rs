use regex::Regex;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");

    day06p01(contents.to_string(), 1000, update_bool_lamp);
    println!();
    day06p02(contents.to_string(), 1000, update_int_lamp);
}

fn day06p01(contents: String, n: i32, update: fn(&str, bool) -> bool) {
    let re = Regex::new(r"(\w+) (\d+),(\d+) through (\d+),(\d+)").unwrap();
    let size = (n * n) as usize;
    let mut vec = vec![false; size];

    for line in contents.lines() {
        let captures = re.captures(line).unwrap();

        let command: String = captures[1].to_string();
        let start_x: i32 = captures[2].parse().unwrap();
        let start_y: i32 = captures[3].parse().unwrap();
        let end_x: i32 = captures[4].parse().unwrap();
        let end_y: i32 = captures[5].parse().unwrap();

        let lamps = xy2lamp(n, start_x, start_y, end_x, end_y);

        for lamp in lamps.iter() {
            vec[*lamp] = update(command.as_ref(), vec[*lamp]);
        }
    }

    println!("   Total lamps: {}", vec.iter().count());
    println!(" Enabled lamps: {}", vec.iter().filter(|x| **x).count());
    println!("Disabled lamps: {}", vec.iter().filter(|x| !**x).count());
}

fn day06p02(contents: String, n: i32, update: fn(&str, u32) -> u32) {
    let re = Regex::new(r"(\w+) (\d+),(\d+) through (\d+),(\d+)").unwrap();
    let size = (n * n) as usize;
    let mut v: Vec<u32> = vec![0; size];
    for line in contents.lines() {
        let captures = re.captures(line).unwrap();

        let command: String = captures[1].to_string();
        let start_x: i32 = captures[2].parse().unwrap();
        let start_y: i32 = captures[3].parse().unwrap();
        let end_x: i32 = captures[4].parse().unwrap();
        let end_y: i32 = captures[5].parse().unwrap();

        let lamps = xy2lamp(n, start_x, start_y, end_x, end_y);

        for lamp in lamps.iter() {
            v[*lamp] = update(command.as_ref(), v[*lamp]);
        }
    }

    println!("Total lamps:      {}", v.iter().count());
    println!("Total brightness: {}", v.iter().sum::<u32>());
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

fn update_int_lamp(command: &str, v: u32) -> u32 {
    match command.as_ref() {
        "toggle" => v + 2,
        "on" => v + 1,
        "off" => {
            if v >= 1 {
                v - 1
            } else {
                v
            }
        }
        _ => v,
    }
}

fn update_bool_lamp(command: &str, v: bool) -> bool {
    match command.as_ref() {
        "toggle" => {
            if v {
                false
            } else {
                true
            }
        }
        "on" => true,
        "off" => false,
        _ => v,
    }
}
