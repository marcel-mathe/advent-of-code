use std::cmp::Eq;
use std::collections::HashSet;
use std::env;
use std::fs;
use std::iter::Iterator;
use std::sync::mpsc;
use std::thread;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");

    day03p01(&contents);
    day03p02(&contents);
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct House {
    x: i32,
    y: i32,
}

fn day03p01(contents: &String) {
    let mut visited_houses: HashSet<House> = HashSet::new();
    let mut current_house = House { x: 0, y: 0 };
    visited_houses.insert(current_house);

    for s in contents.chars() {
        match s {
            '^' => current_house.y += 1,
            'v' => current_house.y -= 1,
            '>' => current_house.x += 1,
            '<' => current_house.x -= 1,
            _ => continue,
        }
        visited_houses.insert(current_house);
    }

    println!("Number of visited houses: {}", visited_houses.len());
}

fn day03p02(contents: &String) {
    let (tx_santa, rx) = mpsc::channel();
    let tx_robot = tx_santa.clone();
    let start = House { x: 0, y: 0 };
    let santa_list: Vec<char> = contents.chars().step_by(2).collect();
    let robot_list: Vec<char> = contents.chars().skip(1).step_by(2).collect();
    let mut visited_houses: HashSet<House> = HashSet::new();

    visited_houses.insert(start);

    thread::spawn(move || {
        let mut current_santa_house = start;

        for direction in santa_list {
            match direction {
                '^' => current_santa_house.y += 1,
                'v' => current_santa_house.y -= 1,
                '>' => current_santa_house.x += 1,
                '<' => current_santa_house.x -= 1,
                _ => continue,
            }
            tx_santa.send(current_santa_house).unwrap();
        }
    });

    thread::spawn(move || {
        let mut current_robot_house = start;

        for direction in robot_list {
            match direction {
                '^' => current_robot_house.y += 1,
                'v' => current_robot_house.y -= 1,
                '>' => current_robot_house.x += 1,
                '<' => current_robot_house.x -= 1,
                _ => continue,
            }
            tx_robot.send(current_robot_house).unwrap();
        }
    });

    for received in rx {
        visited_houses.insert(received);
    }

    println!("Number of visited houses: {}", visited_houses.len());
}
