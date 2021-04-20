use std::cmp::Eq;
use std::collections::HashSet;
use std::env;
use std::fs;

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
            '^' => {
                current_house.y += 1;
                visited_houses.insert(current_house);
            }
            'v' => {
                current_house.y -= 1;
                visited_houses.insert(current_house);
            }
            '>' => {
                current_house.x += 1;
                visited_houses.insert(current_house);
            }
            '<' => {
                current_house.x -= 1;
                visited_houses.insert(current_house);
            }
            _ => continue,
        }
    }

    println!("Number of visited houses: {}", visited_houses.len());
}

fn day03p02(contents: &String) {
    let start = House { x: 0, y: 0 };
    let mut visited_houses: HashSet<House> = HashSet::new();
    let mut current_santa_house = start;
    let mut current_robot_house = start;
    let mut santa: bool = true;

    visited_houses.insert(current_santa_house);

    for s in contents.chars() {
        if santa {
            match s {
                '^' => {
                    current_santa_house.y += 1;
                    visited_houses.insert(current_santa_house);
                }
                'v' => {
                    current_santa_house.y -= 1;
                    visited_houses.insert(current_santa_house);
                }
                '>' => {
                    current_santa_house.x += 1;
                    visited_houses.insert(current_santa_house);
                }
                '<' => {
                    current_santa_house.x -= 1;
                    visited_houses.insert(current_santa_house);
                }
                _ => continue,
            }
            santa = false;
        } else {
            match s {
                '^' => {
                    current_robot_house.y += 1;
                    visited_houses.insert(current_robot_house);
                }
                'v' => {
                    current_robot_house.y -= 1;
                    visited_houses.insert(current_robot_house);
                }
                '>' => {
                    current_robot_house.x += 1;
                    visited_houses.insert(current_robot_house);
                }
                '<' => {
                    current_robot_house.x -= 1;
                    visited_houses.insert(current_robot_house);
                }
                _ => continue,
            }
            santa = true;
        }
    }

    println!("Number of visited houses: {}", visited_houses.len());
}
