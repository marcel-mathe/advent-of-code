use std::cmp::Eq;
use std::collections::HashSet;
use std::env;
use std::fs;
use std::iter::Iterator;
use std::sync::mpsc;
use std::sync::mpsc::{Sender, Receiver};
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
    let (santa_tx, rx): (Sender<House>, Receiver<House>) = mpsc::channel();
    let robot_tx = santa_tx.clone();
    let starting_house = House { x: 0, y: 0 };
    let santa_list: Vec<char> = contents.chars().step_by(2).collect();
    let robot_list: Vec<char> = contents.chars().skip(1).step_by(2).collect();
    let mut visited_houses: HashSet<House> = HashSet::new();
    
    visited_houses.insert(starting_house);

    thread::spawn(move || walker(starting_house, santa_list, santa_tx));
    thread::spawn(move || walker(starting_house, robot_list, robot_tx));
    
    for received in rx {
        visited_houses.insert(received);
    }

    println!("Number of visited houses: {}", visited_houses.len());
}

fn walker (starting_house: House, directions: Vec<char>, sender: Sender<House>) {
    let mut current_house = starting_house;

    for direction in directions {
        match direction {
            '^' => current_house.y += 1,
            'v' => current_house.y -= 1,
            '>' => current_house.x += 1,
            '<' => current_house.x -= 1,
            _ => continue,
        }

        sender.send(current_house).unwrap();
    }
}