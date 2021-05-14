use std::collections::VecDeque;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");

    day08p01(&contents);
    day08p02(&contents);
}

fn unescape(s: &str) -> Option<String> {
    let mut queue: VecDeque<_> = String::from(s).chars().collect();
    let mut s = String::new();

    while let Some(c) = queue.pop_front() {
        if c != '\\' {
            s.push(c);
            continue;
        }

        match queue.pop_front() {
            Some('\"') => s.push('A'),
            Some('\\') => s.push('B'),
            Some('x') => {
                s.push('C');
                queue.pop_front();
                queue.pop_front();
            }
            _ => return None,
        };
    }

    Some(s)
}

/**
 * count the characters on a single line,
 * after removing any quoting
 **/
fn count_line(line: &str) -> i32 {
    let f = line
        .strip_prefix("\"")
        .and_then(|s| s.strip_suffix("\""))
        .and_then(|s| unescape(&s));

    match f {
        None => 0,
        Some(s) => s.len() as i32,
    }
}

fn day08p01(contents: &String) {
    // we have to subtract the newlines
    let code_count = (contents.chars().count() - contents.lines().count()) as i32;
    let memory_count = contents.lines().map(|x| count_line(x)).sum::<i32>();

    println!("Code count: {}", code_count);
    println!("Memory count: {}", memory_count);
    println!("Code - Memory: {}", code_count - memory_count);
}

fn day08p02(_contents: &String) {}
