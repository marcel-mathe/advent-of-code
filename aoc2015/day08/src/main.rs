use std::collections::VecDeque;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");

    day08p01(&contents);
    println!("");
    day08p02(&contents, 6202);
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
    return line
        .strip_prefix("\"")
        .and_then(|s| s.strip_suffix("\""))
        .and_then(|s| unescape(&s))
        .map(|s| s.len() as i32)
        .unwrap_or(0);
}

fn day08p01(contents: &String) {
    // all characters minus newlines
    let code_count = (contents.chars().count() - contents.lines().count()) as i32;
    // all unescaped characters
    let memory_count = contents.lines().fold(0, |acc, x| acc + count_line(x));

    println!("Code count: {}", code_count);
    println!("Memory count: {}", memory_count);
    println!("Code - Memory: {}", code_count - memory_count);
}

fn escape(c: &str) -> &str {
    match c {
        "\\" => return "\\\\",
        "\"" => return "\\\"",
        _ => return c,
    }
}

fn count_escaped_line(line: &str) -> i32 {
    let mut s = String::new();
    s.push('"');

    line.chars().for_each(|c| {
        s.push_str(escape(&c.to_string()));
    });

    s.push('"');

    return s.len() as i32;
}

fn day08p02(contents: &String, prev: i32) {
    let new_count = contents
        .lines()
        .fold(0, |acc, x| acc + count_escaped_line(x));

    println!("New String: {}", new_count);
    println!("Previous String: {}", prev);
    println!("Difference: {}", new_count - prev);
}
