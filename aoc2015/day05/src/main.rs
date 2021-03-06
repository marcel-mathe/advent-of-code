use fancy_regex::Regex;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");

    day05p01(contents.to_string());
    println!("");
    day05p02(contents.to_string());
}

fn is_vowel(c: char) -> bool {
    return c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u';
}

fn has_three_vowels(s: &str) -> bool {
    let mut vowel_count = 0;

    for c in s.chars() {
        if is_vowel(c) {
            vowel_count += 1;
        }

        if vowel_count >= 3 {
            return true;
        }
    }
    return false;
}

fn pairs(s: &str) -> Vec<String> {
    let mut pairs: Vec<String> = Vec::with_capacity(s.len());

    for i in 0..(s.len() - 1) {
        let first = s.chars().nth(i).unwrap();
        let second = s.chars().nth(i + 1).unwrap();
        let mut pair = String::with_capacity(2);
        pair.push(first);
        pair.push(second);

        pairs.push(pair);
    }

    return pairs;
}

fn twice_in_a_row(s: &str) -> bool {
    return pairs(s)
        .into_iter()
        .any(|s| s.chars().nth(0) == s.chars().nth(1));
}

fn has_forbidden_strings(s: &str) -> bool {
    return s.contains("ab") || s.contains("cd") || s.contains("pq") || s.contains("xy");
}

fn has_no_forbidden_strings(s: &str) -> bool {
    return !has_forbidden_strings(s);
}

fn is_nice(s: &str) -> bool {
    return has_three_vowels(s) && twice_in_a_row(s) && has_no_forbidden_strings(s);
}

fn has_non_overlapping_pairs(s: &str) -> bool {
    for pair in pairs(s) {
        if s.matches(&pair).count() >= 2 {
            return true;
        }
    }
    return false;
}

fn has_efe(s: &str) -> bool {
    let re: Regex = Regex::new(r"(.).\1").unwrap();
    return re.is_match(s).unwrap();
}

fn is_new_nice(s: &str) -> bool {
    return has_non_overlapping_pairs(s) && has_efe(s);
}

fn day05p01(contents: String) {
    test(contents, is_nice);
}

fn day05p02(contents: String) {
    test(contents, is_new_nice);
}

fn test(contents: String, f: fn(&str) -> bool) {
    let tested: Vec<bool> = contents.lines().map(f).collect();
    let nice: Vec<bool> = tested.clone().into_iter().filter(|x| *x).collect();
    let naughty: Vec<bool> = tested.clone().into_iter().filter(|x| !*x).collect();

    println!("Number of     all strings: {}", tested.len());
    println!("Number of    nice strings: {}", nice.len());
    println!("Number of naughty strings: {}", naughty.len());
}
