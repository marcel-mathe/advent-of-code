use crypto::digest::Digest;
use crypto::md5::Md5;
use rayon::prelude::*;

fn main() {
    let secret_key = String::from("ckczppom");

    search(&secret_key, 1..1000001, "00000");
    search(&secret_key, 1..10000001, "000000");
}

fn search(secret_key: &str, range: std::ops::Range<i32>, pattern: &str) {
    range.into_par_iter().for_each(|n: i32| {
        let mut md5hasher = Md5::new();
        md5hasher.input_str(&format!("{}{}", &secret_key, n.to_string()));
        let hash = md5hasher.result_str();

        if hash.starts_with(pattern) {
            println!("md5 for {} is {}", n, hash);
        }
    });
}
