use crypto::digest::Digest;
use crypto::md5::Md5;

fn main() {
    let secret_key = String::from("ckczppom");

    day04p01(&secret_key);
    day04p02(&secret_key);
}

fn day04p01(secret_key: &String) {
    let mut md5hasher = Md5::new();

    for n in 1..1000001 {
        md5hasher.input_str(&format!("{}{}", &secret_key, n.to_string()));
        let hash = md5hasher.result_str();
        md5hasher.reset();

        if hash.starts_with("00000") {
            println!("md5 for {} is {}", n, hash);
            break;
        }
    }
}

fn day04p02(_secret_key: &String) {}
