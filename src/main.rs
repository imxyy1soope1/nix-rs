extern crate nix_rs;

use std::io::Write;

use crate::nix_rs::*;

fn main() {
    let env = new_env();
    loop {
        print!(">> ");
        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        println!("{}", eval_with_env(env.clone(), input));
    }
}
