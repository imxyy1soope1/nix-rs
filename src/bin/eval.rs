use nix_rs::eval;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    println!("{}", eval(args[1].clone()).unwrap());
}
