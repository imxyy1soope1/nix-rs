use nix_rs::*;
use rustyline::Config;
use rustyline::error::ReadlineError;
use rustyline::{Editor, Result, history::MemHistory};

fn main() -> Result<()> {
    let env = new_env();
    let mut rl = Editor::<(), MemHistory>::with_history(Config::default(), MemHistory::new()).unwrap();
    loop {
        let readline = rl.readline("nix-rs-eval> ");
        match readline {
            Ok(line) => {
                if line.trim().is_empty() {
                    continue
                }
                match eval_with_env(env.clone(), line) {
                    Ok(o) => println!("{}", o),
                    Err(e) => eprintln!("{}", e),
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    Ok(())
}
