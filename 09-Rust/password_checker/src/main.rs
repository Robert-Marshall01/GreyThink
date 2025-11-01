use clap::Parser;
use std::process;
use std::io::Write;

use password_checker::{classify_score, mask_password, score_password};

#[derive(Parser, Debug)]
#[command(author, version, about = "Simple password strength checker (CLI)", long_about = None)]
struct Args {
    /// Password to check as a flag. If omitted, you can provide a positional arg or you'll be prompted.
    #[arg(short, long, value_name = "PASSWORD")]
    password: Option<String>,

    /// Password as the first positional argument (convenience).
    #[arg(value_name = "PASSWORD", index = 1)]
    positional_password: Option<String>,

    /// Output JSON instead of human-readable text
    #[arg(long)]
    json: bool,

    /// Minimum acceptable score (exit with code 3 if below)
    #[arg(long)]
    min_score: Option<i32>,
}

fn main() {
    let args = Args::parse();

    // prefer explicit -p/--password if provided, otherwise accept positional arg if present,
    // otherwise prompt interactively.
    let password = match args.password.or(args.positional_password) {
        Some(p) => p,
        None => {
            // Use hidden input so typed password isn't echoed. Print a prompt and flush first.
            print!("Enter password: ");
            std::io::stdout().flush().ok();
            match rpassword::read_password() {
                Ok(s) => s,
                Err(_) => {
                    eprintln!("Failed to read password from terminal.");
                    process::exit(2);
                }
            }
        }
    };

    if password.is_empty() {
        eprintln!("No password provided.");
        process::exit(2);
    }

    let (score, suggestions) = score_password(&password);
    let classification = classify_score(score).to_string();

    if args.json {
        // build a small JSON structure and print
        #[derive(serde::Serialize)]
        struct Out<'a> {
            masked: String,
            score: i32,
            classification: &'a str,
            suggestions: Vec<String>,
        }

        let out = Out {
            masked: mask_password(&password),
            score,
            classification: classify_score(score),
            suggestions,
        };

        match serde_json::to_string_pretty(&out) {
            Ok(js) => println!("{}", js),
            Err(e) => eprintln!("Failed to serialize JSON: {}", e),
        }
    } else {
        println!("\nPassword: {}", mask_password(&password));
        println!("Strength: {} (score {})", classification, score);
        if !suggestions.is_empty() {
            println!("Suggestions:");
            for s in suggestions {
                println!(" - {}", s);
            }
        }
    }

    if let Some(min) = args.min_score {
        if score < min {
            eprintln!("Password score {} is below minimum {}.", score, min);
            process::exit(3);
        }
    }
}
