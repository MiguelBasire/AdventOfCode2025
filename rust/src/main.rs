use std::{
    fs,
    io::{BufRead, BufReader},
};

use crate::days::day1;

pub mod days;

#[derive(Debug)]
struct PuzzleError(String);

impl From<std::io::Error> for PuzzleError {
    fn from(value: std::io::Error) -> Self {
        PuzzleError(value.to_string())
    }
}

trait PuzzleResolver {
    fn resolve(&self, file_name: &str) -> Result<String, PuzzleError>;
}

struct Puzzle<'a> {
    name: &'a str,
    file_name: &'a str,
    resolver: &'a dyn PuzzleResolver,
}

fn main() {
    let puzzles = &[
        Puzzle {
            name: "Day1->puzzle1",
            file_name: "day1.txt",
            resolver: &day1::Puzzle1,
        },
        Puzzle {
            name: "Day1->puzzle2",
            file_name: "day1.txt",
            resolver: &day1::Puzzle2,
        },
    ];

    for puzzle in puzzles {
        println!(
            "{}'s result: {:?}",
            puzzle.name,
            puzzle.resolver.resolve(puzzle.file_name).unwrap()
        );
    }
}

fn read_lines<R: std::str::FromStr>(file_name: &str) -> Result<impl Iterator<Item = R>, PuzzleError>
where
    R::Err: std::fmt::Debug,
{
    let file = fs::File::open(file_name)?;
    let lines = BufReader::new(file).lines();

    Ok(lines
        .inspect(|l| {
            if let Err(e) = l {
                panic!("ERROR ! {:?}", e)
            }
        })
        .filter_map(Result::ok)
        .enumerate()
        .map(|(i, line)| {
            line.parse::<R>()
                .inspect_err(|e| panic!("Line {},  {:?}: {}", i, e, line))
        })
        .filter_map(Result::ok))
}
