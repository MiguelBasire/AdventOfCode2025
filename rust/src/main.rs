use std::{
    fs,
    io::{BufRead, BufReader},
};

use crate::days::day1;

pub mod days;

#[derive(Debug)]
struct PuzzleError;

trait PuzzleResolver {
    fn resolve(file_name: &str) -> Result<String, PuzzleError>;
}

fn main() {
    println!("{}", day1::Puzzle1::resolve("day1.txt").unwrap());
    println!("{}", day1::Puzzle2::resolve("day1.txt").unwrap());
    println!("END");
}

fn read_lines<R: std::str::FromStr>(file_name: &str) -> Result<Vec<R>, Box<dyn std::error::Error>>
where
    R::Err: std::fmt::Debug,
{
    let file = fs::File::open(file_name)?;
    let reader = BufReader::new(file);

    reader
        .lines()
        .enumerate()
        .map(|(i, raw_line)| {
            let line = raw_line.map_err(|e| format!("IO error at line {}: {}", i + 1, e))?;
            let record = line
                .parse::<R>()
                .map_err(|e| format!("Parse error at line {}: {:?}", i + 1, e))?;
            Ok(record)
        })
        .collect::<Result<Vec<_>, _>>()
}
