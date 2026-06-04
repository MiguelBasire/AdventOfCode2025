use std::{fmt::Display, iter, str::FromStr};

use crate::{PuzzleError, PuzzleResolver, read_lines};

pub enum Rotations {
    Left(u16),
    Right(u16),
}

#[derive(Clone)]
enum Rotation {
    Left,
    Right,
}

#[derive(Debug)]
pub enum ParseRotationError {
    BadFormat,
    ParseInt(std::num::ParseIntError),
}

impl From<std::num::ParseIntError> for ParseRotationError {
    fn from(value: std::num::ParseIntError) -> Self {
        ParseRotationError::ParseInt(value)
    }
}

impl FromStr for Rotations {
    type Err = ParseRotationError;

    fn from_str(item: &str) -> Result<Self, Self::Err> {
        match item.split_at(1) {
            ("L", distance) => distance
                .parse()
                .map(Rotations::Left)
                .map_err(ParseRotationError::ParseInt),

            ("R", distance) => distance
                .parse()
                .map(Rotations::Right)
                .map_err(ParseRotationError::ParseInt),

            _ => Err(ParseRotationError::BadFormat),
        }
    }
}

impl Display for Rotations {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rotations::Right(distance) => write!(f, "rotation(RIGHT, {})", distance),
            Rotations::Left(distance) => write!(f, "rotation(LEFT, {})", distance),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Dial {
    size: u8,
    position: u8,
}

impl Dial {
    fn turn_right(&mut self) {
        self.position = if self.position == self.size - 1 {
            0
        } else {
            self.position + 1
        }
    }

    fn turn_left(&mut self) {
        self.position = if self.position == 0 {
            self.size - 1
        } else {
            self.position - 1
        }
    }

    fn at_zero(self) -> bool {
        self.position == 0
    }
}

fn rotations(r: Rotations) -> impl Iterator<Item = Rotation> {
    match r {
        Rotations::Right(d) => iter::repeat_n(Rotation::Right, d as usize),
        Rotations::Left(d) => iter::repeat_n(Rotation::Left, d as usize),
    }
}

pub struct Puzzle1;
impl PuzzleResolver for Puzzle1 {
    fn resolve(&self, file_name: &str) -> Result<String, PuzzleError> {
        let dial = Dial {
            size: 100,
            position: 50,
        };

        read_lines::<Rotations>(file_name).map(|rs| {
            let zeros = rs
                .scan(dial, |rotating_dial, rs| {
                    rotations(rs).for_each(|r| match r {
                        Rotation::Right => rotating_dial.turn_right(),
                        Rotation::Left => rotating_dial.turn_left(),
                    });
                    Some(*rotating_dial)
                })
                .filter(|d| d.at_zero())
                .count();

            format!("{}", zeros)
        })
    }
}

pub struct Puzzle2;
impl PuzzleResolver for Puzzle2 {
    fn resolve(&self, file_name: &str) -> Result<String, PuzzleError> {
        let dial = Dial {
            size: 100,
            position: 50,
        };

        read_lines::<Rotations>(file_name).map(|rs| {
            let zeros = rs
                .flat_map(rotations)
                .scan(dial, |rotating_dial, r| {
                    match r {
                        Rotation::Right => rotating_dial.turn_right(),
                        Rotation::Left => rotating_dial.turn_left(),
                    };
                    Some(*rotating_dial)
                })
                .filter(|d| d.at_zero())
                .count();

            format!("{}", zeros)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_puzzle1_sample() {
        assert_eq!(Puzzle1.resolve("day1.txt.sample").unwrap(), "3");
    }

    #[test]
    fn verify_puzzle2_sample() {
        assert_eq!(Puzzle2.resolve("day1.txt.sample").unwrap(), "6")
    }
}
