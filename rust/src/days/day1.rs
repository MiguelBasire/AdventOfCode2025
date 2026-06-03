use std::{fmt::Display, iter, str::FromStr};

use crate::{PuzzleError, PuzzleResolver, read_lines};

pub enum Rotations {
    Left(u16),
    Right(u16),
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

    fn rotate(self, rotations: Rotations) -> impl Iterator<Item = Dial> {
        let mut current = self;
        iter::from_fn(move || match rotations {
            Rotations::Left(d) => {
                current.turn_left();
                Some((d as usize, current))
            }

            Rotations::Right(d) => {
                current.turn_right();
                Some((d as usize, current))
            }
        })
        .enumerate()
        .take_while(|(i, (d, _))| i < d)
        .map(|(_, (_, dial))| dial)
    }
}

pub struct Puzzle1;
impl PuzzleResolver for Puzzle1 {
    fn resolve(&self, file_name: &str) -> Result<String, PuzzleError> {
        let mut zeros = 0;
        let mut dial = Dial {
            size: 100,
            position: 50,
        };

        match read_lines::<Rotations>(file_name) {
            Ok(rotations) => {
                for r in rotations {
                    dial.rotate(r).for_each(|d| {
                        dial = d;
                    });
                    if dial.position == 0 {
                        zeros += 1
                    }
                }
                Ok(format!("{}", zeros))
            }
            Err(err) => Err(err),
        }
    }
}

pub struct Puzzle2;
impl PuzzleResolver for Puzzle2 {
    fn resolve(&self, file_name: &str) -> Result<String, PuzzleError> {
        let mut zeros = 0;
        let mut dial = Dial {
            size: 100,
            position: 50,
        };

        match read_lines::<Rotations>(file_name) {
            Ok(rotations) => {
                for r in rotations {
                    dial.rotate(r).for_each(|d| {
                        if dial.position == 0 {
                            zeros += 1
                        }
                        dial = d;
                    });
                }
                Ok(format!("{}", zeros))
            }
            Err(e) => Err(e),
        }
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
