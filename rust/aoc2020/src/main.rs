use std::{
    collections::HashMap,
    env::{self, Args},
    fs,
    num::ParseIntError,
};

use chrono::Datelike;
mod day01;
mod day02;

fn main() -> Result<(), String> {
    let puzzle = get_puzzle(env::args()).map_err(|e| e.to_string())?;
    let day_runner = get_day_runner(puzzle).ok_or(format!("Day {puzzle} not supported"))?;
    let input = get_input(puzzle)?;
    day_runner(input)
}

fn get_day_runner(puzzle: u32) -> Option<DayRunner> {
    return build_runner_map().get(&puzzle).copied();
}

type DayRunner = fn(String) -> Result<(), String>;
fn build_runner_map() -> HashMap<u32, DayRunner> {
    HashMap::from([(1, day01::run as DayRunner), (2, day02::run)])
}

fn get_puzzle(args: Args) -> Result<u32, ParseIntError> {
    args.skip(1)
        .next()
        .map(|s| s.parse())
        .unwrap_or_else(|| Ok(chrono::Local::now().date_naive().day()))
}

fn get_input(puzzle: u32) -> Result<String, String> {
    let filename = format!("day{:02}.txt", puzzle);
    fs::read_to_string(format!("../../input/{filename}")).map_err(|e| e.to_string())
}
