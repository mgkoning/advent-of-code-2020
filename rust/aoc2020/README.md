# Rust solutions to Advent of Code 2020

## Set up
Install rust using rustup.

## Running puzzles
From this directory (aoc2020), execute `cargo run` to install dependencies, build, and run the
application. By default it will check the current date and run the puzzle number associated with
the day. If you wish to run a specific day, use `cargo run <day>`. To run in release (optimized)
mode, run `cargo run -r <day>`.

Note that the application expects to find inputs named day00.txt in the input directory in the root
of the repository. Make sure the current working directory is this directory (`aoc2020`) as the
path to the inputs is hardcoded.