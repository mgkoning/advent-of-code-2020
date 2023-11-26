pub fn run(input: String) -> Result<(), String> {
    let numbers = input
        .lines()
        .map(|v| u64::from_str_radix(v, 10))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| e.to_string())?;
    part1(&numbers)?;
    part2(numbers)?;
    Ok(())
}

fn part1(numbers: &Vec<u64>) -> Result<(), String> {
    let found = numbers
        .iter()
        .flat_map(|x| numbers.iter().filter(|&y| *x != *y).map(|y| (*x, *y)))
        .find(|(x, y)| x + y == 2020);
    if let Some((x, y)) = found {
        println!("Part 1: {}", x * y);
        Ok(())
    } else {
        Err("No candidates found".into())
    }
}

fn part2(numbers: Vec<u64>) -> Result<(), String> {
    let found = numbers
        .iter()
        .flat_map(|x| {
            numbers.iter().filter(|&y| *x != *y).flat_map(|y| {
                numbers
                    .iter()
                    .filter(|&z| *z != *x && *z != *y)
                    .map(|z| (*x, *y, *z))
            })
        })
        .find(|(x, y, z)| x + y + z == 2020);
    if let Some((x, y, z)) = found {
        println!("Part 2: {}", x * y * z);
        Ok(())
    } else {
        Err("No candidates found".into())
    }
}
