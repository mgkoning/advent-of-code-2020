pub fn run(input: String) -> Result<(), String> {
    let input = input.lines().map(read_line).collect::<Vec<_>>();

    let valid_passwords =
        |check: fn(&Policy, &String) -> bool| input.iter().filter(|(p, pw)| check(p, pw)).count();

    let part1 = valid_passwords(is_valid_part1);
    println!("Part 1:\n{part1}");
    let part2 = valid_passwords(is_valid_part2);
    println!("Part 2:\n{part2}");
    Ok(())
}

fn is_valid_part1(policy: &Policy, password: &String) -> bool {
    let counts = password.chars().filter(|&x| x == policy.character).count() as u32;
    policy.x <= counts && counts <= policy.y
}

fn is_valid_part2(policy: &Policy, password: &String) -> bool {
    let check_pos = |i| {
        password
            .chars()
            .nth(i as usize - 1)
            .map(|v| v == policy.character)
    };
    match (check_pos(policy.x), check_pos(policy.y)) {
        (Some(one), Some(other)) => one ^ other,
        _ => false,
    }
}

fn read_line(line: &str) -> (Policy, String) {
    let parts: Vec<_> = line
        .split(['-', ':', ' '])
        .filter(|&s| !s.is_empty())
        .collect();
    let policy = Policy {
        x: parts[0].parse().unwrap(),
        y: parts[1].parse().unwrap(),
        character: parts[2].chars().nth(0).unwrap(),
    };
    (policy, parts[3].to_string())
}

struct Policy {
    x: u32,
    y: u32,
    character: char,
}
