use std::fs;

fn main() {
    let inp = fs::read_to_string("../input").unwrap();

    let elf_parts = inp.split("\n\n").map(|elf_part| {
        elf_part
            .split('\n')
            .filter_map(|calstr| calstr.parse::<i32>().ok())
    });

    let mut sums = elf_parts.map(|elfcals| elfcals.sum()).collect::<Vec<_>>();

    // sums.sort();
    sums.sort_unstable();

    let topsum: i32 = sums.iter().rev().take(3).sum();
    println!("top sum: {}", topsum);
}
