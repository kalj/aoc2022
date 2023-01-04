use nalgebra::Vector2;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::iter;

#[derive(Debug, Copy, Clone)]
enum Jet {
    LeftJ,
    RightJ,
}

impl Jet {
    fn to_char(&self) -> char {
        use Jet::*;
        match self {
            &LeftJ => '<',
            &RightJ => '>',
        }
    }
}

fn parse_jets(inp: &str) -> Vec<Jet> {
    inp.chars()
        .map(|c| if c == '>' { Jet::RightJ } else { Jet::LeftJ })
        .collect()
}

#[derive(Debug)]
struct JetQueue {
    jets: Vec<Jet>,
    next_idx: usize,
}

impl JetQueue {
    fn next(&mut self) -> Jet {
        let j = self.jets[self.next_idx].clone();
        self.next_idx = (self.next_idx + 1) % self.jets.len();
        j
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Piece {
    Horiz,
    Plus,
    RevL,
    Vert,
    Cube,
}

impl Piece {
    fn next(&self) -> Piece {
        use Piece::*;
        match self {
            &Horiz => Plus,
            &Plus => RevL,
            &RevL => Vert,
            &Vert => Cube,
            &Cube => Horiz,
        }
    }

    fn get_rows(&self) -> Vec<Vec<char>> {
        use Piece::*;
        match self {
            &Horiz => vec![vec!['@', '@', '@', '@']],
            &Plus => vec![
                vec!['.', '@', '.'],
                vec!['@', '@', '@'],
                vec!['.', '@', '.'],
            ],
            &RevL => vec![
                vec!['@', '@', '@'],
                vec!['.', '.', '@'],
                vec!['.', '.', '@'],
            ],
            &Vert => vec![vec!['@'], vec!['@'], vec!['@'], vec!['@']],
            &Cube => vec![vec!['@', '@'], vec!['@', '@']],
        }
    }

    fn width(&self) -> usize {
        self.get_rows().iter().map(|s| s.len()).max().unwrap()
    }
}

struct Chamber {
    rows: Vec<[char; 7]>,
    current_piece: Piece,
    pos: Vector2<usize>,
    queue: JetQueue,
    stuck_count: usize,
}

fn format_lines(lines: &[[char; 7]]) -> Vec<String> {
    lines
        .iter()
        .rev()
        .map(|l| format!(" |{}|", String::from_iter(l.iter())))
        .collect()
}

impl Chamber {
    fn new(inp: &str) -> Chamber {
        let jets = parse_jets(&inp);
        Chamber {
            rows: vec![],
            current_piece: Piece::Horiz,
            pos: Vector2::new(3, 2),
            queue: JetQueue { jets, next_idx: 0 },
            stuck_count: 0,
        }
    }

    fn height(&self) -> usize {
        self.rows.len()
    }

    fn to_str(&self) -> String {
        let stack_height = self.height();
        let piece_vpos = self.pos[0];
        let piece_hpos = self.pos[1];
        let piece_rows = self.current_piece.get_rows();
        let piece_height = piece_rows.len();
        let tot_height = piece_height + piece_vpos;

        let mut lines: Vec<[char; 7]> = Vec::new();

        for i in 0..tot_height {
            if i < stack_height && i < piece_vpos {
                lines.push(self.rows[i]);
            } else if i >= stack_height && i < piece_vpos {
                lines.push(['.'; 7]);
            } else if i >= piece_vpos && i >= stack_height {
                let row = &piece_rows[i - piece_vpos];
                let mut padded_row = ['.'; 7];
                for j in 0..row.len() {
                    padded_row[piece_hpos + j] = row[j];
                }

                lines.push(padded_row);
            } else {
                let prow = &piece_rows[i - piece_vpos];
                let mut merged_row = self.rows[i].clone();
                for j in 0..prow.len() {
                    merged_row[piece_hpos + j] = prow[j];
                }
                lines.push(merged_row);
            }
        }

        let mut sketch_lines = format_lines(&lines);
        sketch_lines.push(" +-------+".into());

        String::from("Chamber {\n")
            // + &format!("\trows = {:?},\n",self.rows)
            + &format!("\tcurrent_piece = {:?},\n", self.current_piece)
            + &format!("\tpos = {:?},\n", self.pos)
            + &format!("\tqueue = {{\n")
            // + &format!("\t\tjets={},\n", String::from_iter(self.queue.jets.iter().map(|j|j.to_char())))
            + &format!("\t\tnext={:?} ({:?})\n", self.queue.next_idx, self.queue.jets[self.queue.next_idx])
            + &format!("\t}},\n")
            + &format!("\tstuck_count = {}\n", self.stuck_count)
            + "}\n\n"
            + &sketch_lines.join("\n")
    }

    fn piece_blocked_at(&self, pos: &Vector2<usize>) -> bool {
        let piece_rows = self.current_piece.get_rows();

        let r = pos[0];
        let c = pos[1];

        for (i, row) in piece_rows.iter().enumerate() {
            for (j, &v) in row.iter().enumerate() {
                if v == '@' && (r + i) < self.height() && self.rows[r + i][c + j] != '.' {
                    return true;
                }
            }
        }

        false
    }

    fn step(&mut self) -> bool {
        //-----------------------------------------------------------
        // process Jet
        //-----------------------------------------------------------
        let new_col = match self.queue.next() {
            Jet::LeftJ => self.pos[1] as isize - 1,
            Jet::RightJ => self.pos[1] as isize + 1,
        };

        let max_col = 7 - self.current_piece.width();

        if new_col >= 0
            && new_col <= (max_col as isize)
            && !self.piece_blocked_at(&Vector2::new(self.pos[0], new_col as usize))
        {
            self.pos[1] = new_col as usize;
        }

        //-----------------------------------------------------------
        // process fall
        //-----------------------------------------------------------

        if self.pos[0] > 0 {
            let new_row = self.pos[0] - 1;
            if !self.piece_blocked_at(&Vector2::new(new_row, self.pos[1])) {
                self.pos[0] = new_row;
                return false;
            }
        }

        //-----------------------------------------------------------
        // process stuck
        //-----------------------------------------------------------

        let piece_rows = self.current_piece.get_rows();
        let piece_width = piece_rows[0].len();
        let piece_height = piece_rows.len();

        let old_height = self.height();
        let smallest_new_height = self.pos[0] + piece_height;
        if smallest_new_height > old_height {
            let growth = smallest_new_height - old_height;
            self.rows.extend(iter::repeat(['.'; 7]).take(growth));
        }

        for i in 0..piece_height {
            for j in 0..piece_width {
                if piece_rows[i][j] == '@' {
                    self.rows[self.pos[0] + i][self.pos[1] + j] = '#';
                }
            }
        }

        self.current_piece = self.current_piece.next();
        self.pos = Vector2::new(self.height() + 3, 2);
        self.stuck_count += 1;

        return true;
    }

    fn reachable_rows(&self) -> impl Iterator<Item = &[char; 7]> {
        let mut reachable_columns = [true; 7];
        let mut banan = true;
        self.rows.iter().rev().take_while(move |&row| {
            let mut new_reachable_columns = [false; 7];
            for i in 0..7 {
                if reachable_columns[i] && row[i] == '.' {
                    // this column is directly reachable
                    new_reachable_columns[i] = true;

                    // spread right
                    for j in (i + 1)..7 {
                        if row[j] == '.' {
                            new_reachable_columns[j] = true
                        } else {
                            break;
                        }
                    }

                    // spread left
                    for j in (0..i).rev() {
                        if row[j] == '.' {
                            new_reachable_columns[j] = true
                        } else {
                            break;
                        }
                    }
                }
            }

            reachable_columns = new_reachable_columns;

            let old_banan = banan;
            banan = reachable_columns.iter().any(|&b| b);
            old_banan
        })
    }
}

fn part1(inp: &str) {
    let mut chamber = Chamber::new(inp);

    while chamber.stuck_count < 2022 {
        chamber.step();
    }

    println!("Part1 answer: {}", chamber.height());
}

fn part2(inp: &str) {
    let mut chamber = Chamber::new(inp);

    let state0 = (chamber.current_piece, 0, Vec::new());
    let mut seen_states = HashMap::from([(state0.clone(), 0)]);
    let mut history = vec![(state0, 0)];

    loop {
        if !chamber.step() {
            continue;
        }

        let toprows: Vec<[char; 7]> = chamber.reachable_rows().copied().collect();
        let state = (
            chamber.current_piece,
            chamber.queue.next_idx,
            toprows.clone(),
        );
        history.push((state.clone(), chamber.height()));

        // println!("{}", chamber.to_str());
        if seen_states.contains_key(&state) {
            // println!("breaking loop");
            break;
        }

        seen_states.insert(state, chamber.stuck_count);
    }

    let toprows: Vec<[char; 7]> = chamber.reachable_rows().copied().collect();
    let prev_stuck_count = seen_states[&(
        chamber.current_piece,
        chamber.queue.next_idx,
        toprows.clone(),
    )];
    // println!(" top rows:");
    // for l in format_lines(&toprows).iter().rev() {
    //     println!("{}", l);
    // }

    let prev_height = history[prev_stuck_count].1;
    let loop_len = chamber.stuck_count - prev_stuck_count;
    let loop_height = chamber.height() - prev_height;

    // println!("  current_piece={:?}", chamber.current_piece);
    // println!("  queue_idx={}", chamber.queue.next_idx);
    // println!("  stuck_count={}", chamber.stuck_count);
    // println!("  prev_stuck_count={}", prev_stuck_count);
    // println!("  height={}", chamber.height());
    // println!("  prev_height={}", prev_height);
    // println!("  loop_len={}", loop_len);
    // println!("  loop_height={}", loop_height);

    let target_index: usize = 1000_000_000_000;

    let prefix = prev_stuck_count;
    let n_loops = (target_index - prefix) / loop_len;
    let suffix = target_index - prefix - n_loops * loop_len;

    // println!("prefix: {}", prefix);
    // println!("n_loops: {}", n_loops);
    // println!("suffix: {}", suffix);

    let prefix_height = prev_height;
    let tot_loop_height = n_loops * loop_height;
    let suffix_height = history[prev_stuck_count + suffix].1 - prev_height;
    let tot_height = prefix_height + tot_loop_height + suffix_height;
    // println!("prefix_height: {}",prefix_height);
    // println!("tot_loop_height: {}",tot_loop_height);
    // println!("suffix_height: {}",suffix_height);
    // println!("tot_height: {}",tot_height);

    println!("Part 2 answer: {}", tot_height);
}

fn main() {
    let inp = fs::read_to_string(env::args().nth(1).unwrap()).unwrap();

    part1(&inp);

    part2(&inp);
}
