use std::fs;
use std::env;
use std::iter;
use nalgebra::Vector2;

#[derive(Debug, Copy, Clone)]
enum Jet {
    LeftJ,
    RightJ
}

impl Jet {
    fn to_char(&self) -> char {
        use Jet::*;
        match self {
            &LeftJ => '<',
            &RightJ => '>'
        }
    }
}

fn parse_jets(inp: &str) -> Vec<Jet> {
    inp.chars().map(|c| if c=='>' { Jet::RightJ } else { Jet::LeftJ } ).collect()
}

#[derive(Debug)]
struct JetQueue {
    jets : Vec<Jet>,
    next_idx : usize
}

impl JetQueue {
    fn next(&mut self) -> Jet {
        let j = self.jets[self.next_idx].clone();
        self.next_idx = (self.next_idx+1)%self.jets.len();
        j
    }
}

#[derive(Debug, PartialEq)]
enum Piece {
    Horiz,
    Plus,
    RevL,
    Vert,
    Cube
}

impl Piece {
    fn next(&self) -> Piece {
        use Piece::*;
        match self {
            &Horiz => Plus,
            &Plus => RevL,
            &RevL => Vert,
            &Vert => Cube,
            &Cube => Horiz
        }
    }

    fn get_rows(&self) -> Vec<Vec<char>> {
        use Piece::*;
        match self {
            &Horiz => vec![vec!['@','@','@','@']],
            &Plus => vec![vec!['.','@','.'],
                          vec!['@','@','@'],
                          vec!['.','@','.']],
            &RevL => vec![vec!['@','@','@'],
                          vec!['.','.','@'],
                          vec!['.','.','@']],
            &Vert => vec![vec!['@'],
                          vec!['@'],
                          vec!['@'],
                          vec!['@']],
            &Cube => vec![vec!['@','@'],
                          vec!['@','@']]
        }
    }

    fn width(&self) -> usize {
        self.get_rows().iter().map(|s| s.len()).max().unwrap()
    }
}

struct Chamber {
    rows : Vec<[char;7]>,
    current_piece : Piece,
    pos : Vector2<usize>,
    queue : JetQueue,
    stuck_count : usize
}

impl Chamber {
    fn to_str(&self) -> String {

        let stack_height = self.rows.len();
        let piece_vpos = self.pos[0];
        let piece_hpos = self.pos[1];
        let piece_rows = self.current_piece.get_rows();
        let piece_height = piece_rows.len();
        let tot_height = piece_height + piece_vpos;

        let mut lines : Vec<[char;7]> = Vec::new();

        for i in 0..tot_height {
            if i < stack_height && i < piece_vpos {
                lines.push(self.rows[i]);
            } else if i>=stack_height && i< piece_vpos{
                lines.push(['.';7]);
            } else if i>=piece_vpos && i>=stack_height {
                let row = &piece_rows[i-piece_vpos];
                let mut padded_row = ['.';7];
                for j in 0..row.len() {
                    padded_row[piece_hpos+j] = row[j];
                }

                lines.push(padded_row);
            } else {
                let prow = &piece_rows[i-piece_vpos];
                let mut merged_row = self.rows[i].clone();
                for j in 0..prow.len() {
                    merged_row[piece_hpos+j] = prow[j];
                }
                lines.push(merged_row);
            }
        }

        let mut sketch_lines : Vec<String> = lines.iter().rev().map(|l| format!(" |{}|",String::from_iter(l.iter()))).collect();

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


    fn new(inp: &str) -> Chamber {
        let jets = parse_jets(&inp);
        Chamber {
            rows : vec![],
            current_piece : Piece::Horiz,
            pos : Vector2::new(3,2),
            queue : JetQueue {
                jets,
                next_idx : 0
            },
            stuck_count : 0
        }

    }

    fn piece_blocked_at(&self, pos : &Vector2<usize>) -> bool {
        let piece_rows = self.current_piece.get_rows();

        let r = pos[0];
        let c = pos[1];

        for (i,row) in piece_rows.iter().enumerate() {
            for (j,&v) in row.iter().enumerate() {
                if v=='@' && (r+i)<self.rows.len() && self.rows[r+i][c+j] != '.' {
                    return true
                }
            }
        }

        false
    }

    fn step(&mut self) {

        //-----------------------------------------------------------
        // process Jet
        //-----------------------------------------------------------
        let new_col = match self.queue.next() {
            Jet::LeftJ => self.pos[1] as isize-1,
            Jet::RightJ => self.pos[1] as isize+1,
        };

        let max_col = 7 - self.current_piece.width();

        if new_col>=0 && new_col<=(max_col as isize) && !self.piece_blocked_at(&Vector2::new(self.pos[0],new_col as usize)) {
            self.pos[1] = new_col as usize;
        }

        //-----------------------------------------------------------
        // process fall
        //-----------------------------------------------------------

        if self.pos[0] > 0 {
            let new_row = self.pos[0]-1;
            if !self.piece_blocked_at(&Vector2::new(new_row, self.pos[1])) {
                self.pos[0] = new_row;
                return;
            }
        }

        //-----------------------------------------------------------
        // process stuck
        //-----------------------------------------------------------

        let piece_rows = self.current_piece.get_rows();
        let piece_width = piece_rows[0].len();
        let piece_height = piece_rows.len();

        let old_height = self.rows.len();
        let smallest_new_height = self.pos[0] + piece_height;
        if smallest_new_height > old_height {
            let growth = smallest_new_height - old_height;
            self.rows.extend(iter::repeat(['.';7]).take(growth));
        }

        for i in 0..piece_height {
            for j in 0..piece_width {
                if piece_rows[i][j] == '@' {
                    self.rows[self.pos[0]+i][self.pos[1]+j] = '#';
                }
            }
        }

        self.current_piece = self.current_piece.next();
        self.pos = Vector2::new(self.rows.len() + 3, 2);
        self.stuck_count += 1;
    }
}


fn part1(inp: &str) {
    let mut chamber = Chamber::new(inp);

    while chamber.stuck_count < 2022 {
        chamber.step();
    }

    println!("Final chamber height: {}", chamber.rows.len());
}


fn part2(inp: &str) {
    let mut chamber = Chamber::new(inp);

    // println!("Initial chamber: {}", chamber.to_str());

    while chamber.stuck_count < 6000000 {

        if chamber.current_piece == Piece::Horiz && chamber.queue.next_idx == 0 {
            println!("--------------------");

            for line in chamber.rows.iter().rev().take(10) {

                println!("{}", String::from_iter(line));
            }
            println!("---------------------");

        }

        chamber.step();
    }

    // println!("Chamber finally: {}", chamber.to_str());
}

fn main() {
    let inp = fs::read_to_string(env::args().nth(1).unwrap()).unwrap();

    part1(&inp);

    // part2(&inp);
}
