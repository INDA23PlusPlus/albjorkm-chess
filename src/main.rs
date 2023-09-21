use scrappy_chess::chess::{Move, MoveSet, ChessState, parse_coordinate};
use scrappy_chess::util::{print_ascii_board, state_to_ascii};

fn read_valid_move(moves: &[Move]) -> Option<&Move> {
    loop {
        let stdin = std::io::stdin();
        let mut input = String::new();
        if let Err(e) = stdin.read_line(&mut input) {
            println!("Could not read input: {e}");
            return None
        }
        input = input.replace(&['\n', '\r', ' '], "");
        if input == "help" {
            println!("Valid moves: {:#?}", moves);
            continue
        }
        if input.len() < 4 {
            println!("Please write in the form of: a2 a3");
            continue
        }

        let from = &input[0..2];
        let to = &input[2..4];
        let square = match parse_coordinate(from) {
            Err(e) => {println!("Invalid coordinate {from}: {:#?}", e); continue}
            Ok(square) => square
        } as u8;
        let square_dest = match parse_coordinate(to) {
            Err(e) => {println!("Invalid destination {from}: {:#?}", e); continue}
            Ok(square) => square
        } as u8;
        println!("Trying to move from: {square} to {square_dest}");
        for mov in moves {
            if mov.from == square && mov.to == square_dest {
                return Some(mov)
            }
        }
        println!("Invalid move, try another");
    }
}

fn main() {
    let move_set = MoveSet::new();
    let mut state = ChessState::standard();

    loop {
        let ascii = state_to_ascii(&state);
        for _ in 0..64 {
            println!("");
        }
        print_ascii_board(&ascii);
        let turn = if state.is_white_turn { "white" } else { "black "};
        println!("It is {turn}s turn to move");
        let moves = state.get_moves(&move_set);
        if moves.is_empty() {
            println!("Game Over");
            break
        }

        let Some(mv) = read_valid_move(&moves) else { break };
        state = mv.result;

    }
}
