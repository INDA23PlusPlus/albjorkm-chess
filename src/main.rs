// Use this file as inspiration for how to use the library.

use scrappy_chess::util::print_ascii_board;
use scrappy_chess::ChessGame;

fn play_valid_move(game: &mut ChessGame) {
    loop {
        let stdin = std::io::stdin();
        let mut input = String::new();
        if let Err(e) = stdin.read_line(&mut input) {
            println!("Could not read input: {e}");
            return
        }
        input = input.replace(&['\n', '\r', ' '], "");
        if input == "help" {
            println!("Valid moves: {:#?}", game.moves());
            continue
        }

        if let Err(e) = game.play_move(&input) {
            println!("Bad move: {:#?}", e);
            continue
        }
        break
    }
}

fn main() {
    let mut game = ChessGame::new();

    loop {
        println!("{}", "".repeat(32));
        let mut board = [' ' as u8; 64];
        for (c, rank, file) in game.pieces() {
            board[(63 - rank * 8 - file) as usize] = c as u8;
        }
        print_ascii_board(&board);
        let turn = if game.is_white_turn() { "white" } else { "black" };
        println!("It is {turn}s turn to move");
        if game.is_over() {
            println!("Game Over");
            break
        }

        play_valid_move(&mut game)
    }
}
