//! Known limitations:
//! - no support for en-passant
//! - no support for pawn promotion
//! - no support for 3-fold repetition
//! - no support for 50-draw
//! - no support for castling

pub mod chess;

/// Extra functions used mainly by tests.
pub mod util {
    use crate::chess::{for_each_piece, ChessState};

    /// Prints a 8x8 board as if it was a chessboard to stdout.
    pub fn print_ascii_board(ascii: &[u8]) {
        println!("  ABCDEFGH\n  --------");
        let mut i = 8;
        for chunk in ascii.chunks(8) {
            println!("{i}|{}", std::str::from_utf8(chunk).unwrap_or_default());
            i-=1;
        }
        println!("  --------\n  ABCDEFGH");
    }

    /// Fills positions of pieces represented through a bitboard to a 8x8 char board.
    pub fn fill_pieces(into: &mut [u8], ascii: u8, positions: u64) {
        for_each_piece(positions, |i|into[63 - i] = ascii);
    }

    /// Creates a chess state from ascii (with it being blacks turn).
    pub fn ascii_to_state(ascii: &[u8]) -> ChessState {
        let mut state = ChessState::default();
        for i in 0..64 {
            let place = 1 << i;
            match ascii[63 - i] as char {
                'K' => state.white.king += place,
                'Q' => state.white.queens += place,
                'P' => state.white.pawns += place,
                'N' => state.white.knights += place,
                'B' => state.white.bishops += place,
                'R' => state.white.rooks += place,
                'k' => state.black.king += place,
                'q' => state.black.queens += place,
                'p' => state.black.pawns += place,
                'n' => state.black.knights += place,
                'b' => state.black.bishops += place,
                'r' => state.black.rooks += place,
                ' ' => {},
                c => println!("unknown character: {c}"),
            }
        }
        return state;
    }

    /// Converts a given chess state to a 8x8 char board.
    pub fn state_to_ascii(state: &ChessState) -> [u8; 64] {
        let mut ascii = [' ' as u8; 64];

        fill_pieces(&mut ascii, 'K' as u8, state.white.king);
        fill_pieces(&mut ascii, 'Q' as u8, state.white.queens);
        fill_pieces(&mut ascii, 'P' as u8, state.white.pawns);
        fill_pieces(&mut ascii, 'N' as u8, state.white.knights);
        fill_pieces(&mut ascii, 'B' as u8, state.white.bishops);
        fill_pieces(&mut ascii, 'R' as u8, state.white.rooks);
        fill_pieces(&mut ascii, 'k' as u8, state.black.king);
        fill_pieces(&mut ascii, 'q' as u8, state.black.queens);
        fill_pieces(&mut ascii, 'p' as u8, state.black.pawns);
        fill_pieces(&mut ascii, 'n' as u8, state.black.knights);
        fill_pieces(&mut ascii, 'b' as u8, state.black.bishops);
        fill_pieces(&mut ascii, 'r' as u8, state.black.rooks);

        return ascii;
    }
}


#[cfg(test)]
mod tests {
    use crate::chess::{ChessState, parse_coordinate, MoveSet};

    /// Only used for moving pieces during tests.
    fn run_move(state: &mut ChessState, move_set: &MoveSet, mov: &str) {
        let from = parse_coordinate(&mov[0..2]).unwrap() as u8;
        let to = parse_coordinate(&mov[2..4]).unwrap() as u8;
        let moves = state.get_moves(move_set);
        for mov in moves {
            if mov.from == from && mov.to == to {
                *state = mov.result;
            }
        }
    }

    #[test]
    fn fools_mate() {
        let move_set = MoveSet::new();
        let mut state = ChessState::standard();
        run_move(&mut state, &move_set, "f2f3");
        run_move(&mut state, &move_set, "e7e5");
        run_move(&mut state, &move_set, "g2g4");
        run_move(&mut state, &move_set, "d8h4");
        let moves = state.get_moves(&move_set);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn fools_mate_mistake() {
        let move_set = MoveSet::new();
        let mut state = ChessState::standard();
        run_move(&mut state, &move_set, "f2f3");
        run_move(&mut state, &move_set, "e7e5");
        run_move(&mut state, &move_set, "a2a3");
        run_move(&mut state, &move_set, "d8h4");
        let moves = state.get_moves(&move_set);
        assert_eq!(moves.len(), 1);
    }

    #[test]
    fn starting_position() {
        let move_set = MoveSet::new();
        let state = ChessState::standard();
        let moves = state.get_moves(&move_set);
        assert_eq!(moves.len(), 20);
    }

    #[test]
    fn starting_position_black() {
        let move_set = MoveSet::new();
        let mut state = ChessState::standard();
        run_move(&mut state, &move_set, "e4e5");
        let moves = state.get_moves(&move_set);
        assert_eq!(moves.len(), 20);
    }

    #[test]
    fn serialize_deserialize() {
        let mut state = ChessState::standard();
        state.move_piece(8, 16);
        state.move_piece(63, 25);
        state.move_piece(15, 24);
        state.move_piece(63-8, 63-16);
        state.move_piece(3, 3+8*4);
        state.move_piece(8*6, 8*5);
        state.is_white_turn = false;

        let ascii = super::util::state_to_ascii(&state);

        let new_state = super::util::ascii_to_state(&ascii);
        let new_ascii = super::util::state_to_ascii(&new_state);

        assert_eq!(ascii, new_ascii);
        assert_eq!(state, new_state);

    }
}
