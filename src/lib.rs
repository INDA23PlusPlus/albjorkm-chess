//! Known limitations:
//! - no support for en-passant
//! - no support for pawn promotion
//! - no support for 3-fold repetition
//! - no support for 50-draw
//! - no support for castling

use self::chess::parse_coordinate;

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

/// The recommended way to use the Chess engine.
pub struct ChessGame {
    move_set: chess::MoveSet,
    state: chess::ChessState,
    moves: Vec<chess::Move>,
}

/// Iterates over all pieces on a Chess board.
pub struct PieceIterator<'a> {
    state: &'a chess::ChessState,
    piece: char,
    index: u8,
}

impl<'a> Iterator for PieceIterator<'a> {
    type Item = (char, u32, u32);
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.piece == '#' {
                return None;
            }

            let (next, pieces) = match self.piece {
                'K' => ('Q', self.state.white.king),
                'Q' => ('P', self.state.white.queens),
                'P' => ('N', self.state.white.pawns),
                'N' => ('B', self.state.white.knights),
                'B' => ('R', self.state.white.bishops),
                'R' => ('k', self.state.white.rooks),
                'k' => ('q', self.state.black.king),
                'q' => ('p', self.state.black.queens),
                'p' => ('n', self.state.black.pawns),
                'n' => ('b', self.state.black.knights),
                'b' => ('r', self.state.black.bishops),
                'r' => ('#', self.state.black.rooks),
                 _  => ('#', 0)
            };

            if pieces == 0 {
                self.index = 0;
                self.piece = next;
                continue
            }

            while self.index < 64 {
                let index = self.index;
                let has_piece = (pieces >> index) & 1;
                self.index += 1;
                if has_piece == 1 {
                    let rank = (index >> 3) as u32;
                    let file = (index & 7) as u32;
                    return Some((self.piece, rank, file))
                }
            }

            self.index = 0;
            self.piece = next;
        }


    }
}

// A reason explaining why a move was not allowed.
#[derive(Debug)]
pub enum MoveError {
    /// Provided coordinate was invalid.
    CoordinateError(chess::CoordinateError),

    /// Input should be (at least) 4 characters.
    TooShort,

    /// Not allowed by the rules of chess.
    InvalidMove
}

impl ChessGame {
    /// Creates a new Chess game.
    pub fn new() -> ChessGame {
        let move_set = chess::MoveSet::new();
        let state = chess::ChessState::standard();
        let moves = state.get_moves(&move_set);
        ChessGame {
            move_set,
            state,
            moves,
        }
    }
    /// Creates an iterator overall pieces.
    pub fn pieces(&self) -> PieceIterator {
        PieceIterator {
            state: &self.state,
            piece: 'K',
            index: 0,
        }
    }

    /// Returns true if it is whites turn.
    pub fn is_white_turn(&self) -> bool {
        return self.state.is_white_turn
    }

    /// Returns true if the game is over.
    pub fn is_over(&self) -> bool {
        return self.moves.is_empty()
    }

    pub fn moves(&self) -> Vec<String> {
        self.moves.iter().map(|e|e.to_string()).collect()
    }

    pub fn play_move(&mut self, move_str: &str) -> Result<(), MoveError> {
        if move_str.len() < 4 {
            return Err(MoveError::TooShort)
        }
        let from = &move_str[0..2];
        let to = &move_str[2..4];

        let square = match parse_coordinate(from) {
            Err(e) => { return Err(MoveError::CoordinateError(e))},
            Ok(square) => square
        } as u8;
        let square_dest = match parse_coordinate(to) {
            Err(e) => { return Err(MoveError::CoordinateError(e))},
            Ok(square) => square
        } as u8;

        for mov in &self.moves {
            if square == mov.from && square_dest == mov.to {
                self.state = mov.result;
                self.moves = self.state.get_moves(&self.move_set);
                return Ok(());
            }
        }
        Err(MoveError::InvalidMove)
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

    #[test]
    fn test_iterator() {
        let game = super::ChessGame::new();
        let state = ChessState::standard();
        let ascii = super::util::state_to_ascii(&state);

        let mut game_ascii = [' ' as u8; 64];
        for (c, rank, file) in game.pieces() {
            game_ascii[(63 - rank * 8 - file) as usize] = c as u8;
        }

        assert_eq!(game_ascii, ascii);
    }

}
