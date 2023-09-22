//! The internals of the chess engine if one wishes to dig deeper.

/// Possible errors that can happen when parsing a coordinate.
#[derive(Debug)]
pub enum CoordinateError {
    LengthNotTwo,
    BadRank(char),
    BadFile(char),
}

/// Parses coordinates in the form of "a6" or "B4".
pub fn parse_coordinate(coord: &str) -> Result<u32, CoordinateError> {
    let mut chars = coord.chars();
    let Some(first_char) = chars.next() else {
        return Err(CoordinateError::LengthNotTwo)
    };
    let Some(second_char) = chars.next() else {
        return Err(CoordinateError::LengthNotTwo)
    };

    if let Some(_) = chars.next() {
        return Err(CoordinateError::LengthNotTwo)
    }

    let square;

    if second_char >= '1' && second_char <= '8' {
        square = ((second_char as u32 - '1' as u32 ) << 3) + 7;
    } else {
        return Err(CoordinateError::BadRank(second_char))
    }

    if first_char >= 'A' && first_char <= 'H' {
        return Ok(square  + 'A' as u32 - first_char as u32);
    } else if first_char >= 'a' && first_char <= 'h' {
        return Ok(square  + 'a' as u32 - first_char as u32);
    } else {
        return Err(CoordinateError::BadFile(first_char))
    };

}

#[derive(Copy, Clone, Default)]
struct SMask {
    lower: u64,
    upper: u64,
    line: u64,
}

fn upper_ray(position: i32, ray: u64) -> u64 {
    ray & 0u64.wrapping_sub(2u64.wrapping_shl(position as u32))
}

fn lower_ray(position: i32, ray: u64) -> u64 {
    ray & ((1 << position) - 1)
}

impl SMask {
    fn new(position: i32, mask: u64) -> SMask {
        let upper = upper_ray(position, mask);
        let lower = lower_ray(position, mask);
        let line = upper | lower;
        return SMask {
            upper,
            lower,
            line,
        }
    }
}

// Obstruction Difference, invented by Michael Hoffman.
// Improved by Daniel Infuehr.
fn line_attacks(occupied: u64, mask: &SMask) -> u64 {
    let lower = mask.lower & occupied;
    let upper = mask.upper & occupied;
    let ms1b = 0x8000000000000000 >> (lower | 1).leading_zeros();
    let odiff = upper ^ upper.wrapping_sub(ms1b);
    return mask.line & odiff;
}

fn diagonal_mask(position: i32) -> u64 {
    const MAIN_DIAGONAL: u64 = 0x8040201008040201;
    let diagonal = ((position & 7) - (position >> 3)) * 8;
    return if diagonal >= 0 { MAIN_DIAGONAL >> diagonal  }
                       else { MAIN_DIAGONAL << -diagonal }
}

fn anti_diagonal_mask(position: i32) -> u64 {
    const MAIN_DIAGONAL: u64 = 0x0102040810204080;
    let diagonal = (7 - (position & 7) - (position >> 3)) * 8;
    return if diagonal >= 0 { MAIN_DIAGONAL >> diagonal  }
                       else { MAIN_DIAGONAL << -diagonal }
}

/// A data structure used to speed up the finding of valid moves.
pub struct MoveSet {
    knights: [u64; 64],
    sliding: [[SMask; 4]; 64],
    kings: [u64; 64],
    white_pawns: [u64; 64],
    black_pawns: [u64; 64],
}

impl MoveSet {
    /// Creates and initializes a move-set. Usually only one MoveSet is
    /// required per program.
    pub fn new() -> MoveSet {
        let mut result = MoveSet {
            knights: [0; 64],
            sliding: [[Default::default(); 4]; 64],
            kings: [0; 64],
            white_pawns: [0; 64],
            black_pawns: [0; 64]
        };
        fn maybe_add_move(rank: i32, file: i32) -> u64 {
            if rank >= 8 || rank < 0 || file >= 8 || file < 0 {
                return 0
            }
            return 1 << (rank * 8) + file;
        }
        for i in 0 .. 64 {
            let mut v = 0;
            let rank = i >> 3;
            let file = i & 7;

            v |= maybe_add_move(rank + 2, file + 1);
            v |= maybe_add_move(rank + 2, file - 1);
            v |= maybe_add_move(rank + 1, file + 2);
            v |= maybe_add_move(rank - 1, file + 2);
            v |= maybe_add_move(rank - 2, file + 1);
            v |= maybe_add_move(rank - 2, file - 1);
            v |= maybe_add_move(rank + 1, file - 2);
            v |= maybe_add_move(rank - 1, file - 2);
            result.knights[i as usize] = v;

            let rank_mask = 0xFF << (i & 56);
            let file_mask = 0x0101010101010101 << (i & 7);
            let diag_mask = diagonal_mask(i);
            let anti_diag_mask = anti_diagonal_mask(i);

            result.sliding[i as usize][0] = SMask::new(i, rank_mask);
            result.sliding[i as usize][1] = SMask::new(i, file_mask);
            result.sliding[i as usize][2] = SMask::new(i, diag_mask);
            result.sliding[i as usize][3] = SMask::new(i, anti_diag_mask);

            let mut k = 0;
            k |= maybe_add_move(rank - 1, file - 1);
            k |= maybe_add_move(rank + 1, file - 1);
            k |= maybe_add_move(rank + 1, file + 1);
            k |= maybe_add_move(rank - 1, file + 1);
            k |= maybe_add_move(rank + 1, file);
            k |= maybe_add_move(rank - 1, file);
            k |= maybe_add_move(rank, file + 1);
            k |= maybe_add_move(rank, file - 1);
            result.kings[i as usize] = k;

            let mut wp = 0;
            wp |= maybe_add_move(rank + 1, file - 1);
            wp |= maybe_add_move(rank + 1, file + 1);
            result.white_pawns[i as usize] = wp;

            let mut bp = 0;
            bp |= maybe_add_move(rank - 1, file - 1);
            bp |= maybe_add_move(rank - 1, file + 1);
            result.black_pawns[i as usize] = bp;

        }
        return result;
    }
}

/// Stores all the pieces belonging to one team using bitboards.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct ChessTeam {
    /// Bitboard position of the king.
    pub king: u64,

    /// Bitboard position of the queens.
    pub queens: u64,

    /// Bitboard position of the pawns.
    pub pawns: u64,

    /// Bitboard position of the knights.
    pub knights: u64,

    /// Bitboard position of the bishops.
    pub bishops: u64,

    /// Bitboard position of the rooks.
    pub rooks: u64,
}

impl ChessTeam {
    fn clear_position(self: &mut ChessTeam, position: i32) {
        let mask = !(1 << position);
        self.bishops &= mask;
        self.knights &= mask;
        self.rooks   &= mask;
        self.pawns   &= mask;
        self.queens  &= mask;
        self.king    &= mask; // Makes no sense, but we like consistency.

    }
    fn get_occupancy(self: &ChessTeam) -> u64 {
        return self.king
            |  self.queens
            |  self.pawns
            |  self.knights
            |  self.bishops
            |  self.rooks
    }
}

/// A move which can be played including the resulting chess state.
#[derive(Debug)]
pub struct Move {
    /// The state that results from applying the move.
    pub result: ChessState,

    /// The piece making the move.
    pub from: u8,

    /// The destination square.
    pub to: u8
}

fn square_to_coordinate(square: u8) -> String {
    let rank = square >> 3;
    let file = square & 7;
    format!("{}{}", ('a' as u8 + rank) as char, file)
}

impl ToString for Move {
    fn to_string(&self) -> String {
        format!("{}{}", square_to_coordinate(self.from), square_to_coordinate(self.to))
    }
}

/// Runs a procedure for each piece inside of a bitboard.
pub fn for_each_piece<F: FnMut(usize) -> ()>(mut positions: u64, mut f: F) {
    for i in 0..64 {
        if positions & 1 == 1 {
            f(i);
        }
        positions >>= 1;
    }
}

trait MovesHandler: FnMut(usize, u64) -> () {}
impl<T: FnMut(usize, u64)> MovesHandler for T {}

fn get_knight_moves<F: MovesHandler>(mut moves_handler: F, move_set: &MoveSet, positions: u64, friendly_occupied: u64) -> F {
    for_each_piece(positions, |i| {
        moves_handler(i, move_set.knights[i] & !friendly_occupied);
    });
    moves_handler
}

fn get_rook_moves<F: MovesHandler>(mut moves_handler: F, move_set: &MoveSet, positions: u64, friendly_occupied: u64, occupied: u64) -> F {
    for_each_piece(positions, |i|{
        let a = line_attacks(occupied, &move_set.sliding[i][0]);
        let b = line_attacks(occupied, &move_set.sliding[i][1]);
        moves_handler(i, (a | b) & !friendly_occupied);
    });
    moves_handler
}

fn get_bishops_moves<F: MovesHandler>(mut moves_handler: F, move_set: &MoveSet, positions: u64, friendly_occupied: u64, occupied: u64) -> F{
    for_each_piece(positions, |i|{
        let a = line_attacks(occupied, &move_set.sliding[i][2]);
        let b = line_attacks(occupied, &move_set.sliding[i][3]);
        moves_handler(i, (a | b) & !friendly_occupied);
    });
    moves_handler
}

fn get_queens_moves<F: MovesHandler>(mut moves_handler: F, move_set: &MoveSet, positions: u64, friendly_occupied: u64, occupied: u64) -> F{
    for_each_piece(positions, |i|{
        let a = line_attacks(occupied, &move_set.sliding[i][0]);
        let b = line_attacks(occupied, &move_set.sliding[i][1]);
        let c = line_attacks(occupied, &move_set.sliding[i][2]);
        let d = line_attacks(occupied, &move_set.sliding[i][3]);
        moves_handler(i, (a | b | c | d) & !friendly_occupied);
    });
    moves_handler
}

fn get_king_moves<F: MovesHandler>(mut moves_handler: F, move_set: &MoveSet, positions: u64, friendly_occupied: u64) -> F {
    for_each_piece(positions, |i|{
        moves_handler(i, move_set.kings[i] & !friendly_occupied);
    });
    moves_handler
}

fn get_pawns_moves<F: MovesHandler>(mut moves_handler: F, move_set: &MoveSet, positions: u64, friendly_occupied: u64, occupied: u64, is_white_turn: bool) -> F {
    for_each_piece(positions, |i|{
        // TODO: handle en-passant.

        let capture_moves = occupied & if is_white_turn {
            move_set.white_pawns[i]
        } else {
            move_set.black_pawns[i]
        };
        let piece_moves = if is_white_turn {
            1 << (i + 8) | if (i >> 3) == 1 {1 << (i + 16)} else {0}
        } else {
            1 << (i - 8) | if (i >> 3) == 6 {1 << (i - 16)} else {0}
        };
        moves_handler(i, (piece_moves & !occupied) | (capture_moves & !friendly_occupied));
    });
    moves_handler
}

fn get_all_moves<F: MovesHandler>(mut moves_handler: F, is_white_turn: bool, move_set: &MoveSet, friends: &ChessTeam, friendly_occupied: u64, occupied: u64) -> F {
    moves_handler = get_knight_moves(moves_handler, move_set, friends.knights, friendly_occupied);
    moves_handler = get_rook_moves(moves_handler, move_set, friends.rooks,   friendly_occupied, occupied);
    moves_handler = get_bishops_moves(moves_handler, move_set, friends.bishops, friendly_occupied, occupied);
    moves_handler = get_queens_moves(moves_handler, move_set, friends.queens,  friendly_occupied, occupied);
    moves_handler = get_king_moves(moves_handler, move_set, friends.king,    friendly_occupied);
    moves_handler = get_pawns_moves(moves_handler, move_set, friends.pawns, friendly_occupied, occupied, is_white_turn);
    moves_handler
}

/// Contains the pieces of both teams and extra data about Chess.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct ChessState {
    /// Pieces of the black team.
    pub black: ChessTeam,

    /// Pieces of the white team.
    pub white: ChessTeam,

    /// Is it whites turn.
    pub is_white_turn: bool,
}

impl ChessState {
    /// Creates the state of a standard Chess game.
    pub fn standard() -> ChessState {
        ChessState {
            white: ChessTeam {
                king:    0x00_00_00_00_00_00_00_08,
                queens:  0x00_00_00_00_00_00_00_10,
                pawns:   0x00_00_00_00_00_00_FF_00,
                knights: 0x00_00_00_00_00_00_00_42,
                bishops: 0x00_00_00_00_00_00_00_24,
                rooks:   0x00_00_00_00_00_00_00_81,
            },
            black: ChessTeam {
                king:    0x08_00_00_00_00_00_00_00,
                queens:  0x10_00_00_00_00_00_00_00,
                pawns:   0x00_FF_00_00_00_00_00_00,
                knights: 0x42_00_00_00_00_00_00_00,
                bishops: 0x24_00_00_00_00_00_00_00,
                rooks:   0x81_00_00_00_00_00_00_00,
            },
            is_white_turn: true,
        }
    }
    fn get_attacker(&self) -> &ChessTeam {
        if self.is_white_turn {
            &self.white
        } else {
            &self.black
        }
    }
    fn get_defender(&self) -> &ChessTeam {
        if self.is_white_turn {
            &self.black
        } else {
            &self.white
        }
    }

    /// Finds all valid moves for the team whose turn it is.
    /// Returns an empty list if none are possible (check-mate or remi).
    pub fn get_moves(self: &ChessState, move_set: &MoveSet) -> Vec<Move> {
        let is_white_turn = self.is_white_turn;

        // TODO: Factor into ChessState
        let friends = self.get_attacker();
        let enemies = self.get_defender();

        let friendly_occupied = friends.get_occupancy();
        let enemy_occupied = enemies.get_occupancy();
        let occupied = friendly_occupied | enemy_occupied;

        let mut total_moves = vec![];
        let moves_handler = |from: usize, moves: u64| {
            for_each_piece(moves, |to| {
                let mut result = self.clone();
                result.move_piece(from as u8, to as u8);
                let is_white_turn = result.is_white_turn;

                let attacker = result.get_attacker();
                let defender = result.get_defender();
                let attacker_occupied = attacker.get_occupancy();
                let defender_occupied = defender.get_occupancy();
                let occupied = attacker_occupied | defender_occupied;

                let mut in_check = 0u64;
                let king_checker = |_from: usize, moves: u64| {
                    in_check |= moves & defender.king;
                };
                _ = get_all_moves(king_checker, is_white_turn, move_set, attacker, attacker_occupied, occupied);
                if in_check == 0 {
                    total_moves.push(Move {
                        result,
                        from: from as u8,
                        to: to as u8,
                    });
                }
            })
        };

        _ = get_all_moves(moves_handler, is_white_turn, move_set, friends, friendly_occupied, occupied);

        return total_moves;
    }

    /// Move a piece from one square to another.
    /// Capturing if a piece is already in the destination.
    /// This procedure also changes whose turn it is.
    pub fn move_piece(self: &mut ChessState, from: u8, to: u8) {
        // If we know the piece ahead of time (which we know often).
        // We could optimize this further. Perhaps in another method.

        let team = if self.is_white_turn {
            &mut self.white
        } else {
            &mut self.black
        };
        team.pawns |= ((team.pawns >> from) & 1) << to;
        team.king |= ((team.king >> from) & 1) << to;
        team.queens |= ((team.queens >> from) & 1) << to;
        team.rooks |= ((team.rooks >> from) & 1) << to;
        team.bishops |= ((team.bishops >> from) & 1) << to;
        team.knights |= ((team.knights >> from) & 1) << to;
        team.clear_position(from as i32);

        let enemies = if self.is_white_turn {
            &mut self.black
        } else {
            &mut self.white
        };
        enemies.clear_position(to as i32);

        self.is_white_turn = !self.is_white_turn;
    }
}


