
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
// Improed by Daniel Infuehr.
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

struct MoveSet {
    knights: [u64; 64],
    sliding: [[SMask; 4]; 64],
    kings: [u64; 64]
}

impl MoveSet {
    fn new() -> MoveSet {
        let mut result = MoveSet {
            knights: [0; 64],
            sliding: [[Default::default(); 4]; 64],
            kings: [0; 64],
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
        }
        return result;
    }
}

#[derive(Default, Debug, Clone, Copy)]
struct ChessTeam {
    king: u64,
    queens: u64,
    pawns: u64,
    knights: u64,
    bishops: u64,
    rooks: u64,
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

#[derive(Debug)]
struct Moves {
    result: ChessState,
    from: u8,
    to: u8
}

fn for_each_piece<F: FnMut(usize) -> ()>(mut positions: u64, mut f: F) {
    for i in 0..64 {
        if positions & 1 == 1 {
            f(i);
        }
        positions >>= 1;
    }
}

/*fn insert_all_moves(into: &mut Vec<Moves>, state: &ChessState, from: usize, moves: u64) {
    for_each_piece(moves, |to| {
        let mut result = state.clone();
        result.move_piece(from as u8, to as u8);
        into.push(Moves {
            result,
            from: from as u8,
            to: to as u8,
        });
    })
}*/

fn fill_pieces(into: &mut [u8], ascii: u8, positions: u64) {
    for_each_piece(positions, |i|into[63 - i] = ascii);
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

fn get_pawns_moves<F: MovesHandler>(mut moves_handler: F, is_white_turn: bool, positions: u64, friendly_occupied: u64, occupied: u64) -> F {
    for_each_piece(positions, |i|{
        // TODO: handle en-passant.

        let capture_moves = occupied & if is_white_turn { 5 << (i + 7) } else { 5 << (i - 7) };
        let piece_moves = if is_white_turn { 1 << (i + 8) } else { 1 << (i - 8) };
        moves_handler(i, (piece_moves | capture_moves) & !friendly_occupied);
    });
    moves_handler
}

fn get_all_moves<F: MovesHandler>(mut moves_handler: F, is_white_turn: bool, move_set: &MoveSet, friends: &ChessTeam, friendly_occupied: u64, occupied: u64) -> F {
    moves_handler = get_knight_moves(moves_handler, move_set, friends.knights, friendly_occupied);
    moves_handler = get_rook_moves(moves_handler, move_set, friends.rooks,   friendly_occupied, occupied);
    moves_handler = get_bishops_moves(moves_handler, move_set, friends.bishops, friendly_occupied, occupied);
    moves_handler = get_queens_moves(moves_handler, move_set, friends.queens,  friendly_occupied, occupied);
    moves_handler = get_king_moves(moves_handler, move_set, friends.king,    friendly_occupied);
    moves_handler = get_pawns_moves(moves_handler, is_white_turn, friends.pawns, friendly_occupied, occupied);
    moves_handler
}

#[derive(Default, Debug, Clone, Copy)]
struct ChessState {
    black: ChessTeam,
    white: ChessTeam,
    is_white_turn: bool,
}

impl ChessState {
    fn from_ascii(ascii: &[u8]) -> ChessState {
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
    fn to_ascii(self: &ChessState) -> [u8; 64] {
        let mut ascii = [' ' as u8; 64];

        fill_pieces(&mut ascii, 'K' as u8, self.white.king);
        fill_pieces(&mut ascii, 'Q' as u8, self.white.queens);
        fill_pieces(&mut ascii, 'P' as u8, self.white.pawns);
        fill_pieces(&mut ascii, 'N' as u8, self.white.knights);
        fill_pieces(&mut ascii, 'B' as u8, self.white.bishops);
        fill_pieces(&mut ascii, 'R' as u8, self.white.rooks);
        fill_pieces(&mut ascii, 'k' as u8, self.black.king);
        fill_pieces(&mut ascii, 'q' as u8, self.black.queens);
        fill_pieces(&mut ascii, 'p' as u8, self.black.pawns);
        fill_pieces(&mut ascii, 'n' as u8, self.black.knights);
        fill_pieces(&mut ascii, 'b' as u8, self.black.bishops);
        fill_pieces(&mut ascii, 'r' as u8, self.black.rooks);

        return ascii;
    }
    fn standard() -> ChessState {
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
    fn get_moves(self: &ChessState, move_set: &MoveSet) -> Vec<Moves> {
        let is_white_turn = self.is_white_turn;

        let friends = if is_white_turn {
            &self.white
        } else {
            &self.black
        };
        let enemies = if is_white_turn {
            &self.black
        } else {
            &self.white
        };
        let friendly_occupied = friends.get_occupancy();
        let enemy_occupied = enemies.get_occupancy();
        let occupied = friendly_occupied | enemy_occupied;

        let mut total_moves = vec![];
        let moves_handler = |from: usize, moves: u64| {
            for_each_piece(moves, |to| {
                let mut result = self.clone();
                result.move_piece(from as u8, to as u8);
                total_moves.push(Moves {
                    result,
                    from: from as u8,
                    to: to as u8,
                });
            })
        };

        _ = get_all_moves(moves_handler, is_white_turn, move_set, friends, friendly_occupied, occupied);

        return total_moves;
    }
    fn move_piece(self: &mut ChessState, from: u8, to: u8) {
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

fn print_ascii_board(ascii: &[u8]) {
    println!("  ABCDEFGH\n  --------");
    let mut i = 8;
    for chunk in ascii.chunks(8) {
        println!("{i}|{}", std::str::from_utf8(chunk).unwrap_or_default());
        i-=1;
    }
    println!("  --------\n  ABCDEFGH");
}

fn main() {
    let move_set = MoveSet::new();
    let mut state = ChessState::standard();

    state.move_piece(8, 16);
    state.move_piece(63, 25);
    state.move_piece(15, 24);
    //state.move_piece(7, 24);
    state.move_piece(63-8, 63-16);

    //state.white.queens |= 0x00_00_00_10_00_00_00_00;
    //state.white.bishops |= 0x00_00_00_10_00_00_00_00;

    let ascii = state.to_ascii();
    print_ascii_board(&ascii);

    let other_board = ChessState::from_ascii(&ascii);
    let mut other_ascii = other_board.to_ascii();

    /*for i in 0..4 {
        fill_pieces(&mut other_ascii, 'X' as u8,
            line_attacks(other_board.white.get_occupancy() | other_board.black.get_occupancy(),
                         &move_set.sliding[28][i]));
    }*/

    let moves = state.get_moves(&move_set);
    println!("Moves: {:#?}", moves);
    for m in moves {
        fill_pieces(&mut other_ascii, 'X' as u8, 1 << m.to);
    }

    //fill_pieces(&mut other_ascii, '.' as u8, state.white.get_occupancy() | state.black.get_occupancy());

    print_ascii_board(&other_ascii);

}
