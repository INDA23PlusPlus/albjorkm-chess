# About
scrappy_chess is an implementation of the classical
game of Chess. Albeit with many defects such as not supporting
pawn promotion, en-passant or castling. Internally
it uses bitboards. Since they can be annoying to work with,
scrappy_chess has a public API which does not require any usage
of bitboard. As well as a `scrappy_chess::chess` module which
exposes the internals of the engine a bit more.
This project has no dependencies other than the rust standard
library. All of the important procedures should be documented.
Simply run `cargo doc` to generate the docs.

# How-to-use
See the `src/main.rs` for a concrete example. The ChessGame
struct should be the only thing that you actually need.

# Changes
If you believe the code could be changed to fit your implementation
better. Please contact me and I can make the change for you.
For example, if you would rather have the iterator return a square index
instead of both the rank and file.

