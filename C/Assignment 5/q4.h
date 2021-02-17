// This module provides a function for analyzing a game of Connect
// (e.g., Connect 4)

// SEASHELL_READONLY

// See the assignment for more details

extern const int TIE;
extern const int INVALID_GAME;
extern const int MAX_BOARD_SIZE; // will not exceed 1024

// connect_analysis(moves, width, height, length) analyzes a game of connect
//   on a board of size (width x height) with the given [column] moves
//   and the player must get length in-a-row to win
// notes: returns TIE if the result is a tie after (width x height) moves
//        returns INVALID_GAME if a column overflows or an invalid
//          column (outside the range 0..width-1) occurs in moves before the
//          game is won (elements of moves are ignored after a win)
//        otherwise, returns the winning move #:
//          +x if player #1 wins on move #x
//          -x if player #2 wins on move #x
//        move #1 is made by player #1 and is stored in moves[0]
// requires: moves is an array of length (width x height) [not asserted]
//           2 < width, height
//           width x height <= MAX_BOARD_SIZE
//           2 < length <= max(width, height)
int connect_analysis(const int moves[], int width, int height, int length);