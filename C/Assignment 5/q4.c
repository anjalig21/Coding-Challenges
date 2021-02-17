#include "trace-2d.h"
#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/////////////////////////////////////////////////////////////////////////////
// DO NOT MODIFY THESE CONSTANTS
const int INVALID_GAME = -1;
const int TIE = 1;
const int MAX_BOARD_SIZE = 1024;
const int EMPTY = 0;
const int PLAYING = 0;
const int P1 = 2;
const int P2 = -2;
/////////////////////////////////////////////////////////////////////////////

bool valid_move(int val, int compare) {
  return (val < compare && val >= 0);
}

bool analyze_row(int board[], int width, int height, int row, 
               int column, int move_right, int move_up, int player, 
               int length) {
  bool repeat = true;
  int max_row = 1;
  int curr_col = column;
  int curr_row = row;
  for (int i = 0; i < length; i++) {
    for (int j = 0; j < length && repeat; j++) {
      curr_col += move_right;
      curr_row += move_up;
      if (valid_move(curr_col, width) && valid_move(curr_row, height) &&
          board[(curr_row * width) + curr_col] == player) {
        max_row++;
      } 
      else {
        repeat = false;
      }
    }
    curr_row = row;
    curr_col = column;
    move_right *= -1;
    move_up *= -1;
  }
  if (max_row < length) {
    return false;
  }
  else {
    return true;
  }
}

bool check_winner(int board[], int width, int height, int length, int row, 
                  int column, int player) {
  return (analyze_row(board, width, height, row, column, 1, -1, 
                       player, length) || 
          analyze_row(board, width, height, row, column, 1, 0, 
                       player, length) ||
          analyze_row(board, width, height, row, column, 1, 1, 
                       player, length) ||
          analyze_row(board, width, height, row, column, 0, 1,
                       player, length));
}

int mimic_board(int board[], int column, int width, int height, 
                int length, int player) {
  int pos = 0;
  if (valid_move(column, width)) {
    for (int i = (height - 1); i >= 0; i--) {
      pos = i * width + column;
      if (board[pos] == EMPTY) {
        board[pos] = player;
        if (check_winner(board, width, height, length, i, column, 
                         player)) {
          return player;
        } 
        else {
          return PLAYING;
        }
      }
    }
  }
  return INVALID_GAME;
}

int connect_analysis(const int moves[], int width, int height, int length) {
  assert(width > 2);
  assert(height > 2);
  assert(width * height <= MAX_BOARD_SIZE);
  assert(length > 2);
  if (width > height) {
    assert(2 < length && length <= width);
  }
  else {
    assert(2 < length && length <= height);
  }
  int player_move = P1;
  int board[1024] = {EMPTY};
  for (int i = 0; i < (width * (height - 1)); i++) {
    int curr_board =  mimic_board(board, moves[i], width, 
                                  height, length, player_move);
    if (curr_board == INVALID_GAME) {
      return INVALID_GAME;
    } 
    else if (curr_board != PLAYING) {
      return ((player_move * (i + 1)) / P1);
    }
    else {
      player_move *= -1;
    }
  }
  return TIE;
}