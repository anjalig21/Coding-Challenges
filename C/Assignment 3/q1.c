#include "cs136.h"

// IMPORTANT: review draw.txt to see the input format

// draw_box(width, height, c) draws (prints) a solid box with the given
// positive dimensions, and the non-whitespace printable ASCII character c.
// requires: width > 0
//           height > 0
//           c >= '!' && c <= '~'
// effects: produces output
void draw_box(int width, int height, char c) {
  assert(width > 0);
  assert(height > 0);
  assert(c >= '!' && c <= '~');
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
      printf("%c", c);
    }
    printf("\n");
  }
}

// draw_xbox(size) draws (prints) a fancier box 
// with an "X" inside of the box.
// requires: size > 0
// effects: produces output
void draw_xbox(int size) {
  assert(size > 0);
  int dimension = 2 * size + 1;
  int counter = 0;
  int counter_bottom = 0;
  for (int i = 0; i < dimension; i++) {
    if ((counter == 0) ||  (counter == dimension - 1)) {
      printf("+");
    }
    else {
      printf("-");
    }
    counter++;
  }
  printf("\n");
  int counter_slash = -1;
  for (int j = 0; j < dimension - 2; j++) {
    printf("|");
    counter_slash++;
    for (int k = 0; k < dimension - 2; k++) {
      if ((j == (dimension - 2) / 2) && (k == (dimension - 2) / 2)) {
        printf("X");
      }
      else if (counter_slash == k) {
        printf("\\");
      }
      else if (k == (dimension - 3) - counter_slash) {
        printf("/");
      }
      else {
        printf(" ");
      }
    }
    printf("|\n");
  }
  for (int p = 0; p < dimension; p++) {
    if ((counter_bottom == 0) || (counter_bottom == dimension - 1)) {
      printf("+");
    }
    else {
      printf("-");
    }
    counter_bottom++;
  }
  printf("\n");
}

///////////////////////////////////////////////////////
// You do not need to modify the rest of the program //
///////////////////////////////////////////////////////

int main(void) {
  int BOX = lookup_symbol("box");
  int XBOX = lookup_symbol("xbox");
  // if there is any invalid input, the program simply stops
  //   with no error message
  while (1) {
    int cmd = read_symbol();
    if (cmd == BOX) {
      int width = read_int();
      int height = read_int();
      int c = read_char(true);
      if (height == READ_INT_FAIL || c == READ_CHAR_FAIL) {
        break;
      }
      draw_box(width, height, c);
    } else if (cmd == XBOX) {
      int size = read_int();
      if (size == READ_INT_FAIL) {
        break;
      }
      draw_xbox(size);
    } else {
      break;
    }
    printf("\n");
  }
}