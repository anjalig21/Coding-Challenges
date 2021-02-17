#include "cs136.h"

// check_char() does the opposite of what we did 
// in Assignment 2, question 2b.
// effects: reads input
// effects: produces output
void check_char(void) {
  int counter = 0;
  char n = read_char(false);
  while (n != READ_CHAR_FAIL) {
    if (n == '@') {
      counter++;
      n = read_char(false);
      while (n == '@') {
        counter++;
        n = read_char(false);
      }
      printf("%d ", counter);
      counter = 0;
    }
    else if (n == ' ') {
      counter--;
      n = read_char(false);
      while (n == ' ') {
        counter--;
        n = read_char(false);
      }
      printf("%d ", counter);
      counter = 0;
    }
    else {
      printf("0\n");
      n = read_char(false);
    }
  }
}
    
int main(void) {
  check_char();
}
