#include "cs136.h"

// print_grade(num, denom) prints the value of the integer division
// (num / denom) as a percentage with exactly one digit after 
// the decimal point.
// requires: num > 0
//          denom > 0
// effects: produces output 
void print_grade(int num, int denom) {
  assert(num > 0);
  assert(denom > 0);
  num *= 100;
  int quotient = (num / denom);
  int decimals = ((10 * (num % denom)) / denom);
  printf("%d.%d%%", quotient, decimals);
}

// decimal_printer(amount) prints the 2 decimal places needed 
// for "money" or "dollars" notation.
// effects: produces output
void decimal_printer(int amount, int decimal) {
  if (amount % 100 == 0) {
    printf(".%d%d", 0, 0);
  }
  else {
    printf(".%02d", decimal);
  }
}
    
// money_printer(amount) prints the commas needed for "money" 
// or "dollars" notation.
// effects: produces output
void money_printer(int amount) {
  if (amount < 1000) {
    printf("%d", amount);
  }
  else {
    money_printer(amount / 1000);
    printf(",%03d", (amount % 1000));
  }
}

// print_money(amount) prints amount using a "money" or "dollars" notation.
// requires: amount >= -999999999
//           amount <= 999999999
// effects: produces output
void print_money(int amount) {
  assert(amount >= -999999999);
  assert(amount <= 999999999);
  if (amount < 0) {
    printf("-$");
  }
  else {
    printf("$");
  }
  int decimal = abs(amount) % 100;
  money_printer(abs(amount) / 100);
  decimal_printer(amount, decimal);
}