#include "cs136.h"

///////////////////////////////////////////////////////////////////////////// 
// Do not modify this function

// You MUST use this function to read in an int from input
// (it uses a method from section 05 -- you don't have to understand it yet)

// read_int_or_exit() reads in an integer from input, or exits (terminates)
//   the program if an int cannot be successfully read in
// note: terminates the program quietly with no output displayed
// effects: reads input
//          may terminate program (a rare side effect we don't normally
//                                 worry about in this course)
int read_int_or_exit(void) {
  int n = 0;
  if (scanf("%d", &n) != 1) {
    exit(EXIT_SUCCESS);
  }
  return n;
}
/////////////////////////////////////////////////////////////////////////////
// This program is designed to behave like handheld calculators.
// effects: reads input

// calculator(num, operator, result) is an interactive calculator 
// that behaves similarly to many handheld calculators. With operator
// and numbers inputted, it applies the operators to the numbers.
// effects: produces output
int calculator(int num, int operator, int result) {
  if (operator == lookup_symbol("add")) {
    if (((result > 0 && num > 0) && (result > INT_MAX - num)) || 
        ((result < 0 && num < 0) && (result < INT_MIN + num))) {
      printf("OVERFLOW\n");
      exit(EXIT_SUCCESS);
    } 
    else {
      result = result + num;
      printf("%d\n", result);
      return result;
    }
  } 
  else if (operator == lookup_symbol("sub")) {
    if (((result > 0 && num < 0) && (result > INT_MAX + num)) || 
        ((result < 0 && num > 0) && (result < INT_MIN + num))) {
      printf("OVERFLOW\n");
      exit(EXIT_SUCCESS);
    } 
    else {
      result = result - num;
      printf("%d\n", result);
      return result;
    }
  } 
  else if (operator == lookup_symbol("mult")) {
    if (((result > 0 && num < 0) && (result > (INT_MIN / num))) ||
        ((result < 0 && num > 0) && (result < (INT_MIN / num))) ||
        ((result > 0 && num > 0) && (result > (INT_MAX / num))) ||
        ((result < 0 && num < 0) && (result < (INT_MAX / num)))) {
      printf("OVERFLOW\n");
      exit(EXIT_SUCCESS);
    } 
    else {
      result = result * num;
      printf("%d\n", result);
      return result;
    }
  } 
  else if (operator == lookup_symbol("div")) {
    if (num == 0) {
      printf("DIVIDE BY ZERO\n");
      exit(EXIT_SUCCESS);
    } 
    else {
      result = result / num;
      printf("%d\n", result);
      return result;
    }
  } 
  else {
    exit(EXIT_SUCCESS);
  }
}

int main(void) {
  int result = 0;
  while (true) {
    int operator = read_symbol();
    int num = read_int_or_exit();
    result = calculator(num, operator, result);
  }
}