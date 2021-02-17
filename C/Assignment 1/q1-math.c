#include "cs136.h"

//
// A
//

// max(a, b) consumes two integers and produces
// the max of the two numbers.
int max(int a, int b) {
  if (a >= b) {
    return a;
  }
  else {
    return b;
  }
}


//
// B
//

// my_expt(base, counter, result) produces the result
// when the power inputted is applied to the base.
int my_expt(int base, int counter, int result) {
  if (counter == 0) {
    return result;
  }
  else {
    result = result * base;
    return my_expt(base, counter - 1, result);
  }
}

// expt(b, n) consumes a base and power and produces the result
// when the power is applied to the base.
int expt(int b, int n) {
  if (n == 0) {
    return 1;
  }
  else {
    return my_expt(b, n, 1);
  }
}
  

//
// C
//

// find_sqrt(n, counter) produces the square-root of n
// requires: n >= 0
int find_sqrt(int n, int counter) {
  if ((n >= expt(counter, 2)) && (n < expt((counter + 1), 2))) {
    return counter;
  }
  else {
    return find_sqrt(n, counter + 1);
  }
}

// sqrt_int(n) produces the square-root of n
// requires: n >= 0
int sqrt_int(int n) {
  assert(n >= 0);
  return find_sqrt(n, 1);
}


//
// D
//

// log_2_calc(n, counter) produces the result of log base 2 of n.
// requires: n > 0
int log_2_calc(int n, int counter) {
  if ((n > expt(2, counter - 1)) && (n <= expt(2, counter))) {
    return counter;
  }
  else {
    return log_2_calc(n, counter + 1);
  }
}

// log_2_(n) produces the result of log base 2 of n.
// requires: n > 0
int log_2(int n) {
  assert(n > 0);
  return log_2_calc(n, 1);
}


int main(void) {
  assert(max(4, 4) == 4);
  assert(expt(2, 2) == 4);
  assert(expt(0, 0) == 1);
  assert(sqrt_int(16) == 4);
  assert(log_2(16) == 4);
  
  // ADD YOUR OWN TESTS BELOW:
  assert(max(1, 0) == 1);
  assert(max(-4, 4) == 4);
  assert(max(0, 2) == 2);
  assert(max(-3, -8) == -3);
  assert(max(0, 0) == 0);
  assert(expt(2, 5) == 32);
  assert(expt(10, 7) == 10000000);
  assert(expt(3, 3) == 27);
  assert(expt(0, 0) == 1);
  assert(expt(-2, 3) == -8);
  assert(expt(1, 12) == 1);
  assert(sqrt_int(25) == 5);
  assert(sqrt_int(24) == 4);
  assert(sqrt_int(17) == 4);
  assert(sqrt_int(22) == 4);
  assert(sqrt_int(39) == 6);
  assert(log_2(25) == 5);
  assert(log_2(5) == 3);
  assert(log_2(3) == 2);
  assert(log_2(32) == 5);
  assert(log_2(420) == 9);
}