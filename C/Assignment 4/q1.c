#include "q1.h"
#include "cs136-trace.h"
#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

double bisection(double (*f)(double), double lo, double hi, double tolerance) {
  assert(tolerance > 0);
  assert(lo < hi);
  assert(f(lo) != 0);
  assert(f(hi) != 0);
  assert(((f(lo) < 0 && f(hi) > 0) || (f(lo) > 0 && f(hi) < 0)));
  bool loop = true;
  double midpoint = ((lo + hi) / 2);
  while (loop) {
    if ((f(midpoint) <= tolerance) && (f(midpoint) >= (tolerance * -1))) {
      loop = false;
    }
    else if (f(midpoint) < 0) {
      lo = midpoint;
    }
    else {
      hi = midpoint;
    }
    midpoint = ((lo + hi) / 2);
  }
  return midpoint;
}