// root.h [Interface]

// This module is about calculating roots

// bisection(f, lo, hi, tolerance) finds a root of an arbitrary function f.
// requires: tolerance is positive
//           lo is less than hi
//           f(lo) and f(hi) must both be non-zero and have opposite signs
//           f is continuous between lo..hi (not asserted)
double bisection(double (*f)(double), double lo, double hi, double tolerance);