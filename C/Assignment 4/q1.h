// This module provides functions that calculate stats
//   on integer inputs

// The following applies to all functions:
// requires: all pointer parameters are valid

struct statistics {
  int count;
  int sum;
  int min;
  int max;
};
// requires: count >= 0


// get_stats_params(sum, min, max) reads in all integers from input
//   and updates *sum, *min and *max accordingly, returning the
//   count of how many numbers were read in
// effects: reads input
//          modifies *sum, *min, *max
int get_stats_params(int *sum, int *min, int *max);

// get_stats_struct(stats) reads in all integers from input and updates
//   the stats structure accordingly.
// effects: reads input
//          modifies *stats
void get_stats_struct(struct statistics *stats);