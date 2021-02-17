#include "q2.h"
#include "cs136-trace.h"
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>

int get_stats_params(int *sum, int *min, int *max) {
  assert(min);
  assert(max);
  assert(sum);
  int sum_count = 0;
  int min_num = 0;
  int max_num = 0;
  int n = 0;
  int counter = 0;
  while (scanf("%d", &n) == 1) {
    sum_count += n;
    if (counter == 0) {
      max_num = n;
      min_num = n;
    }
    else if (n > max_num) {
      max_num = n;
    }
    else if (n < min_num) {
      min_num = n;
    }
    counter++;
  }
  *sum = sum_count;
  *min = min_num;
  *max = max_num;
  return counter;
}

void get_stats_struct(struct statistics *stats) {
  assert(stats);
  int sum_count = 0;
  int min_num = 0;
  int max_num = 0;
  int n = 0;
  int counter = 0;
  while (scanf("%d", &n) == 1) {
    sum_count += n;
    if (counter == 0) {
      max_num = n;
      min_num = n;
    }
    else if (n > max_num) {
      max_num = n;
    }
    else if (n < min_num) {
      min_num = n;
    }
    counter++;
  }
  stats->sum = sum_count;
  stats->min = min_num;
  stats->max = max_num;
  stats->count = counter;
}