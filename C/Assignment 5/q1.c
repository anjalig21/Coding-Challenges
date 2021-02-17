#include "const-fun.h"
#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// note: functions are currently returning "dummy" values that
//       should be replaced

/////////////////////////////////////////////////////////////////////////////
// BLACK QUESTION

int spread(const int a[], int len) {
  assert(a);
  assert(len > 0);
  int max_value = a[0];
  int min_value = a[0];
  for (int i = 1; i < len; i++) {
    if (a[i] > max_value) {
      max_value = a[i];
    }
    if (a[i] < min_value) {
      min_value = a[i];
    }
  }
  return max_value - min_value;
}


/////////////////////////////////////////////////////////////////////////////
// GOLD QUESTIONS

bool unique(const int a[], int len) {
  assert(a);
  assert(len > 0);
  for (int i = 0; i < len; i++) {
    for (int j = (i + 1); j < len; j++) {
      if (a[i] == a[j]) {
        return 0;
      }
    }
  }
  return 1;
}


int skew(const int a[], int len) {
  assert(a);
  assert(len > 0);
  int counter = 0;
  int sum = 0;
  int counter_less = 0;
  int counter_more = 0;
  for (int i = 0; i < len; i++) {
    counter++;
    sum += a[i];
  }
  for (int j = 0; j < len; j++) {
    if (a[j] * counter < sum) {
      counter_less++;
    }
    if (a[j] * counter > sum) {
      counter_more++;
    }
  }
  if (counter_more > counter_less) {
    return 1;
  }
  else if (counter_more < counter_less) {
    return -1;
  }
  else {
    return 0;
  }
}


int longest_sorted(const int a[], int len) {
  assert(a);
  assert(len > 0);
  const int MIN_SIZE = 2;
  int seq_sorted = MIN_SIZE;
  int final_seq = MIN_SIZE;
  int same_num = 0;
  bool increasing = true;
  bool decreasing = true;
  if (len == 1) {
    return 1;
  } 
  else if (a[1] == a[0]) {
    same_num++;
  } 
  else if (a[1] > a[0]) {
    increasing = true;
    decreasing = false;
  } 
  else {
    decreasing = true;
    increasing = false;
  } 
  for (int i = 2; i < len; ++i) {
    if (increasing == true && a[i] > a[i-1]) {
      seq_sorted++;
      same_num = 0;
    } 
    else if ((increasing == true || decreasing == true) && 
             a[i] == a[i-1]) {
      same_num++;
      seq_sorted++;
    } 
    else if (decreasing == true && a[i] < a[i-1]) {
      seq_sorted++;
      same_num = 0;
    } 
    else if (increasing == true && a[i] < a[i-1]) {
      increasing = false;
      decreasing = true;
      if (seq_sorted > final_seq) {
        final_seq = seq_sorted;
      } seq_sorted = MIN_SIZE + same_num;
    } 
    else {
      increasing = true;
      decreasing = false;
      if (seq_sorted > final_seq) {
        final_seq = seq_sorted;
      } seq_sorted = MIN_SIZE + same_num;
    } 
  } 
  if (seq_sorted > final_seq) {
      return seq_sorted;
  } 
  else {
    return final_seq;
  }