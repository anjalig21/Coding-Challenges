#include "mutable-fun.h"
#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "cs136-trace.h"


/////////////////////////////////////////////////////////////////////////////
// BLACK QUESTION

void swap(int *px, int *py) {
  int temp = *px;
  *px = *py;
  *py = temp;
}

void reverse_norm(int a[], int len) {
  assert(a);
  assert(len >= 0);
  int min_value = 0;
  int number_of_swaps = (len / 2);
  for (int i = 0; i < len; i++) {
    if (i == 0) {
      min_value = a[i];
    }
    else if (a[i] < min_value) {
      min_value = a[i];
    }
  }
  for (int j = 0; j < number_of_swaps; j++) {
    swap(&a[j], &a[(len - 1) - j]);
  }
  for (int k = 0; k < len; k++) {
    a[k] -= min_value;
  }
}


/////////////////////////////////////////////////////////////////////////////
// GOLD QUESTIONS

void offset(int a[], int len) {
  assert(a);
  int q[2000000];
  for (int j = 0; j < len; j++) {
    q[j] = a[j];
  }
  if (len > 1) {
    for (int i = 0; i < len; i++) {
      if (i != 0) {
        a[i] -= q[i - 1];
      }
    }
  }
}

void selection_sort(int a[], int len) {
  assert(a);
  int pos = 0;
  for (int i = 0; i < len - 1; i++) {
    pos = i;
    for (int j = i + 1; j < len; j++) {
      if (a[j] < a[pos]) {
        pos = j;
      }
    }
    swap(&a[i], &a[pos]);
  }
}

void index_sort(const int a[], int len, int idx[]) {
  assert(a);
  int sorted_array[2000000] = {0};
  for (int i = 0; i < len; i++) {
    sorted_array[i] = a[i];
  }
  selection_sort(sorted_array, len);
  for (int i = 0; i < len; i++) {
    for (int j = 0; j < len; j++) {
      if (sorted_array[i] == a[j]) {
        idx[i] = j;
      }
    }
  }
}

int frequency(int array[], int length, int value) {
  assert(array);
  int counter = 0;
  for(int i = 0; i < length; i++) {
    if(value == array[i]) {
      counter++;
    }
  }
  return counter;
}

void most_freq_sort(int a[], int len) {
  assert(a);
  selection_sort(a, len);
  int previous_freq = 0;
  int current_freq = 0;
  int i = 0;
  while(i < len) {
    if (i != 0) {
      previous_freq = frequency(a, len, a[i - 1]);
    }
    else {
      previous_freq = frequency(a, len, a[i]);
    }
    current_freq = frequency(a, len, a[i]);
    if(current_freq < previous_freq) {
      swap(&a[i - 1], &a[i]);
      i = 0;
    }
    i++;
  }
}