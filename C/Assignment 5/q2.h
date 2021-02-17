// This module provides a variety of array functions
// (that are not very cohesive)

// SEASHELL_READONLY

// For all of these functions:
//   * you must NOT define your own local arrays
//   * do not assume anything about the length of the array
//     (other than it is positive)

// The following applies to all functions:
// requires: all array parameters are valid (not NULL)
//           all array length parameters (e.g., len) are > 0
//           all arrays have a length >= length parameter [not asserted]

/////////////////////////////////////////////////////////////////////////////
// BLACK QUESTION

// reverse_norm(a, len) modifies the elements of a so that they are in
//   the reverse of their original order, and the value of the smallest
//   element in a is subtracted from each element
// examples: {1, 3, 6} => {5, 2, 0}
//           {1, 0, -1} => {0, 1, 2}
//           {1} => {0}
// effects: modifies a
void reverse_norm(int a[], int len);


/////////////////////////////////////////////////////////////////////////////
// GOLD QUESTIONS


// offset(a, len) modifies the elements of a so that each element a[i]
//   is now the difference between the original a[i] and a[i-1]
// notes: a[0] is not modified
//        a[1] becomes a[1] - a[0]
// examples: {1, 3, 6} => {1, 2, 3}
//           {1, 10, 5} => {1, 9, -5}
//           {1} => {1}
// effects: modifies a
void offset(int a[], int len);


// index_sort(a, len, idx) modifies idx so that it contains the indices of the
//   array a arranged so that their corresponding elements are in ascending
//   order. For example, if after calling this function idx[0] is j,
//   then that means a[j] is the smallest element in a.
//   In other words, a[idx[0]] is the smallest element.
// notes: a[idx[0]] is the smallest element of a
//        a[idx[1]] is next larger element, and a[idx[len-1]] is the largest
//        idx contains all of the numbers 0..len-1, each occurring once
//        this function completely overwrites the previous contents of idx
// examples: a: {6, 7, 1, 0, 2, -1, 4} => idx: {5, 3, 2, 4, 6, 0, 1}
//           a: {5, 10, 15} => idx: {0, 1, 2} 
// requires: idx is the same length as a [not asserted]
//           (to make testing easier) a contains no duplicates [not asserted]
// effects: modifies idx
void index_sort(const int a[], int len, int idx[]);


// most_freq_sort(a, len) sorts all of the elements in a by their frequency
//   in ascending order, so a[0] is the least frequently occurring element
//   and a[len-1] contains the most frequently occurring element
// notes: to break ties, sort element in ascending order
//        remember: you may not define any additional arrays
// examples: {3, 3, 7, 3, 7, 5} => {5, 7, 7, 3, 3, 3}
//           {3, 4, 3, 5, 1, 4, 2} => {1, 2, 5, 3, 3, 4, 4}
// effects: modifies a
void most_freq_sort(int a[], int len);