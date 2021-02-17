// For all of these functions:
//   * you cannot mutate any of the arrays
//   * you must NOT define your own local arrays
//   * do not assume anything about the length of the array
//     (other than it is positive)

// The following applies to all functions:
// requires: all array parameters are valid (not NULL)
//           all array length parameters (e.g., len) are > 0
//           all arrays have a length >= length parameter [not asserted]

/////////////////////////////////////////////////////////////////////////////
// BLACK QUESTION


// spread(a, len) finds the difference between the largest element and the
//   smallest element in a (i.e., max - min)
// example: {1, 3, 6} => 5
// example: {1, 3, -6} => 9
// example: {1} => 0
int spread(const int a[], int len);


/////////////////////////////////////////////////////////////////////////////
// GOLD QUESTIONS

// unique(a, len) determines whether all of the elements of a are "unique",
//   or in other words, if there are no duplicates in a
// examples: {1, 3, 6} => true
//           {1, 3, 1} => false
bool unique(const int a[], int len);


// skew(a, len) finds the "skewness" of the array a, which involves
//   comparing all elements to the mean
// notes: returns 1 if more elements are higher than the mean than lower
//        returns -1 if more elements are lower than the mean than higher
//        returns 0 if there are exactly the same number of values
//          higher than the mean and lower than the mean
//        uses the "true" mean (no rounding)
// examples: {1, 3, 4} => 1
//           {1, 3, 6} => -1
//           {1} => 0
int skew(const int a[], int len);


// longest_sorted(a, len) finds the length of the longest consecutive
//   sequence or "subarray" that contains elements that are sorted
//   in either non-descending (e.g., ascending) or non-increasing order
// examples: {5, 1, 3, 6, 2} => 3 (1 <= 3 <= 6)
//           {3, 2, 2, 1} => 4 (3 >= 2 >= 2 >= 1)
int longest_sorted(const int a[], int len);