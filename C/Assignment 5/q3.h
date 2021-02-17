// A module for an int sequence ADT (with a maximum length)

#include <stdbool.h>

// SEASHELL_READONLY

extern const int SEQUENCE_MAXLEN; // define as 1024 in your .c file

// NOTE: This is a "transparent" structure, but should be treated
//        as opaque: clients should NOT access fields directly
struct sequence {
  int len;
  int maxlen;
  int data[1024];
};

// The following applies to all functions:
// requires: all seq pointers are valid (not NULL)
//           all function pointers are valid (not NULL)

// sequence_init(seq) initializes (or resets) seq to be empty
//   with maximum capacity SEQUENCE_MAXLEN
// effects: modifies seq
void sequence_init(struct sequence *seq);

// sequence_length(seq) returns the number of items in seq
int sequence_length(const struct sequence *seq);

// sequence_item_at(seq, pos) returns the item in seq at position pos
// requires: 0 <= pos < sequence_length(seq)
int sequence_item_at(const struct sequence *seq, int pos);

// sequence_insert_at(seq, pos, val) inserts a new item with value val
//   at position pos in seq
// note: changes the position of items that were at position >= pos
// requires: 0 <= pos <= sequence_length(seq) < SEQUENCE_MAXLEN
// effects: modifies seq
void sequence_insert_at(struct sequence *seq, int pos, int val);

// sequence_remove_at(seq, pos) removes the item at position pos in seq
//   and returns the removed value
// note: changes the position of items that were at position > pos
// requires: 0 <= pos < sequence_length(seq)
// effects: modifies seq
int sequence_remove_at(struct sequence *seq, int pos);

//////////////////////////////////////////////////////////////////////////
// The above are the typical operations (functions) for a Sequence ADT. //
// You must also complete the following "Advanced" operations.          //
//////////////////////////////////////////////////////////////////////////

// sequence_equiv(seq1, seq2) determines if seq1 and seq2 are equivalent
//   (they both have the same length and have identical sequences of items)
bool sequence_equiv(const struct sequence *seq1, const struct sequence *seq2);

// sequence_print(seq) prints out the items in seq
//   using the format: "[item_0,item_1,...,item_last]\n"
//   or "[empty]\n"
// examples: [1,3,6]
//           [42]
//           [empty]
// effects: produces output
void sequence_print(const struct sequence *seq);

// sequence_build(seq, f, n) builds a sequence [f(0),f(1),..,f(n-1)]
// note: ignores ("overwrites") previous contents of seq
// requires: 0 <= n <= SEQUENCE_MAXLEN
// effects: modifies seq
void sequence_build(struct sequence *seq, int (*f)(int), int n);

// sequence_filter(seq, f) removes all items i where f(i) is false
// effects: modifies seq
void sequence_filter(struct sequence *seq, bool (*f)(int));

// sequence_append(seq1, seq2) appends all items from seq2 to the end
//   of seq1
// requires: seq1 and seq2 are different sequences
//           sequence_length(seq1) + sequence_length(seq2) <= SEQUENCE_MAXLEN
// effects: modifies seq1
void sequence_append(struct sequence *seq1, const struct sequence *seq2);

// sequence_remove_dups(seq) removes all duplicates from seq
//   (keeping each first occurrence)
// examples: [1,3,6] => [1,3,6]
//           [1,1,3,1,6,1,3] => [1,3,6]
// effects: modifies seq
void sequence_remove_dups(struct sequence *seq);