#include "sequence.h"
#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

const int SEQUENCE_MAXLEN = 1024;

void sequence_init(struct sequence *seq) {
  assert(seq);
  seq->len = 0;
  seq->maxlen = SEQUENCE_MAXLEN;
}

int sequence_length(const struct sequence *seq) {
  assert(seq);
  return seq->len;
}

int sequence_item_at(const struct sequence *seq, int pos) {
  assert(seq);
  assert(0 <= pos < sequence_length(seq));
  return seq->data[pos];
}

void sequence_insert_at(struct sequence *seq, int pos, int val) {
  assert(seq);
  assert(0 <= pos <= sequence_length(seq) < SEQUENCE_MAXLEN);
  if (seq->len == 0) {
    seq->data[pos] = val;
  }
  for (int i = (seq->len - 1); i > pos; i--) {
    seq->data[i] = seq->data[i - 1];
  }
  seq->data[pos] = val;
  seq->len++;
}

int sequence_remove_at(struct sequence *seq, int pos) {
  assert(seq);
  assert(0 <= pos && pos < seq->len);
  int remove_value = 0;
  int temp = 0;
  for (int i = 0; i < seq->len; i++) {
    if (i == pos) {
      remove_value = seq->data[i]; 
      temp = seq->data[i + 1];
      seq->data[i] = temp;
    } 
    else if (i > pos) {
      temp = seq->data[i + 1];
      seq->data[i] = temp;
    }
  }
  seq->len -= 1;
  return remove_value;
}

bool sequence_equiv(const struct sequence *seq1, const struct sequence *seq2) {
  assert(seq1);
  assert(seq2);
  if (seq1->len != seq2->len) {
    return false;
  }
  for (int i = 0; i < seq1->len; i++) {
    if (seq1->data[i] != seq2->data[i]) {
      return false;
    }
  }
  return true;
}

void sequence_print(const struct sequence *seq) {
  assert(seq);
  if (seq->len == 0) {
    printf("[empty]\n");
  } 
  else {
    printf("[");
    for (int i = 0; i < seq->len; i++) {
      if (i == (seq->len - 1)) {
        printf("%d", seq->data[i]);
      } 
      else {
        printf("%d,", seq->data[i]);
      }
    }
    printf("]\n");
  }
}

void sequence_build(struct sequence *seq, int (*f)(int), int n) {
  assert(seq);
  assert(f);
  assert(0 <= n <= SEQUENCE_MAXLEN);
  for (int i = 0; i < n; i++) {
    seq->data[i] = f(i);
  }
  seq->len = n;
}

void sequence_filter(struct sequence *seq, bool (*f)(int)) {
  assert(seq);
  assert(f);
  for (int i = seq->len - 1; i >= 0; i--) {
    if (f(i) == false) {
      sequence_remove_at(seq, i);
    }
  }
}

void sequence_append(struct sequence *seq1, const struct sequence *seq2) {
  assert(seq1);
  assert(seq2);
  assert(seq1 != seq2);
  assert((seq1->len + seq2->len) <= SEQUENCE_MAXLEN);
  int len_seq1 = seq1->len;
  for (int i = 0; i < seq2->len; i++) {
    seq1->data[len_seq1] = seq2->data[i];
    ++len_seq1;
  }
  seq1->len += seq2->len;
}

void sequence_remove_dups(struct sequence *seq) {
  assert(seq);
  for (int i = 0; i < seq->len; ++i) {
    for (int j = i + 1; j < seq->len + 1; ++j) {
      if (seq->data[i] == seq->data[j]) {
        sequence_remove_at(seq, j);
        while (seq->data[i] == seq->data[j]) {
          sequence_remove_at(seq, j);
        }
      }
    }
  }
}