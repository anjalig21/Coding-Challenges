#include "q3.h"
#include "cs136-trace.h"
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// This initial "empty" shack depends on your burger_shack structure.
// Because we don't know what your fields are, we can't do this for you.
// If your fields simply need to be all initialized to zero, then
//   you do not have to change this constant:
const struct burger_shack empty_shack = {0};

// you must complete this implementation

void add_pickles(struct burger_shack *shack, int jars) {
  if (jars > 0) {
    shack->pickles = shack->pickles + (250 * jars);
  }
  else {
    printf("invalid: add_pickles\n");
  }
}

void add_buns(struct burger_shack *shack, int crates) {
  if (crates > 0) {
    shack->buns = shack->buns + (crates * 144);
  }
  else {
    printf("invalid: add_buns\n");
  }
}

void add_cheese(struct burger_shack *shack, int blocks) {
  if (blocks > 0) {
    shack->cheese = shack->cheese + (blocks * 36);
  }
  else {
    printf("invalid: add_cheese\n");
  }
}

void add_patties(struct burger_shack *shack, int boxes) {
  if (boxes > 0) {
    shack->patties = shack->patties + (boxes * 48);
  }
  else {
    printf("invalid: add_patties\n");
  }
}

void order(struct burger_shack *shack, int burgers) {
  if (burgers <= 0) {
    printf("invalid: order\n");
  }
  else if ((2 * burgers <= shack->patties) &&
           (2 * burgers <= shack->cheese) &&
           (3 * burgers <= shack->pickles) && 
           (burgers <= shack->buns)) {
    shack->patties = shack->patties - (2 * burgers);
    shack->cheese = shack->cheese - (2 * burgers);
    shack->pickles = shack->pickles - (3 * burgers);
    shack->buns = shack->buns - burgers;
    printf("order complete: %d burgers\n", burgers);
  }
  else {
    printf("order cancelled:\n");
    if (shack->buns < burgers) {
      printf("  not enough buns\n");
      if (shack->cheese < (2 * burgers)) {
        printf("  not enough cheese\n");
        if (shack->patties < (2 * burgers)) {
          printf("  not enough patties\n");
          if (shack->pickles < (3 * burgers)) {
            printf("  not enough pickles\n");
          }
        }
      }
    }
    else if (shack->cheese < (2 * burgers)) {
      printf("  not enough cheese\n");
      if (shack->patties < (2 * burgers)) {
        printf("  not enough patties\n");
        if (shack->pickles < (3 * burgers)) {
          printf("  not enough pickles\n");
        }
      }
    }
    else if (shack->patties < (2 * burgers)) {
      printf("  not enough patties\n");
      if (shack->pickles < (3 * burgers)) {
        printf("  not enough pickles\n");
      }
    }
    else if (shack->pickles < (3 * burgers)) {
      printf("  not enough pickles\n");
    }
  }
}

void check_inventory(const struct burger_shack *shack) {
  printf("inventory:\n");
  printf("  buns: [%d] ", shack->buns);
  if (shack->buns < 10) {
    printf("WARNING\n");
  }
  else if (shack->buns < 100) {
     printf("LOW\n");
  }
  else {
    printf("OK\n");
  }
  printf("  cheese: [%d] ", shack->cheese);
  if (shack->cheese < 20) {
    printf("WARNING\n");
  }
  else if (shack->cheese < 200) {
     printf("LOW\n");
  }
  else {
    printf("OK\n");
  }
  printf("  patties: [%d] ", shack->patties);
  if (shack->patties < 20) {
    printf("WARNING\n");
  }
  else if (shack->patties < 200) {
     printf("LOW\n");
  }
  else {
    printf("OK\n");
  }
  printf("  pickles: [%d] ", shack->pickles);
  if (shack->pickles < 30) {
    printf("WARNING\n");
  }
  else if (shack->pickles < 300) {
     printf("LOW\n");
  }
  else {
    printf("OK\n");
  }
}