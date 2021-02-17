#include "cs136.h"

// IMPORTANT: review burger-io.txt to see the input format

// The partial printf("...") statements have been provided
// to avoid any unfortuante typos

int buns = 0;
int cheese = 0;
int patties = 0;
int pickles = 0;

// add_buns(crates) adds crates of buns to the inventory. 
// A crate contains a dozen dozen (144) buns.
// effects: produces output
void add_buns(int crates) {
  if (crates > 0) {
    buns = buns + (crates * 144);
  }
  else {
    printf("invalid: add_buns\n");
  }
}

// add_cheese(blocks) adds blocks of cheese to the inventory.
// A block contains 36 slices of cheese.
// effects: produces output
void add_cheese(int blocks) {
  if (blocks > 0) {
    cheese = cheese + (blocks * 36);
  }
  else {
    printf("invalid: add_cheese\n");
  }
}

// add_patties(boxes) adds boxes of patties to the inventory.
// A box contains 48 patties.
// effects: produces output
void add_patties(int boxes) {
  if (boxes > 0) {
    patties = patties + (boxes * 48);
  }
  else {
    printf("invalid: add_patties\n");
  }
}

// add_pickles(jars) adds jars of pickles to the inventory. 
// A jar contains 250 pickles.
// effects: produces output
void add_pickles(int jars) {
  if (jars > 0) {
    pickles = pickles + (250 * jars);
  }
  else {
    printf("invalid: add_pickles\n");
  }
}

// order(burgers) tries to fulfill an order for the given 
// quantity of ++burgers.
// effects: produces output
void order(int burgers) {
  if (burgers <= 0) {
    printf("invalid: order\n");
  }
  else if ((2 * burgers <= patties) && (2 * burgers <= cheese) &&
           (3 * burgers <= pickles) && (burgers <= buns)) {
    patties = patties - (2 * burgers);
    cheese = cheese - (2 * burgers);
    pickles = pickles - (3 * burgers);
    buns = buns - burgers;
    printf("order complete: %d burgers\n", burgers);
  }
  else {
    printf("order cancelled:\n");
    if (buns < burgers) {
      printf("  not enough buns\n");
      if (cheese < (2 * burgers)) {
        printf("  not enough cheese\n");
        if (patties < (2 * burgers)) {
          printf("  not enough patties\n");
          if (pickles < (3 * burgers)) {
            printf("  not enough pickles\n");
          }
        }
      }
    }
    else if (cheese < (2 * burgers)) {
      printf("  not enough cheese\n");
      if (patties < (2 * burgers)) {
        printf("  not enough patties\n");
        if (pickles < (3 * burgers)) {
          printf("  not enough pickles\n");
        }
      }
    }
    else if (patties < (2 * burgers)) {
      printf("  not enough patties\n");
      if (pickles < (3 * burgers)) {
        printf("  not enough pickles\n");
      }
    }
    else if (pickles < (3 * burgers)) {
      printf("  not enough pickles\n");
    }
  }
}

// check_inventory() prints a message listing all of the current 
// ingredient inventory levels.
// effects: produces output
void check_inventory(void) {
  printf("inventory:\n");
  printf("  buns: [%d] ", buns);
  if (buns < 10) {
    printf("WARNING\n");
  }
  else if (buns < 100) {
     printf("LOW\n");
  }
  else {
    printf("OK\n");
  }
  printf("  cheese: [%d] ", cheese);
  if (cheese < 20) {
    printf("WARNING\n");
  }
  else if (cheese < 200) {
     printf("LOW\n");
  }
  else {
    printf("OK\n");
  }
  printf("  patties: [%d] ", patties);
  if (patties < 20) {
    printf("WARNING\n");
  }
  else if (patties < 200) {
     printf("LOW\n");
  }
  else {
    printf("OK\n");
  }
  printf("  pickles: [%d] ", pickles);
  if (pickles < 30) {
    printf("WARNING\n");
  }
  else if (pickles < 300) {
     printf("LOW\n");
  }
  else {
    printf("OK\n");
  }
}

// burger_io() is an I/O driver that reads commands and values from input
//   and then calls the corresponding burger inventory functions
// effects: reads input
//          produces output
void burger_io(void) {
  int cmd = read_symbol();
  if (cmd == lookup_symbol("add")) {
    int type = read_symbol();
    int qty = read_int();
    if (qty == READ_INT_FAIL) {
      printf("UNEXPECTED: INVALID INPUT FILE [add]\n");
      return;
    }
    if (type == lookup_symbol("buns")) {
      add_buns(qty);
    } else if (type == lookup_symbol("cheese")) {
      add_cheese(qty);
    } else if (type == lookup_symbol("patties")) {
      add_patties(qty);
    } else if (type == lookup_symbol("pickles")) {
      add_pickles(qty);
    } else {
      printf("UNEXPECTED: INVALID INPUT FILE [add]\n");
      return;
    }
  } else if (cmd == lookup_symbol("order")) {
    int qty = read_int();
    if (qty == READ_INT_FAIL) {
      printf("UNEXPECTED: INVALID INPUT FILE [order]\n");
      return;
    }
    order(qty);
  } else if (cmd == lookup_symbol("inventory")) {
    check_inventory();
  } else if (cmd == lookup_symbol("quit")) {
    return;
  } else {
    printf("UNEXPECTED: INVALID INPUT FILE [bad/missing command]\n");
    return;
  }
  burger_io();  
}

      
int main(void) {
  burger_io();
}