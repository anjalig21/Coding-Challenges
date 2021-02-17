// You should complete this interface

struct burger_shack {
  int buns;
  int cheese;
  int patties;
  int pickles;
};

// in your burger_shack, you should define this constant
extern const struct burger_shack empty_shack;

// add_cheese(blocks) adds blocks of cheese to the inventory.
// A block contains 36 slices of cheese.
// effects: produces output
void add_pickles(struct burger_shack *shack, int jars);

// add_buns(crates) adds crates of buns to the inventory. 
// A crate contains a dozen dozen (144) buns.
// effects: produces output
void add_buns(struct burger_shack *shack, int crates);

// add_cheese(blocks) adds blocks of cheese to the inventory.
// A block contains 36 slices of cheese.
// effects: produces output
void add_cheese(struct burger_shack *shack, int blocks);

// add_patties(boxes) adds boxes of patties to the inventory.
// A box contains 48 patties.
// effects: produces output
void add_patties(struct burger_shack *shack, int boxes);

// order(burgers) tries to fulfill an order for the given 
// quantity of ++burgers.
// effects: produces output
void order(struct burger_shack *shack, int burgers);

// check_inventory() prints a message listing all of the current 
// ingredient inventory levels.
// effects: produces output
void check_inventory(const struct burger_shack *shack);