#include "cs136.h"

// This file contains 4 functions: away_div produces the quotient of 
// num / denom  rounding away from 0, round_nice rounds (num / denom) 
// away from zero for any  remainder that is at least ⅓ and towards 0 
// otherwise, cookie_order determines the  total number of boxes 
// of Girl Guide cookies and pizza_order determines the 
// total number of pizzas that must be ordered according to user input.


//
// A
//

// away_div(num, denom) produces the quotient of num / denom
// rounding away from 0
// requires: denom != 0
int away_div(int num, int denom) {
  assert(denom != 0);
  if (num % denom == 0) {
    return (num / denom);
  }
  else if (((num < 0) && (denom > 0)) || ((num > 0) && (denom < 0))) {
    return (abs(num/denom) + 1) * -1;
  }
  else {
    return (num/denom) + 1;
  }
}


//
// B
//

// round_nice(num, denom) rounds (num / denom) away from zero for any 
// remainder that is at least ⅓ (one third) and towards 0 otherwise.
// requires: denom > 0
int round_nice(int num, int denom) {
  assert(denom > 0);
  if (num % denom == 0) {
    return (num / denom);
  }
  else if (num < 0) {
    if ((3 * (abs(num) % denom)) >= denom) {
      return ((num / denom) - 1);
    }
    else {
      return (num / denom);
    }
  }
  else {
    if ((3 * (num % denom)) >= denom) {
      return ((num / denom) + 1);
    }
    else {
      return (num / denom);
    }
  }
}


//
// C
//

// mint_boxes(mint) calculates the number of mint cookie boxes
// required for the number of mint cookies inputted.
int mint_boxes(int mint) {
  return away_div(mint , 22);
}

// chocolate_boxes(chocolate) calculates the number of chocolate cookie boxes
// required for the number of chocolate cookies inputted.
int chocolate_boxes(int chocolate) {
  return away_div(chocolate , 10);
}

// vanilla_boxes(vanilla, chocolate_boxes) calculates the number of vanilla 
// cookie boxes required for the number of vanilla cookies inputted.
int vanilla_boxes(int vanilla, int chocolate_boxes) {
  if (vanilla <= (chocolate_boxes * 10)) {
    return 0;
  }
  else {
    return away_div((vanilla - (chocolate_boxes * 10)), 10);
  }
}
// cookie_order(mint, chocolate, vanilla) determines the 
// total number of boxes of Girl Guide cookies 
// required if you desire the given quantity of each type of cookie.
// requires: mint >= 0
//           chocolate >= 0
//           vanilla >= 0
int cookie_order(int mint, int chocolate, int vanilla) {
  assert(mint >= 0);
  assert(chocolate >= 0);
  assert(vanilla >= 0);
  return mint_boxes(mint) + 
    chocolate_boxes(chocolate) + 
    vanilla_boxes(vanilla, chocolate_boxes(chocolate));  
}


//
// D
//

// vegan_pizzas(vegan, slices_per_pizza) determines the amount
// of boxes needed for vegan pizzan slices based on the amount
// requested by the consumer (vegan).
int vegan_pizzas(int vegan, int slices_per_pizza) {
  return away_div(vegan, slices_per_pizza);
}

// mixed_pizzas(canadian, hawaiian, spicy, slices, counter) 
// determines the amount of additional pizza boxes needed 
// by grouping together pizzas in half.
int mixed_pizzas(int canadian, int hawaiian, 
                 int spicy, int slices, int counter) {
  if (canadian + spicy + hawaiian == slices) {
  counter = counter + 1;
  canadian = 0;
  spicy = 0;
  hawaiian = 0;
  return mixed_pizzas(canadian, hawaiian, spicy, slices, counter);
  }
  else if ((canadian >= (slices / 2)) && (hawaiian >= (slices / 2))) {
    canadian = canadian - (slices / 2);
    hawaiian = hawaiian - (slices / 2);
    counter = counter + 1;
    return mixed_pizzas(canadian, hawaiian, spicy, slices, counter);
  }
  else if ((canadian >= (slices / 2)) && (spicy >= (slices / 2))) {
    canadian = canadian - (slices / 2);
    spicy = spicy - (slices / 2);
    counter = counter + 1;
    return mixed_pizzas(canadian, hawaiian, spicy, slices, counter);
  }
  else if ((hawaiian >= (slices / 2)) && (spicy >= (slices / 2))) {
    hawaiian = hawaiian - (slices / 2);
    spicy = spicy - (slices / 2);
    counter = counter + 1;
    return mixed_pizzas(canadian, hawaiian, spicy, slices, counter);
  }
  else if (canadian > 0) {
    counter = counter + 1;
    canadian = 0;
    return mixed_pizzas(canadian, hawaiian, spicy, slices, counter);
  }
  else if (hawaiian > 0) {
    counter = counter + 1;
    hawaiian = 0;
    return mixed_pizzas(canadian, hawaiian, spicy, slices, counter);
  }
  else if (spicy > 0) {
    counter = counter + 1;
    spicy = 0;
    return mixed_pizzas(canadian, hawaiian, spicy, slices, counter);
  }
  else {
    return counter;
  }
}
    
// canadian_pizzas(canadian, slices_per_pizza, counter) determines the amount
// of boxes needed for canadian pizzan slices based on the amount
// requested by the consumer (canadian).
int canadian_pizzas(int canadian, int slices_per_pizza, int counter) {
  if (canadian < slices_per_pizza) {
    return counter;
  }
  else if (canadian == slices_per_pizza) {
    counter = counter + 1;
    return counter;
  }
  else {
    counter = counter + 1;
    return canadian_pizzas((canadian - slices_per_pizza), 
                           slices_per_pizza, counter);
  }
}
  
// hawaiian_pizzas(hawaiian, slices_per_pizza, counter) determines the amount
// of boxes needed for hawaiian pizzan slices based on the amount
// requested by the consumer (hawaiian).
int hawaiian_pizzas(int hawaiian, int slices_per_pizza, int counter) {
  if (hawaiian < slices_per_pizza) {
    return counter;
  }
  else if (hawaiian == slices_per_pizza) {
    counter = counter + 1;
    return counter;
  }
  else {
    counter = counter + 1;
    return hawaiian_pizzas((hawaiian - slices_per_pizza), 
                           slices_per_pizza, counter);
  }
}

// spicy_pizzas(spicy, slices_per_pizza, counter) determines the amount
// of boxes needed for spicy pizzan slices based on the amount
// requested by the consumer (spicy).
int spicy_pizzas(int spicy, int slices_per_pizza, int counter) {
  if (spicy < slices_per_pizza) {
    return counter;
  }
  else if (spicy == slices_per_pizza) {
    counter = counter + 1;
    return counter;
  }
  else {
    counter = counter + 1;
    return spicy_pizzas((spicy - slices_per_pizza), 
                        slices_per_pizza, counter);
  }
}

// pizza_order(canadian, hawaiian, spicy, vegan, slices_per_pizza) 
// determines the total number of pizzas that must be ordered if 
// each pizza contains a given number of slices_per_pizza and 
// you require the given quantity of slices of each type of pizza.
int pizza_order(int canadian, int hawaiian, int spicy, int vegan,
                int slices_per_pizza) {
  assert(slices_per_pizza > 0);
  assert(slices_per_pizza % 2 == 0);
  assert(canadian >= 0);
  assert(hawaiian >= 0);
  assert(spicy >= 0);
  assert(vegan >= 0);
  int remaining_canadian = canadian - 
    (canadian_pizzas(canadian, slices_per_pizza, 0) *
    slices_per_pizza);
  int remaining_hawaiian = hawaiian - 
    (hawaiian_pizzas(hawaiian, slices_per_pizza, 0) * 
      slices_per_pizza);
  int remaining_spicy = spicy - 
    (spicy_pizzas(spicy, slices_per_pizza, 0) *
     slices_per_pizza);
  return vegan_pizzas(vegan, slices_per_pizza) + 
    spicy_pizzas(spicy, slices_per_pizza, 0) + 
    hawaiian_pizzas(hawaiian, slices_per_pizza, 0) +
    canadian_pizzas(canadian, slices_per_pizza, 0) +
    mixed_pizzas(remaining_canadian, remaining_hawaiian, 
                 remaining_spicy, slices_per_pizza, 0);
}

int main(void) {
  assert(away_div(4, 1) == 4);
  assert(round_nice(4, 1) == 4);
  assert(cookie_order(44, 20, 20) == 4);
  assert(pizza_order(8, 8, 8, 8, 8) == 4);

  // ADD YOUR OWN TESTS BELOW:
  assert(away_div(2, 3) == 1);
  assert(away_div(1, 3) == 1);
  assert(away_div(-6, 4) == -2);
  assert(away_div(9, -5) == -2);
  assert(away_div(8, -2) == -4);
  assert(away_div(-8, 2) == -4);
  assert(round_nice(268, 3) == 90);
  assert(round_nice(268, 7) == 38);
  assert(round_nice(-268, 3) == -90);
  assert(round_nice(56, 45) == 1);
  assert(round_nice(2, 1234000000) == 0);
  assert(cookie_order(2, 5, 20) == 3);
  assert(cookie_order(21, 7, 7) == 2);
  assert(cookie_order(0, 0, 0) == 0);
  assert(cookie_order(22, 10, 10) == 2);
  assert(cookie_order(4, 25, 78) == 9);
  assert(cookie_order(48, 9, 25) == 6);
  assert(pizza_order(9, 9, 3, 1, 6) == 5);
  assert(pizza_order(6, 6, 6, 6, 6) == 4);
  assert(pizza_order(2, 5, 2, 1, 4) == 4);
  assert(pizza_order(3, 6, 8, 0, 2) == 9);
  assert(pizza_order(0, 0, 1, 1, 8) == 2);
  assert(pizza_order(2, 2, 0, 0, 4) == 1);
  assert(pizza_order(2, 2, 2, 0, 6) == 1);
}