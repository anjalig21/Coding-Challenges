#include "cs136.h"

// This file contains two functions that determine if an 
// individual qualifies for the CERB.

//
// A
//

// qualify_cerb_logical(age, stopped_working, 
// quit_voluntarily, ei_eligible, income_12months, income_4weeks)
// produces 1 if individual qualifies for the CERB and 0 otherwise.
bool qualify_cerb_logical(int age, bool stopped_working, 
                          bool quit_voluntarily, bool ei_eligible,
                          int income_12months, int income_4weeks) {
  return (age >= 15) && (income_12months >= 5000) && 
    (income_4weeks < 1000) && 
    ((ei_eligible == 1) || (stopped_working == 1 && quit_voluntarily == 0));
}


//
// B
//

// qualify_cerb_if(age, stopped_working, 
// quit_voluntarily, ei_eligible, income_12months, income_4weeks)
// produces 1 if individual qualifies for the CERB and 0 otherwise.
bool qualify_cerb_if(int age, bool stopped_working,
                     bool quit_voluntarily, bool ei_eligible,
                     int income_12months, int income_4weeks) {
  if (age >= 15) {
    if (income_12months >= 5000) {
      if (income_4weeks < 1000) {
        if (ei_eligible == 1) {
          return 1;
        }
        else if (stopped_working == 1) {
          if (quit_voluntarily == 0) {
            return 1;
          }
          else {
            return 0;
         }
       }
       else {
        return 0;
       }
     }
     else {
        return 0;
     }
    }
    else {
       return 0;
    }
  }
  else {
      return 0;
  }
}

int main(void) {
  assert(qualify_cerb_logical(22, true, false, true, 33000, 0));
  assert(qualify_cerb_if(22, true, false, true, 33000, 0));

  // ADD YOUR OWN TESTS BELOW:
  assert(!(qualify_cerb_logical(3, true, false, true, 33000, 0)));
  assert(!(qualify_cerb_logical(15, true, false, true, 5, 5000)));
  assert(qualify_cerb_logical(16, true, false, false, 5000, 0));
  assert(qualify_cerb_logical(15, true, true, true, 5000, 999));
  assert(!qualify_cerb_logical(9, false, false, true, 18000, 9));
  assert(qualify_cerb_if(15, true, true, true, 33000, 0));
  assert(!(qualify_cerb_if(15, true, true, true, 0, 0)));
  assert(!(qualify_cerb_if(2, true, false, true, 10000, 5)));
  assert(!qualify_cerb_if(25, true, false, true, 36000, 10000));
}