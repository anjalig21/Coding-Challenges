#include "cs136.h"

/////////////////////////////////////////////////////////////////////////////
// PROVIDED FUNCTION and CONSTANTS (DO NOT CHANGE)

// print_header(year, month) prints the calendar "header" 
//   for the given year/month
// notes: if month is invalid, no month line is printed
//        header is aligned for 4-digit years
// effects: produces output
void print_header(const int year, const int month) {
  if (month == 1) {
    printf("====January %d====\n", year);
  } else if (month == 2) {
    printf("===February %d====\n", year);
  } else if (month == 3) {
    printf("=====March %d=====\n", year);
  } else if (month == 4) {
    printf("=====April %d=====\n", year);
  } else if (month == 5) {
    printf("======May %d======\n", year);
  } else if (month == 6) {
    printf("=====June %d======\n", year);
  } else if (month == 7) {
    printf("=====July %d======\n", year);
  }else if (month == 8) {
    printf("====August %d=====\n", year);
  } else if (month == 9) {
    printf("===September %d===\n", year);
  } else if (month == 10) {
    printf("====October %d====\n", year);
  } else if (month == 11) {
    printf("===November %d====\n", year);
  } else if (month == 12) {
    printf("===December %d====\n", year);
  }
  printf("Su Mo Tu We Th Fr Sa\n");
}

// you may use these constants in your code if you wish
// it is not a requirement, but it is strongly recommended
// you may not change their values

const int SUNDAY = 0;
const int base_year = 1589;
const int base_year_jan_1 = SUNDAY;
const int max_year = 2999;
/////////////////////////////////////////////////////////////////////////////


// leap_year(year, month, day) produces true if the year 
// is a leap year and false otherwise.
bool leap_year(int year) {
  if (year % 400 == 0) {
    return true;
  }
  else if (year % 100 == 0) {
    return false;
  }
  else if (year % 4 == 0) {
    return true;
  }
  else {
    return false;
  }
}


// valid_date(year, month, day) determines if the provided 
// date is a valid date, according to the Gregorian Calendar.
bool valid_date(int year, int month, int day) {
  if (year > max_year || year < base_year) {
    return false;
  }
  else if (day > 31 || day < 1) {
    return false;
  }
  else if (month > 12 || month < 1) {
    return false;
  }
  else if (month == 2 && day > 29) {
    return false;
  }
  else if ((month == 4 || month == 6 || month == 9 || month == 11) 
           && day > 30) {
    return false;
  }
  else if (!leap_year(year)) {
    if (month == 2 && day > 28) {
      return false;
    }
    else {
      return true;
    }
  }
  else {
    return true;
  }
}


// day_calc(day, month, year) finds the date of the first day 
// of the month for the given date.
int day_calc(int day, int month, int year) {
  if (month == 4 || month == 6 ||
      month == 9 || month == 11) {
    day += (30 % 7);
    day %= 7;
  }
  else if (month == 2 && leap_year(year) == 1) {
    day += (29 % 7);
    day %= 7;
  }
  else if (month == 2) {
    day += (28 % 7);
    day %= 7;
  }
  else {
    day += (31 % 7);
    day %= 7;
  }
  return day;
}

// day_of_the_week(year, month, day) finds the "day of the week"
// for the given date.
// requires: date to be valid
int day_of_the_week(int year, int month, int day) {
  assert(valid_date(year, month, day));
  int month_count = 0;
  int year_count = base_year;
  int day_count = SUNDAY;
  int counter = -1;
  while (year_count < year) {
    month_count++;
    counter++;
    if (month_count > 12) {
      year_count++;
      month_count = 1;
    }
    if (counter > 0) {
      day_count = day_calc(day_count, month_count - 1, year_count);
    }
  }
  while (month_count < month) {
    month_count++;
    if (counter > 0) {
      day_count = day_calc(day_count, month_count - 1, year_count);
    }
  }
  day_count += day - 1;
  day_count %= 7;
  return day_count;
}

// month_calc(month, year) finds the number of days
// in the month for the given date.
int month_calc(int month, int year) {
  int day = 0;
  if (month == 4 || month == 6 ||
      month == 9 || month == 11) {
    day = 30;
  }
  else if (month == 2 && leap_year(year)) {
    day = 29;
  }
  else if (month == 2) {
    day = 28;
  }
  else {
    day = 31;
  }
  return day;
}

// print_calendar(year, month) prints a "pretty" 
// calendar for the given month
// effects: produces output
void print_calendar(int year, int month) {
  assert(valid_date(year, month, 1));
  print_header(year, month);
  int days_in_month = month_calc(month, year);
  int start_day = day_of_the_week(year, month, 1);
  
  for (int i = 0; i < start_day; i++) {
    printf("   ");
  }
  
  for (int i = 1; i <= days_in_month; i++) {
    printf("%2d", i);
    if (i == days_in_month) {
      printf("\n");
    }
    else if (day_of_the_week(year, month, i) != 6) {
      printf(" ");
    }
    else {
      printf("\n");
    }
  }
}


void assertion_tests(void) {
  // Due date is a valid date and a Thursday 
  assert(valid_date(2021, 1, 28));
  assert(day_of_the_week(2021, 1, 28) == 4);
  
  // Add your own assertion-based tests below
  assert(day_of_the_week(2021, 1, 25) == 1);
  assert(day_of_the_week(2002, 10, 21) == 1);
  assert(day_of_the_week(2002, 8, 21) == 3);
  assert(day_of_the_week(1589, 1, 1) == 0);
  assert(day_of_the_week(1589, 1, 2) == 1);
  assert(day_of_the_week(1592, 2, 29) == 6);
  assert(day_of_the_week(1592, 3, 1) == 0);
  assert(day_of_the_week(1794, 5, 26) == 1);
  assert(day_of_the_week(2020, 4, 30) == 4);
  assert(day_of_the_week(2021, 1, 25) == 1);
}

///////////////////////////////////////////////////////
// You do not need to modify the rest of the program //
///////////////////////////////////////////////////////

int main(void) {
  assertion_tests();
  while (1) {
    int year = read_int();
    int month = read_int();
    if (month == READ_INT_FAIL) {
      break;
    }
    print_calendar(year, month);
  }
}