#include "cs136.h"

// int_printer(counter) reads in all integers from input and prints
// them out in their original order and then in reverse order.
// effects: reads input
//          produces output
void int_printer(void) {
  int n = read_int();
  if (n != READ_INT_FAIL) {
    printf("%d\n", n);
    int_printer();
    printf("%d\n", n);
  }
}

// symbol_at(counter) prints "@" counter amount of times
// requires: counter >= 0
// effects: produces output
void symbol_at(int counter) {
  assert(counter >= 0);
  if (counter == 0) {
    return;
  }
  else {
    printf("@");
    counter--;
    symbol_at(counter);
  }
}

// print_spaces(counter) prints " " counter amount of times
// requires: counter >= 0
// effects: produces output
void print_spaces(int counter) {
  assert(counter >= 0);
  if (counter == 0) {
    return;
  }
  else {
    printf(" ");
    counter--;
    print_spaces(counter);
  }
}

// image_output() reads in all integers from input and for a positive 
// integer n, it prints out "a" n amount of times, for 
// a negative integer n, it prints out " " n amount of times,
// and for n==0, it prints a new line.
// effects: reads input
//          produces output
void image_output(void) {
  int n = read_int();
  if (n != READ_INT_FAIL) {
    if (n > 0) {
      symbol_at(n);
      image_output();
    }
    else if (n < 0) {
      print_spaces(abs(n));
      image_output();
    }
    else {
      printf("\n");
      image_output();
    }
  }
}

// reverse_digits(number) prints out the number reversed
// effects: produces output
void reverse_digits(int number) {
  if (number < 0) {
    printf("-");
    reverse_digits(abs(number));
  }
  else if (number != 0) {
    printf("%d", number % 10);
    reverse_digits(number / 10);
  }
  else {
    printf("\n");
  }
}

// reverse_number() reverses the order of the numbers
// effects: reads input
void reverse_number(void) {
  int n = read_int();
  if (n != READ_INT_FAIL) {
    reverse_number();
    reverse_digits(n);
  }
}

// mean_calc(sum, counter) calculates the mean.
// effects: produces output
void mean_calc(int sum, int counter) {
  if ((sum < 0) && (sum / counter == 0) && ((sum * 100) / counter != 0)) {
    printf("mean = -0.%02d\n", (abs((100 * (sum % counter)) / counter)));
  }
  else {
    printf("mean = %d.%02d\n", sum / counter, 
           (abs((100 * (sum % counter)) / counter)));
  }
}

// statistic_printer() reads in all integers from input and then 
// prints some simple statistics on the integers (the data).
// effects: reads input
//          produces output
void statistic_printer(int counter, int sum, int mean,
                      int min, int max) {
  int n = read_int();
  if (n != READ_INT_FAIL) {
    counter++;
    sum = sum + n;
    if (n < min) {
      min = n;
      statistic_printer(counter, sum, mean, min, max);
    }
    else if (n > max) {
      max = n;
      statistic_printer(counter, sum, mean, min, max);
    }
    else {
      statistic_printer(counter, sum, mean, min, max);
    }
  }
  else {
    printf("count = %d\n", counter);
    printf("sum = %d\n", sum);
    mean_calc(sum, counter);
    printf("min = %d\n", min);
    printf("max = %d\n", max);
  } 
}
    
// statistics() reads in all integers from input and then 
// prints some simple statistics on the integers (the data)
// effects: reads input
void statistics(void) {
  int n = read_int();
  statistic_printer(1, n, n, n, n);
}

// rev_min_printer(min, first_number) reads in all integers from input 
// and finds the minimum value read in. It prints all of the integers except 
// for the first number in reverse order, subtracting the minimum 
// value from each number.
// effects: reads input
//          produces output
int rev_min_printer(int min, int first_number) {
  int n = read_int();
  if (n != READ_INT_FAIL) {
    if (n < min) {
      min = n;
    }
    min = rev_min_printer(min, first_number);
    printf("%d\n", (n - min));
  }
  return min;
}

// rev_min() reads in all integers from input and finds the minimum value 
// read in. It prints all of the integers in reverse order,
// subtracting the minimum value from each number.
// effects: reads input
//          produces output
void rev_min(void) {
  int n = read_int();
  printf("%d\n", (n - rev_min_printer(n, n)));
}
    
int main(void) {
  rev_min();
}

// turning_point_calc(previous_number, counter, previous_state) reads 
// in all integers from input and prints a single integer: 
// the number of turning points in the data.
// effects: reads input
//          produces output
void turning_point_calc(int previous_number, int counter, 
                        int previous_state) {
  int n = read_int();
  if (n != READ_INT_FAIL) {
    if (n == previous_number) {
      turning_point_calc(n, counter, previous_state);
    }
    else if (n > previous_number) {
      if (previous_state != 1) {
        counter++;
        turning_point_calc(n, counter, 1);
      }
      else {
        turning_point_calc(n, counter, 1);
      }
    }
    else {
      if (previous_state != 0) {
        counter++;
        turning_point_calc(n, counter, 0);
      }
      else {
        turning_point_calc(n, counter, 0);
      }
    }
  }
  else {
    printf("%d\n", counter);
  }
}

// start_calc(previous_number, counter, previous_state) reads 
// in all integers from input and prints a single integer: 
// the number of turning points in the data.
// effects: reads input
//          produces output
void start_calc(void) {
  int counter = 0;
  int n = read_int();
  if (n != READ_INT_FAIL) {
    if (n > 0) {
      turning_point_calc(n, 0, 1);
    }
    else if (n < 0) {
       turning_point_calc(n, 0, 0);
    }
    else {
      start_calc();
    }
  }
  else {
    printf("%d\n", counter);
  }
}    