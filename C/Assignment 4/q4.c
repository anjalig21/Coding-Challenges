// A

#include "cs136-trace.h"
#include "stack.h"
#include <stdio.h>

// This program reads in all characters from input until EOF occurs, 
// and then prints out all of the characters in reverse order.
int main(void) {
  struct stack *s = stack_create();
  char n = '0';
  while (scanf("%c", &n) == 1) {
    stack_push(n, s);
  }
  while (!stack_is_empty(s)) {
    printf("%c", stack_pop(s));
  } 
  stack_destroy(s);
}


// B

// This program reads in all integers from input and prints them 
// out in their original order and then in reverse order and then 
// in their original order, and finally one more time in their 
// reverse order (4 times in total). 
int main(void) {
  struct stack *reverse = stack_create();
  struct stack *original = stack_create();
  int n = 0;
  while (scanf("%d", &n) == 1) {
    printf("%d\n", n);
    stack_push(n, reverse);
  }
  while (!stack_is_empty(reverse)) {
    printf("%d\n", stack_top(reverse));
    stack_push(stack_pop(reverse), original);
  }
  while (!stack_is_empty(original)) {
    printf("%d\n", stack_top(original));
    stack_push(stack_pop(original), reverse);
  }
  while (!stack_is_empty(reverse)) {
    printf("%d\n", stack_top(reverse));
    stack_push(stack_pop(reverse), original);
  }
  stack_destroy(original);
  stack_destroy(reverse);
}


// C

// This program reads in all characters from input and prints 
// either "balanced\n" or "unbalanced\n" with respect to 
// whether the brackets in the input are balanced or not.
int main(void) {
  struct stack *brackets = stack_create();
  char n = '0';
  while (scanf("%c", &n) == 1) {
    if (n == '(' || n == '{' || n == '[' || n == '<') {
      stack_push(n, brackets);
    }
    else if (n == ')') {
      if (stack_top(brackets) == '(') {
        stack_pop(brackets);
      }
      else {
        printf("unbalanced\n");
        break;
      }
    }
    else if (n == '}') {
      if (stack_top(brackets) == '{') {
        stack_pop(brackets);
      }
      else {
        printf("unbalanced\n");
        break;
      }
    }
    else if (n == ']') {
      if (stack_top(brackets) == '[') {
        stack_pop(brackets);
      }
      else {
        printf("unbalanced\n");
        break;
      }
    }
    else if (n == '>') {
      if (stack_top(brackets) == '<') {
        stack_pop(brackets);
      }
      else {
        printf("unbalanced\n");
        break;
      }
    }
  }
  if (stack_is_empty(brackets)) {
    printf("balanced\n");
  }
  stack_destroy(brackets);
}

