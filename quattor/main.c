#include <stdio.h>
#include "stack.h"

void plus(Stack* stack) {
  size_t x = pop_val(stack);
  size_t y = pop_val(stack);
  push_val(stack, x + y);
}

int main(int argc, char* argv[])
{
  Stack* stack = make_stack();
  push_val(stack, 1337);
  push_val(stack, 1111);
  plus(stack);
  fprintf(stdout, "%lu ok\n", pop_val(stack));
  return 0;
}
