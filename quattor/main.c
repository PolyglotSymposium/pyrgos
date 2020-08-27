#include <stdio.h>
#include "stack.h"

void* plus(void* x, void* y) {
  return (void*)((size_t)x + (size_t)y);
}

int main(int argc, char* argv[])
{
  Stack* stack = make_stack(254);
  push(stack, 2);
  push(stack, 3);
  apply2(stack, plus);
  fprintf(stdout, "%lu ok\n", pop(stack));
  return 0;
}
