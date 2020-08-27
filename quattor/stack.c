#include "stack.h"
#include <assert.h>
/* OFFSET = one cell for size + one cell for top */
#define OFFSET 2

struct Stack {};

Stack* make_stack(size_t size) {
  size_t* stack = (size_t*)malloc(sizeof(size_t)*size+OFFSET);
  stack[0] = size;
  stack[1] = OFFSET - 1; // denotes empty
  return (Stack*)stack;
}

static size_t stack_size(Stack* stack) {
  return ((size_t*)stack)[0];
}

static size_t stack_top(Stack* stack) {
  return ((size_t*)stack)[1];
}

static bool stack_not_empty(Stack* stack) {
  return stack_top(stack) >= OFFSET;
}

static size_t stack_top_decr(Stack* stack) {
  assert(stack_not_empty(stack));
  size_t old = stack_top(stack);
  ((size_t*)stack)[1] = old - 1;
  return old;
}

static size_t stack_top_incr(Stack* stack) {
  assert(stack_top(stack) <= stack_size(stack));
  size_t old = stack_top(stack);
  ((size_t*)stack)[1] = old + 1;
  return old;
}

size_t pop(Stack* stack) {
  size_t top = stack_top_decr(stack);
  return ((size_t*) stack)[top];
}

void push(Stack* stack, size_t x) {
  size_t top = stack_top_incr(stack);
  ((size_t*) stack)[top+1] = x;
}

void dup(Stack* stack) {
  assert(stack_not_empty(stack));
  size_t top = stack_top_incr(stack);
  ((size_t*) stack)[top+1] = ((size_t*) stack)[top];
}

void drop(Stack* stack) {
  stack_top_decr(stack);
}

void swap(Stack* stack) {
  assert(stack_size(stack) > 1);
  size_t top = stack_top(stack);
  size_t a = ((size_t*) stack)[top-1];
  size_t b = ((size_t*) stack)[top];
  ((size_t*) stack)[top] = a;
  ((size_t*) stack)[top-1] = b;
}

void over(Stack* stack) {
  assert(stack_size(stack) > 1);
  size_t top = stack_top_incr(stack);
  size_t a = ((size_t*) stack)[top-1];
  size_t b = ((size_t*) stack)[top];
  ((size_t*) stack)[top+1] = a;
  ((size_t*) stack)[top] = b;
  ((size_t*) stack)[top-1] = a;
}

void apply1(Stack* stack, void* (*f) (void*)) {
  assert(stack_not_empty(stack));
  size_t top = stack_top(stack);
  ((size_t*) stack)[top] = (size_t)f(((void**) stack)[top]);
}

void apply2(Stack* stack, void* (*f) (void*, void*)) {
  assert(stack_size(stack) > 1);
  size_t top = stack_top_decr(stack);
  ((size_t*) stack)[top-1] = (size_t)f(((void**) stack)[top], ((void**) stack)[top-1]);
}

//void apply3(Stack*, void* (*f) (void*, void*, void*)) {
//  assert(stack_size(stack) > 2);
//  //
//}
