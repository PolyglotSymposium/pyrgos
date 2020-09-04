#include "stack.h"
#include <assert.h>
#define METADATA_WORD 0
#define TOP_INDEX 1
#define PTR_NEXT 2
// offset = metadata word + top index + pointer to next segment
#define OFFSET 3
#define SIZE 64 // Sorry 32-bit architectures

struct Stack {};

Stack* make_stack() {
  size_t* stack = (size_t*)malloc(sizeof(size_t)*SIZE+OFFSET);
  stack[METADATA_WORD] = 0;
  stack[TOP_INDEX] = OFFSET - 1; // denotes empty
  stack[PTR_NEXT] = (size_t)NULL;
  return (Stack*)stack;
}

static Stack* expand_stack(Stack* stack) {
  Stack* new_segment = make_stack();
  ((Stack**)new_segment)[PTR_NEXT] = stack;
  return new_segment;
}

static Stack* next_segment(Stack* stack) {
  return ((Stack**)stack)[PTR_NEXT];
}

static size_t stack_top(Stack* stack) {
  return ((size_t*)stack)[TOP_INDEX];
}

static void set_stack_top(Stack* stack, size_t x) {
  ((size_t*)stack)[TOP_INDEX] = x;
}

static size_t seg_meta(Stack* stack) {
  return ((size_t*)stack)[METADATA_WORD];
}

static void push_seg_meta_val(Stack* stack) {
  ((size_t*)stack)[METADATA_WORD] = ((size_t*)stack)[METADATA_WORD] << 1;
}

static void push_seg_meta_ptr(Stack* stack) {
  ((size_t*)stack)[METADATA_WORD] = (((size_t*)stack)[METADATA_WORD] << 1) | 1;
}

static bool pop_seg_meta(Stack* stack) {
  size_t meta = seg_meta(stack);
  bool is_ptr = meta & 1;
  ((size_t*)stack)[METADATA_WORD] = meta >> 1;
  return is_ptr;
}

static bool stack_segment_full(Stack* stack) {
  size_t length = stack_top(stack) + 1;
  return length >= SIZE + OFFSET;
}

void push_val(Stack* stack, size_t x) {
  if (stack_segment_full(stack)) {
    stack = expand_stack(stack);
  }
  size_t top = stack_top(stack);
  ((size_t*) stack)[top+1] = x;
  set_stack_top(stack, top+1);
  push_seg_meta_val(stack);
}

void push_ptr(Stack* stack, void* x) {
  if (stack_segment_full(stack)) {
    stack = expand_stack(stack);
  }
  size_t top = stack_top(stack);
  ((size_t*) stack)[top+1] = (size_t)x;
  set_stack_top(stack, top+1);
  push_seg_meta_ptr(stack);
}

static bool segment_empty(Stack* stack) {
  return stack_top(stack) < OFFSET;
}

static Stack* shrink_stack(Stack* stack) {
  // TODO we could get thrashing at this point if the code hangs out around
  // the boundary of two segments a lot. :(
  Stack* next = next_segment(stack);
  assert(next != NULL);
  free(stack); // TODO optimization: push to the side instead?
  return next;
}

bool pop(Stack* stack, void** out) {
  if (segment_empty(stack)) {
    stack = shrink_stack(stack);
  }
  size_t top = stack_top(stack);
  set_stack_top(stack, top-1);
  bool is_ptr = pop_seg_meta(stack);
  *out = ((void**) stack)[top];
  return is_ptr;
}

size_t pop_val(Stack* stack) {
  void* x = NULL;
  bool x_is_ptr = pop(stack, &x);
  assert(!x_is_ptr);
  return (size_t)x;
}

void* pop_ptr(Stack* stack) {
  void* x = NULL;
  bool x_is_ptr = pop(stack, &x);
  assert(x_is_ptr);
  return x;
}
