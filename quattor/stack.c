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

void expand_stack(Stack* stack) {
  Stack* new_segment = make_stack();
  ((size_t*)new_segment)[PTR_NEXT] = (size_t)stack;
  stack = new_segment;
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

static void pop_seg_meta_val(Stack* stack) {
  assert(!(seg_meta(stack) & 1));
  ((size_t*)stack)[METADATA_WORD] = ((size_t*)stack)[METADATA_WORD] >> 1;
}

static void pop_seg_meta_ptr(Stack* stack) {
  assert(seg_meta(stack) & 1);
  ((size_t*)stack)[METADATA_WORD] = ((size_t*)stack)[METADATA_WORD] >> 1;
}

static bool stack_segment_full(Stack* stack) {
  size_t length = stack_top(stack) + 1;
  return length >= SIZE + OFFSET;
}

void push_val(Stack* stack, size_t x) {
  if (stack_segment_full(stack)) {
    expand_stack(stack);
  }
  size_t top = stack_top(stack);
  ((size_t*) stack)[top+1] = x;
  set_stack_top(stack, top+1);
  push_seg_meta_val(stack);
}

void push_ptr(Stack* stack, void* x) {
  if (stack_segment_full(stack)) {
    expand_stack(stack);
  }
  size_t top = stack_top(stack);
  ((size_t*) stack)[top+1] = (size_t)x;
  set_stack_top(stack, top+1);
  push_seg_meta_ptr(stack);
  // TODO GC: we have another ref to some memory
}

static bool segment_empty(Stack* stack) {
  return stack_top(stack) < OFFSET;
}

static void shrink_stack(Stack* stack) {
  // TODO we could get thrashing at this point if the code hangs out around
  // the boundary of two segments a lot. :(
  Stack* next = next_segment(stack);
  assert(next != NULL);
  free(stack); // TODO optimization: push to the side instead?
  stack = next;
}

size_t pop_val(Stack* stack) {
  if (segment_empty(stack)) {
    shrink_stack(stack);
  }
  size_t top = stack_top(stack);
  set_stack_top(stack, top-1);
  pop_seg_meta_val(stack);
  return ((size_t*) stack)[top];
}

void* pop_ptr(Stack* stack) {
  if (segment_empty(stack)) {
    shrink_stack(stack);
  }
  size_t top = stack_top(stack);
  set_stack_top(stack, top-1);
  pop_seg_meta_ptr(stack);
  // TODO GC: decrement ref to memory
  return ((void**) stack)[top];
}
