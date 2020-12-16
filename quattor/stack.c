#include "stack.h"
#include <assert.h>
#define METADATA_WORD 0
#define PTR_NEXT 1
// offset = metadata word + pointer to next segment
#define OFFSET 2
#define SEGMENT_EMPTY (OFFSET-1)
#define SIZE 64 // Sorry 32-bit architectures
#define SEGMENT_FULL (SEGMENT_EMPTY + SIZE)

struct Stack {
  size_t* extra;
  size_t top;
  size_t* segment;
};

static size_t* make_segment() {
  size_t* segment = (size_t*)malloc(sizeof(size_t)*SIZE+OFFSET);
  segment[METADATA_WORD] = 0;
  segment[PTR_NEXT] = (size_t)NULL;
  return segment;
}

Stack* make_stack() {
  Stack* stack = (Stack*)malloc(sizeof(Stack));
  stack->extra = NULL;
  stack->top = SEGMENT_EMPTY;
  stack->segment = make_segment();
  return stack;
}

static Stack expand_stack(Stack stack) {
  size_t* new_segment = NULL;
  if (stack.extra == NULL) {
    new_segment = make_segment();
  } else {
    assert((size_t*)stack.extra[PTR_NEXT] == NULL);
    new_segment = stack.extra;
  }
  new_segment[PTR_NEXT] = (size_t)stack.segment;
  Stack new_stack = {
    .extra = NULL,
    .top = SEGMENT_EMPTY,
    .segment = new_segment
  };
  return new_stack;
}

void push_val(Stack* stack, size_t x) {
  if (stack->top >= SEGMENT_FULL) {
    *stack = expand_stack(*stack);
  }
  stack->segment[stack->top+1] = x;
  stack->top = stack->top + 1;
  stack->segment[METADATA_WORD] = stack->segment[METADATA_WORD] << 1;
}

void push_ptr(Stack* stack, void* x) {
  if (stack->top >= SEGMENT_FULL) {
    *stack = expand_stack(*stack);
  }
  stack->segment[stack->top+1] = (size_t)x;
  stack->top = stack->top + 1;
  stack->segment[METADATA_WORD] = (stack->segment[METADATA_WORD] << 1) | 1;
}

static Stack shrink_stack(Stack stack) {
  size_t* next = (size_t*)stack.segment[PTR_NEXT];
  assert(next != NULL);
  if (stack.extra == NULL) {
    stack.segment[PTR_NEXT] = (size_t)NULL;
    stack.extra = stack.segment;
  } else {
    free(stack.segment);
  }
  stack.top = SEGMENT_FULL;
  stack.segment = next;
  return stack;
}

bool peek(Stack* stack, void** out) {
  if (stack->top == SEGMENT_EMPTY) {
    *stack = shrink_stack(*stack);
  }
  *out = (void*)stack->segment[stack->top];
  size_t meta = stack->segment[METADATA_WORD];
  bool is_ptr = meta & 1;
  return is_ptr;
}

bool pop(Stack* stack, void** out) {
  if (stack->top == SEGMENT_EMPTY) {
    *stack = shrink_stack(*stack);
  }
  *out = (void*)stack->segment[stack->top];
  stack->top = stack->top - 1;
  size_t meta = stack->segment[METADATA_WORD];
  bool is_ptr = meta & 1;
  stack->segment[METADATA_WORD] = meta >> 1;
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

size_t peek_val(Stack* stack) {
  void* x = NULL;
  bool x_is_ptr = peek(stack, &x);
  assert(!x_is_ptr);
  return (size_t)x;
}

const void* const peek_ptr(Stack* stack) {
  void* x = NULL;
  bool x_is_ptr = peek(stack, &x);
  assert(x_is_ptr);
  return x;
}
