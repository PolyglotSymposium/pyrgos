#include "pair.h"
#include "tagged.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// TODO confirm size_of is has compacted the two bools
typedef struct Pair {
  bool first_is_ptr;
  bool second_is_ptr;
  void* first;
  void* second;
} Pair;

static Pair* make_pair(bool x_is_ptr, void* x, bool y_is_ptr, void* y) {
  Pair* pair = (Pair*)malloc(sizeof(Pair));
  pair->first_is_ptr = x_is_ptr;
  pair->second_is_ptr = y_is_ptr;
  pair->first = x;
  pair->second = y;
  return tag_pair(pair);
}

void cons(Stack* stack) {
  void* x = NULL;
  bool x_is_ptr = pop(stack, &x);
  void* y = NULL;
  bool y_is_ptr = pop(stack, &y);
  push_ptr(stack, make_pair(x_is_ptr, x, y_is_ptr, y));
}

void uncons(Stack* stack) {
  void* x = pop_ptr(stack);
  assert(is_pair(x));
  Pair* p = (Pair*)untag_pair(x);
  if (p->second_is_ptr) {
    push_ptr(stack, p->second);
  } else {
    push_val(stack, (size_t)p->second);
  }
  if (p->first_is_ptr) {
    push_ptr(stack, p->first);
  } else {
    push_val(stack, (size_t)p->first);
  }
  free(p);
}
