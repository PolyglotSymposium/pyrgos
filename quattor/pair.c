#include "pair.h"
#include <stdlib.h>

void* copy(void*);

struct Pair {
  void* first;
  void* second;
};

Pair* make_pair(bool x_is_ptr, void* x, bool y_is_ptr, void* y) {
  Pair* pair = (Pair*)malloc(sizeof(Pair));
  pair->first = x;
  pair->second = y;
  // TODO tag pair with x_is_ptr and y_is_ptr
  return pair; // TODO
}

void* pair_ptr(Pair* pair) {
  return pair; // TODO untag
}

Pair* copy_pair(Pair* pair) {
  // TODO untag pair
  // TODO memcpy pair
  // TODO tag new pair
  return pair; // TODO new pair
}
