#include "pair.h"
#include <stdlib.h>

void* copy(void*);

struct Pair {
  void* first;
  void* second;
};

static Pair* tag_pair_ptr(Pair* pair, bool x_is_ptr, bool y_is_ptr) {
  return pair; // TODO untag
}

static Pair* copy_pair_tag(Pair* dst, Pair* src) {
  return pair; // TODO untag
}

Pair* make_pair(bool x_is_ptr, void* x, bool y_is_ptr, void* y) {
  Pair* pair = (Pair*)malloc(sizeof(Pair));
  pair->first = x;
  pair->second = y;
  return tag_pair_ptr(pair, x_is_ptr, y_is_ptr);
}

Pair* pair_ptr(Pair* pair) {
  return pair; // TODO untag
}

Pair* copy_pair(Pair* pair) {
  Pair* src = pair_ptr(pair);
  Pair* dst = (Pair*)malloc(sizeof(Pair));
  memcpy(dst, src, sizeof(Pair));
  return tag_pair_ptr(dst, pair);
}
