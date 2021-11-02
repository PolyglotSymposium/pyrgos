#include "pair.h"
#include "tagged.h"
#include <stdlib.h>
#include <string.h>

void* copy(void*);

// TODO confirm size_of is has compacted the two bools
struct Pair {
  bool first_is_ptr;
  bool second_is_ptr;
  void* first;
  void* second;
};

Pair* make_pair(bool x_is_ptr, void* x, bool y_is_ptr, void* y) {
  Pair* pair = (Pair*)malloc(sizeof(Pair));
  pair->first_is_ptr = x_is_ptr;
  pair->second_is_ptr = y_is_ptr;
  pair->first = x;
  pair->second = y;
  return tag_pair(pair);
}

Pair* copy_pair(Pair* pair) {
  Pair* src = untag_pair(pair);
  Pair* dst = (Pair*)malloc(sizeof(Pair));
  memcpy(dst, src, sizeof(Pair));
  return tag_pair(dst);
}
