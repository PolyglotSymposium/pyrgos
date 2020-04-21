#include "Cons.h"

#include <gc.h>
#include <stdlib.h>

Cons* cons(void* head, Cons* tail) {
  Cons* cons = (Cons*)GC_MALLOC(sizeof(Cons));
  cons->head = head;
  cons->tail = tail;
  return cons;
}

const size_t length(const Cons* const cons) {
  size_t x = 0;
  const Cons* c = cons;
  while (c != NULL) {
    x++;
    c = c->tail;
  }
  return x;
}
