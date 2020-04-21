#ifndef CONS_H
#define CONS_H

#include <stddef.h>

typedef struct Cons {
  void* head;
  Cons* tail;
} Cons;

Cons* cons(void*, Cons*);
const size_t length(const Cons* const);

#endif // CONS_H
