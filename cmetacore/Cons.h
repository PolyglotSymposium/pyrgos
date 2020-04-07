#ifndef CONS_H
#define CONS_H

typedef struct Cons {
  void* head;
  Cons* tail;
} Cons;

Cons* cons(void*, Cons*);

#endif // CONS_H
