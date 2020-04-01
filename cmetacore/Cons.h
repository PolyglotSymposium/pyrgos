#ifndef CONS_H
#define CONS_H

typedef struct Cons {
  void* head;
  Cons* tail;
} Cons;

Cons* cons(void*, Cons*);

void recDelConsWith(void(*)(void*), Cons*);

#endif // CONS_H
