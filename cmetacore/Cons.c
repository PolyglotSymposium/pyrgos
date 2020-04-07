#include "Cons.h"

#include <gc.h>
#include <stdlib.h>

Cons* cons(void* head, Cons* tail) {
  Cons* cons = (Cons*)GC_MALLOC(sizeof(Cons));
  cons->head = head;
  cons->tail = tail;
  return cons;
}
