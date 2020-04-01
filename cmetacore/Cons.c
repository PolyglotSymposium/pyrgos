#include "Cons.h"
#include <stdlib.h>

Cons* cons(void* head, Cons* tail) {
  Cons* cons = (Cons*)malloc(sizeof(Cons));
  cons->head = head;
  cons->tail = tail;
  return cons;
}

void recDelConsWith(void delNode(void*), Cons* cons) {
  if (cons == NULL) return;
  delNode(cons->head);
  recDelConsWith(delNode, cons->tail);
  free(cons);
  cons = NULL;
}
