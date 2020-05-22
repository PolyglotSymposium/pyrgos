#include "Struct.h"
#include <assert.h>
#include <gc.h>

struct Struct {
  Symbol tag;
  size_t size;
  void** fields;
};

Struct* new_struct(Symbol tag, size_t size, void** fields) {
  Struct* x = (Struct*)GC_MALLOC(sizeof(Struct));
  x->tag = tag;
  x->size = size;
  x->fields = fields;
  return x;
}

Struct* singleton_struct(Symbol tag, void* x) {
  return new_struct(tag, 1, (void**)x);
}

Struct* value_struct(Symbol tag, unsigned long x) {
  return new_struct(tag, 1, (void**)x);
}

Struct* atomic_struct(Symbol tag) {
  return new_struct(tag, 0, NULL);
}

void* get_field(Struct* s, size_t n) {
  assert(s != NULL);
  void* x = NULL;
  if (s->size > n) {
    x = s->fields[n];
  }
  return x;
}

Symbol get_tag(Struct* s) {
  assert(s != NULL);
  return s->tag;
}

size_t get_size(Struct* s) {
  assert(s != NULL);
  return s->size;
}

void* singleton_payload(Struct* s) {
  assert(s != NULL);
  return (void*)s->fields;
}

unsigned long value_payload(Struct* s) {
  assert(s != NULL);
  return (unsigned long)s->fields;
}
