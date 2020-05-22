#include <assert.h>
#include "SymbolValue.h"
#include "Forms.h"
#include "Error.h"

static Struct* quoteForm(Struct* form) {
  Struct* x = NULL;
  if (get_size(form) == 1) {
    x = (Struct*)get_field(form, 0);
  } else {
    x = malformed(get_tag(form));
  }
  return x;
}

static Struct* constructForm(Struct* form) {
  Struct* x = NULL;
  size_t size = get_size(form);
  if (size > 0) {
    Struct** payload = (Struct**)get_fields(form);
    Struct* shouldBeTag = payload[0];
    if (get_tag(shouldBeTag) == SYMBOL_SYMBOL) {
      Symbol tag = asSymbol(shouldBeTag);
      switch (size) {
      case 1 : x = atomic_struct(tag)                      ; break;
      case 2 : x = singleton_struct(tag, (void*)payload[1]); break;
      default: x = new_struct(tag, size-1, (void**)payload+1)      ; break;
      }
    }
  }
  if (x == NULL) {
    x = malformed(get_tag(form));
  }
  return x;
}

Struct* matchForm(Struct* form) {
  Struct* x = NULL;
  Symbol tag = get_tag(form);
  switch (tag) {
  case 4831888        /* quote     */: x = quoteForm(form)    ; break;
  case 20981506192834 /* construct */: x = constructForm(form); break;
  default                            : x = noSuchForm(tag)    ; break;
  }
  return x;
}
