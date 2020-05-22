#ifndef __METACORE_STRUCT_H__
#define __METACORE_STRUCT_H__

#include <stddef.h>
#include "Symbol.h"

typedef struct Struct Struct;

Struct* new_struct(Symbol, size_t, void**);
Struct* singleton_struct(Symbol, void*);
Struct* value_struct(Symbol, unsigned long);
Struct* atomic_struct(Symbol);
void* get_field(Struct*, size_t);
Symbol get_tag(Struct*);
size_t get_size(Struct*);
void* singleton_payload(Struct*);
unsigned long value_payload(Struct*);

#endif
