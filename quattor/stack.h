#ifndef __QUATTOR_STACK_H__
#define __QUATTOR_STACK_H__

#include <stdlib.h>

typedef struct Stack Stack;

Stack* make_stack();

void push_val(Stack*, size_t);
void push_ptr(Stack*, void*);
bool pop(Stack*, void*);
size_t pop_val(Stack*);
void* pop_ptr(Stack*);

#endif//__QUATTOR_STACK_H__
