#ifndef __QUATTOR_STACK_H__
#define __QUATTOR_STACK_H__

#include <stdlib.h>

typedef struct Stack Stack;

Stack* make_stack(size_t);

size_t pop(Stack*);
void push(Stack*, size_t);
void dup(Stack*);
void drop(Stack*);
void swap(Stack*);
void over(Stack*);
void apply1(Stack*, void* (*) (void*));
void apply2(Stack*, void* (*) (void*, void*));
//void apply3(Stack*, void* (*) (void*, void*, void*));

#endif//__QUATTOR_STACK_H__
