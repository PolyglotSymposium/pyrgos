#ifndef __QUATTOR_VM_H__
#define __QUATTOR_VM_H__

#include <stdlib.h>

typedef struct VM VM;

VM* make_vm();

void qsymbol(VM*, size_t);
void qdup(VM*);
void qdrop(VM*);
void qswap(VM*);
void qover(VM*);

void qadd(VM*);

void qprsym(VM*);

#endif//__QUATTOR_VM_H__
