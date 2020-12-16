#ifndef __QUATTOR_VM_H__
#define __QUATTOR_VM_H__

#include <stdlib.h>

typedef struct VM VM;

VM* make_vm();

void qsym(VM*, size_t);
void qstr(VM*, const char* const);

void qdup(VM*);
void qdrop(VM*);
void qswap(VM*);
void qover(VM*);

void qadd(VM*);
void qcat(VM*);

void qprsym(VM*);
void qprchr(VM*);
void qprstr(VM*);

#endif//__QUATTOR_VM_H__
