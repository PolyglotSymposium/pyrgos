#ifndef __QUATTOR_VM_H__
#define __QUATTOR_VM_H__

typedef struct VM VM;

VM* make_vm();

void symbol(VM*, size_t);
void string(VM*, char*);
void dup(VM*);
void drop(VM*);
void swap(VM*);
void over(VM*);

void add(VM*);
void string_length(VM*);

#endif//__QUATTOR_VM_H__
