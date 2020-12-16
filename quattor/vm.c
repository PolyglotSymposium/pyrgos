#include "vm.h"
#include "stack.h"
#include <stdio.h>

struct VM {
  Stack* stack;
};

VM* make_vm() {
  VM* vm = (VM*)malloc(sizeof(VM));
  vm->stack = make_stack();
  return vm;
}

void qsymbol(VM* vm, size_t x) {
  push_val(vm->stack, x);
}

void qdup(VM* vm) {
  void* x = NULL;
  bool is_ptr = pop(vm->stack, x);
  if (is_ptr) {
    push_ptr(vm->stack, x);
    // TODO duplicate
    push_ptr(vm->stack, x);
  } else {
    push_val(vm->stack, (size_t)x);
    push_val(vm->stack, (size_t)x);
  }
}

void qdrop(VM* vm) {
  void* x = NULL;
  bool is_ptr = pop(vm->stack, x);
  if (is_ptr) {
    // TODO deallocate
  }
}

void qswap(VM* vm) {
  void* x = NULL;
  bool x_is_ptr = pop(vm->stack, x);
  void* y = NULL;
  bool y_is_ptr = pop(vm->stack, y);
  if (x_is_ptr) {
    push_ptr(vm->stack, x);
  } else {
    push_val(vm->stack, (size_t)x);
  }
  if (y_is_ptr) {
    push_ptr(vm->stack, y);
  } else {
    push_val(vm->stack, (size_t)y);
  }
}

void qover(VM* vm) {
  void* x = NULL;
  bool x_is_ptr = pop(vm->stack, x);
  void* y = NULL;
  bool y_is_ptr = pop(vm->stack, y);
  if (x_is_ptr) {
    push_ptr(vm->stack, x);
  } else {
    push_val(vm->stack, (size_t)x);
  }
  if (y_is_ptr) {
    push_ptr(vm->stack, y);
  } else {
    push_val(vm->stack, (size_t)y);
  }
  if (x_is_ptr) {
    push_ptr(vm->stack, x);
    // TODO duplicate
  } else {
    push_val(vm->stack, (size_t)x);
  }
}

void qadd(VM* vm) {
  size_t x = pop_val(vm->stack);
  size_t y = pop_val(vm->stack);
  push_val(vm->stack, x + y);
}

void qprsym(VM* vm) {
  fprintf(stdout, "%lu\n", pop_val(vm->stack));
}
