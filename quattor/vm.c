#include "vm.h"
#include "stack.h"
#include "pair.h"
#include "tagged.h"
#include <stdio.h>
#include <string.h>

struct VM {
  Stack* stack;
};

VM* make_vm() {
  VM* vm = (VM*)malloc(sizeof(VM));
  vm->stack = make_stack();
  return vm;
}

void qsym(VM* vm, size_t x) {
  push_val(vm->stack, x);
}

void qstr(VM* vm, const char* const x) {
  char* s = (char*)malloc(28);
  strcpy(s, x);
  push_ptr(vm->stack, tag_cstr(s));
}

void qstrdup(VM* vm) {
  const void* x = peek_ptr(vm->stack);
  push_ptr(vm->stack, cstr_copy(x));
}

void qsymdup(VM* vm) {
  size_t x = peek_val(vm->stack);
  push_val(vm->stack, x);
}

void qstrdrp(VM* vm) {
  void* x = pop_ptr(vm->stack);
  cstr_free(x);
}

void qsymdrp(VM* vm) {
  pop_val(vm->stack);
}

void qswap(VM* vm) {
  void* x = NULL;
  bool x_is_ptr = pop(vm->stack, &x);
  void* y = NULL;
  bool y_is_ptr = pop(vm->stack, &y);
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

void qstrovr(VM* vm) {
  void* x = pop_ptr(vm->stack);
  void* y = NULL;
  bool y_is_ptr = pop(vm->stack, &y);
  push_ptr(vm->stack, cstr_copy(x));
  if (y_is_ptr) {
    push_ptr(vm->stack, y);
  } else {
    push_val(vm->stack, (size_t)y);
  }
  push_ptr(vm->stack, x);
}

void qsymovr(VM* vm) {
  size_t x = pop_val(vm->stack);
  void *y = NULL;
  bool y_is_ptr = pop(vm->stack, &y);
  push_val(vm->stack, x);
  if (y_is_ptr) {
    push_ptr(vm->stack, y);
  } else {
    push_val(vm->stack, (size_t)y);
  }
  push_val(vm->stack, x);
}

void qadd(VM* vm) {
  size_t x = pop_val(vm->stack);
  size_t y = pop_val(vm->stack);
  push_val(vm->stack, x + y);
}

void qcons(VM *vm) { cons(vm->stack); }

void quncons(VM* vm) { uncons(vm->stack); }

void qcat(VM* vm) {
  void* x = pop_ptr(vm->stack);
  const void* y = peek_ptr(vm->stack);
  push_ptr(vm->stack, tag_cstr(strcat(untag_cstr(x), untag_cstr(y))));
}

void qprsym(VM* vm) {
  fprintf(stdout, "%lu\n", peek_val(vm->stack));
}

void qprchr(VM* vm) {
  fprintf(stdout, "%c\n", (int)peek_val(vm->stack));
}

void qprstr(VM* vm) {
  fprintf(stdout, "%s\n", untag_cstr(peek_ptr(vm->stack)));
}
