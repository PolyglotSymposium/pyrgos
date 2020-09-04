#include "vm.h"
#include "stack.h"

struct GC {};

static GC* make_gc() {
  return NULL; // TODO
}

static void deregister_ptr(GC*, void*) {
  return NULL; // TODO
}

static void register_ptr(GC*, void*) {
  return NULL; // TODO
}

struct VM {
  Stack* stack;
  GC* gc;
};

VM* make_vm() {
  VM* vm = (VM*)malloc(sizeof(VM));
  vm->stack = make_stack();
  vm->gc = make_gc();
  return vm;
}

void symbol(VM* vm, size_t x) {
  push_val(vm->stack, x);
}

void string(VM* vm, char* s) {
  push_ptr(vm->stack, s);
  register_ptr(vm->gc, s);
}

void dup(VM* vm) {
  void* x = NULL;
  bool is_ptr = pop(vm->stack, x);
  if (is_ptr) {
    push_ptr(vm->stack, x);
    push_ptr(vm->stack, x);
    register_ptr(vm->gc, x);
  } else {
    push_val(vm->stack, (size_t)x);
    push_val(vm->stack, (size_t)x);
  }
}

void drop(VM* vm) {
  void* x = NULL;
  bool is_ptr = pop(vm->stack, x);
  if (is_ptr) {
    deregister_ptr(vm->gc, x);
  }
}

void swap(VM* vm) {
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

void over(VM* vm) {
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
    register_ptr(vm->gc, x);
  } else {
    push_val(vm->stack, (size_t)x);
  }
}

void add(VM* vm) {
  size_t x = pop_val(vm->stack);
  size_t y = pop_val(vm->stack);
  push_val(vm->stack, x + y);
}

void string_length(VM* vm) {
  void* x = pop_ptr(vm->stack);
  size_t len = (size_t)x; // TODO actually measure length
  push_val(vm->stack, len);
  deregister_ptr(x);
}
