#include "tagged.h"
#include "pair.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

static const size_t CSTR_TAG = 0UL;
static const size_t PAIR_TAG = 1UL;

static const size_t MASK = ~0x07UL;
static const size_t UNMASK = 0x07UL;

static void* tag(void* p, size_t tag) {
  return (void*)(((size_t)p & MASK) | tag);
}

void* tag_cstr(void* p) {
  return tag(p, CSTR_TAG);
}

void* tag_pair(void* p) {
  return tag(p, PAIR_TAG);
}

static bool has_tag(const void* const p, size_t tag) {
  return ((size_t)p & UNMASK) == tag;
}

bool is_cstr(const void* const p) {
  return has_tag(p, CSTR_TAG);
}

bool is_pair(const void* const p) {
  return has_tag(p, PAIR_TAG);
}

static void* untag(const void* const p) {
  return (void*)((size_t)p & MASK);
}

void* untag_cstr(const void* const p) {
  assert(is_cstr(p));
  return untag(p);
}

void* untag_pair(const void* const p) {
  assert(is_pair(p));
  return untag(p);
}

void* cstr_copy(const void* p) {
  void* out = NULL;
  switch ((size_t)p & UNMASK) {
  case CSTR_TAG: out = strdup(untag(p)); break;
  default:
    {
      size_t NOT_STR_POINTER = 1;
      assert(NOT_STR_POINTER);
    }
  }
  return out;
}

void cstr_free(void* p) {
  switch ((size_t)p & UNMASK) {
  case CSTR_TAG: free(untag(p)); break;
  default:
    {
      size_t NOT_STR_POINTER = 1;
      assert(NOT_STR_POINTER);
    }
  }
}
