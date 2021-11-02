#ifndef __QUATTOR_TAGGED_POINTERS_H__
#define __QUATTOR_TAGGED_POINTERS_H__

#include <stdbool.h>

void* tag_cstr(void*);
void* tag_pair(void*);
bool is_cstr(const void* const);
bool is_pair(const void* const);
void* untag_cstr(const void* const);
void* untag_pair(const void* const);

void* cstr_copy(const void*);
void cstr_free(void*);

#endif//__QUATTOR_TAGGED_POINTERS_H__
