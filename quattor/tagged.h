#ifndef __QUATTOR_TAGGED_POINTERS_H__
#define __QUATTOR_TAGGED_POINTERS_H__

#include <stdbool.h>

void* tag_cstr(void*);
void* tag_pair(void*);
bool is_cstr(const void* const);
bool is_pair(const void* const);
void* untag_cstr(const void* const);
void* untag_pair(const void* const);

void* copy(void*);

#endif//__QUATTOR_TAGGED_POINTERS_H__
