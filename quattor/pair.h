#ifndef __QUATTOR_PAIR_H__
#define __QUATTOR_PAIR_H__

#include <stdbool.h>

typedef struct Pair Pair;

Pair* make_pair(bool, void*, bool, void*);
Pair* copy_pair(Pair*);

#endif//__QUATTOR_PAIR_H__
