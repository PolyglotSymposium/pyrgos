#include <stdio.h>
#include "vm.h"
#include <unistd.h>
#include <string.h>

int main(int argc, char* argv[])
{
  VM* vm = make_vm();
  char buffer[32];
  char* end = NULL;
  while (read(0, &buffer, 32) > 0) {
    if (strncmp(buffer, "quit", 4) == 0) {
      break;
    }
    if (strncmp(buffer, "dup", 3) == 0) {
      qdup(vm);
    } else if (strncmp(buffer, "drop", 4) == 0) {
      qdrop(vm);
    } else if (strncmp(buffer, "swap", 4) == 0) {
      qswap(vm);
    } else if (strncmp(buffer, "over", 4) == 0) {
      qover(vm);
    } else if (strncmp(buffer, "add", 3) == 0) {
      qadd(vm);
    } else if (strncmp(buffer, "prsym", 5) == 0) {
      qprsym(vm);
    } else {
      qsymbol(vm, strtoul(buffer, &end, 10));
    }
  }
  return 0;
}
