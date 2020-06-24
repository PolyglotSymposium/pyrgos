#include "Primitives.h"
#include "Eval.h"
#include "StructParser.h"
#include <assert.h>

int main(int argc, char* argv[])
{
  char* code = NULL;
  if (argc == 2) {
    code = argv[1];
  } else {
    size_t len = 0;
    getline(&code, &len, stdin); assert(code != NULL);
  }
  Struct* e = chars_to_struct(code);
  if (e == NULL) {
    return 1;
  } else {
    code = NULL;
    e = eval(e); assert(e != NULL);
    printValue(stdout, e);
    return 0;
  }
}
