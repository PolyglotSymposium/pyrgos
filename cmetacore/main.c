#include "Primitives.h"
#include "Eval.h"
#include "Parser.h"
#include "Lexer.h"
#include <assert.h>

int yyparse(Struct** expr, yyscan_t scanner);

Struct* getAST(const char *code)
{
  Struct* expr = NULL;
  yyscan_t scanner = 0;
  YY_BUFFER_STATE state = 0;
  if (yylex_init(&scanner)) return NULL;
  state = yy_scan_string(code, scanner);
  if (yyparse(&expr, scanner)) return NULL;
  yy_delete_buffer(state, scanner);
  yylex_destroy(scanner);
  return expr;
}

int main(int argc, char* argv[])
{
  char* code = NULL;
  if (argc == 2) {
    code = argv[1];
  } else {
    size_t len = 0;
    getline(&code, &len, stdin); assert(code != NULL);
  }
  Struct* e = getAST(code); assert(e != NULL);
  code = NULL;
  e = eval(e); assert(e != NULL);
  printValue(stdout, e);
  return 0;
}
