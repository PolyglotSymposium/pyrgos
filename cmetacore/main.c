#include "Expr.h"
#include "Eval.h"
#include "Parser.h"
#include "Lexer.h"
#include <assert.h>

int yyparse(Expr** expr, yyscan_t scanner);

Expr* getAST(const char *code)
{
  Expr* expr = NULL;
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
    getline(&code, &len, stdin);
    assert(code != NULL);
  }
  Expr* e = getAST(code);
  code = NULL;
  assert(e != NULL);
  Value* value = eval(e);
  assert(value != NULL);
  printValue(stdout, value);
  return 0;
}
