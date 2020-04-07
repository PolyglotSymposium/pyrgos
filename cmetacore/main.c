#include "Expr.h"
#include "Eval.h"
#include "Parser.h"
#include "Lexer.h"
#include <assert.h>

int yyparse(SExpr** expr, yyscan_t scanner);

SExpr* getAST(const char *code)
{
  SExpr* expr = NULL;
  yyscan_t scanner = 0;
  YY_BUFFER_STATE state = 0;
  if (yylex_init(&scanner)) return NULL;
  state = yy_scan_string(code, scanner);
  if (yyparse(&expr, scanner)) return NULL;
  yy_delete_buffer(state, scanner);
  yylex_destroy(scanner);
  return expr;
}

int main(void)
{
  char* code = NULL;
  size_t len = 0;
  getline(&code, &len, stdin);
  assert(code != NULL);
  SExpr* e = getAST(code);
  free(code);
  code = NULL;
  assert(e != NULL);
  Value* value = eval(e);
  assert(value != NULL);
  printValue(stdout, value);
  return 0;
}
