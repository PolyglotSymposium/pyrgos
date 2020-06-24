#include "Struct.h"
#include "Parser.h"
#include "Lexer.h"

int yyparse(Struct** expr, yyscan_t scanner);

Struct* chars_to_struct(char* code)
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
