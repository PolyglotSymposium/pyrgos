%{

#include "Value.h"
#include "Parser.h"
#include "Symbol.h"

#include <stdio.h>

static char* strbuf = NULL;

int yyerror(const char *msg) {
  if (strbuf) {
    free(strbuf);
    strbuf = NULL;
  }
  fprintf(stderr, "Lexer: %s\n", msg);
  return 0;
}

%}

%option outfile="Lexer.c" header-file="Lexer.h"
%option warn

%option reentrant noyywrap never-interactive nounistd
%option bison-bridge

%x STRING

%%

[ \r\n\t]*                       { continue; }
[0-9]+                           { sscanf(yytext, "%d", &yylval->value); return TOKEN_NUMBER; }
[a-z\-+/*?!]{1,12}[0-9\-+/*?!]?  { yylval->name = compressSymbol(yytext); return TOKEN_NAME; }
[a-z\-+/*?!]{0,11}_[0-9\-+/*?!]  { yylval->name = compressSymbol(yytext); return TOKEN_NAME; }
"("                              { return TOKEN_LPAREN; }
")"                              { return TOKEN_RPAREN; }
"["                              { return TOKEN_LSQBRK; }
"]"                              { return TOKEN_RSQBRK; }
\"                               { BEGIN STRING; }
<STRING>[^\\"\n]*                { strbuf = strdup(yytext); }
<STRING>\"                       { yylval->string = strbuf; strbuf = NULL;
                                   BEGIN 0; return TOKEN_STRING; }
<STRING>\\                       { yyerror("escape sequences not implemented in strings"); }
<STRING>\n                       { yyerror("string not terminated before EOL"); }
<STRING><<EOF>>                  { yyerror("string not terminated before EOF"); }

%%
