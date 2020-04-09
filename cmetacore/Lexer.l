%{

#include "Expr.h"
#include "Parser.h"

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

[ \r\n\t]*         { continue; }
[0-9]+             { sscanf(yytext, "%d", &yylval->value); return TOKEN_NUMBER; }
"*"                { yylval->func = fMULT; return TOKEN_FUNC; }
"+"                { yylval->func = fADD; return TOKEN_FUNC; }
"k"                { yylval->func = fKCOMB; return TOKEN_FUNC; }
"i"                { yylval->func = fICOMB; return TOKEN_FUNC; }
"b"                { yylval->func = fBCOMB; return TOKEN_FUNC; }
"("                { return TOKEN_LPAREN; }
")"                { return TOKEN_RPAREN; }
\"                 { BEGIN STRING; }
<STRING>[^\\"\n]*  { strbuf = strdup(yytext); }
<STRING>\"         { yylval->string = strbuf; strbuf = NULL;
                     BEGIN 0; return TOKEN_STRING; }
<STRING>\\         { yyerror("escape sequences not implemented in strings"); }
<STRING>\n         { yyerror("string not terminated before EOL"); }
<STRING><<EOF>>    { yyerror("string not terminated before EOF"); }

%%
