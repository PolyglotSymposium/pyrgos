%token <Syntax.symbol> SYMBOL
%token <int> NUMBER
%token LBRACE RBRACE
%token LBRACK RBRACK
%token LPAREN RPAREN
%token LAMBDA APPLY CONS UNCONS NIL ISNIL CASE QUOTE EVAL SUGARQUOTE
%token EOF
%type <Syntax.toplvl> toplvl
%start toplvl

%%

toplvl:
| SYMBOL sxp EOF { Syntax.Define ($1, $2) }
| sxp EOF { Syntax.Expr $1 }
| EOF { Syntax.Blank }

sxp:
| SYMBOL { Syntax.Symbol $1 }
| NUMBER { Syntax.Integer $1 }
| LBRACE listish RBRACE { $2 }
| LBRACK RBRACK { Syntax.Nil }
| LPAREN app RPAREN { $2 }
| LBRACK cons RBRACK { $2 }
| SUGARQUOTE sxp { Syntax.Quote $2 }

listish:
| NIL { Syntax.Nil }
| ISNIL sxp sxp sxp { Syntax.IsNil ($2, $3, $4) }
| APPLY sxp sxp { Syntax.Appl ($2, $3) }
| LAMBDA SYMBOL sxp { Syntax.Lambda ($2, $3) }
| CONS sxp sxp { Syntax.Cons ($2, $3) }
| UNCONS sxp sxp sxp { Syntax.Uncons ($2, $3, $4) }
| CASE sxp NUMBER sxp sxp { Syntax.Case ($2, $3, $4, $5) }
| QUOTE sxp { Syntax.Quote $2 }
| EVAL sxp { Syntax.Eval $2 }

app:
| sxp sxp { Syntax.Appl ($1, $2) }
| app sxp { Syntax.Appl ($1, $2) }

cons:
| sxp cons { Syntax.Cons ($1, $2) }
| sxp { Syntax.Cons ($1, Syntax.Nil) }
