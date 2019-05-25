%token <Syntax.symbol> SYMBOL
%token <int> NUMBER
%token LSQBR RSQBR
%token LAMBDA APPLY CONS UNCONS NIL ISNIL CASE QUOTE EVAL
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
| LSQBR listish RSQBR { $2 }

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
