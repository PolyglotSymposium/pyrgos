%token <Syntax.symbol> ATOM
%token <Syntax.symbol> SYMBOL
%token LPAREN RPAREN
%token QUOTE
%token ARROW FATARROW
%token DOLLAR
%token LSQUARE RSQUARE
%token LBRACE RBRACE
%token EOF
%type <Syntax.toplvl> toplvl
%start toplvl

%%

toplvl:
| DOLLAR sxp EOF { Syntax.Up $2 }
| sxp EOF { Syntax.Expr $1 }
| EOF { Syntax.Expr Prelude.unit }

sxp:
| LBRACE txp RBRACE { Syntax.TExpr $2 }
| ATOM { Syntax.Atom $1 }
| SYMBOL { Syntax.Symbol $1 }
| LPAREN RPAREN { Prelude.unit }
| LPAREN func_body RPAREN { $2 }
| LPAREN appl_body RPAREN { $2 }
| QUOTE sxp { Syntax.Quote $2 }
| LSQUARE RSQUARE { Prelude.nil }
| LSQUARE cons RSQUARE { $2 }

cons:
| sxp { Prelude.consCtr $1 Prelude.nil }
| sxp cons { Prelude.consCtr $1 $2 }

txp:
| SYMBOL { Syntax.TVar $1 }
| SYMBOL ARROW txp { Syntax.Func (Syntax.TVar $1, $3) }
| LPAREN txp RPAREN ARROW txp { Syntax.Func ($2, $5) }

func_body:
| SYMBOL FATARROW func_body { Syntax.Lambda ($1, $3) }
| SYMBOL FATARROW sxp { Syntax.Lambda ($1, $3) }

appl_body:
| sxp sxp { Syntax.Appl ($1, $2) }
| appl_body sxp { Syntax.Appl ($1, $2) }
