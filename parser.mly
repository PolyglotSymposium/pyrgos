%token <Syntax.symbol> ATOM
%token <Syntax.symbol> SYMBOL
%token LPAREN RPAREN QUOTE ARROW FATARROW DOLLAR LSQUARE RSQUARE LBRACE RBRACE
%type <Syntax.toplvl> toplvl
%start toplvl

%%

toplvl:
| DOLLAR sxp { Syntax.Up $2 }
| sxp { Syntax.Expr $1 }

sxp:
| LBRACE txp RBRACE { Syntax.TExpr $2 }
| ATOM { Syntax.Atom $1 }
| SYMBOL { Syntax.Symbol $1 }
| LPAREN RPAREN { Syntax.unit }
| LPAREN func_body RPAREN { $2 }
| LPAREN appl_body RPAREN { $2 }
| QUOTE sxp { Syntax.Quote $2 }

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
