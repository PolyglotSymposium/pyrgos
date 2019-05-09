%token <Syntax.symbol> ATOM
%token <Syntax.symbol> SYMBOL
%token LPAREN RPAREN
%token QUOTE
%token ARROW FATARROW
%token DOLLAR
%token LSQUARE RSQUARE
%token LBRACE RBRACE
%token UNIT
%token PIPE
%token EOF
%type <Syntax.toplvl> toplvl
%start toplvl

%%

toplvl:
| DOLLAR sxp EOF { Syntax.Up $2 }
| sxp EOF { Syntax.Expr $1 }
| EOF { Syntax.Expr (Syntax.Symbol Prelude.unit) }

sxp:
| UNIT { Syntax.Symbol Prelude.unit }
| LBRACE txp RBRACE { Syntax.TExpr $2 }
| ATOM { Syntax.Atom $1 }
| SYMBOL { Syntax.Symbol $1 }
| LPAREN PIPE RPAREN { Syntax.Lambda [] }
| LPAREN func_body RPAREN { Syntax.Lambda $2 }
| LPAREN appl_body RPAREN { $2 }
| QUOTE sxp { Syntax.Quote $2 }
| LSQUARE RSQUARE { Prelude.nil }
| LSQUARE cons RSQUARE { Syntax.List $2 }

cons:
| ATOM { [$1] }
| ATOM cons { $1 :: $2 }

txp:
| SYMBOL { Syntax.TVar $1 }
| SYMBOL ARROW txp { Syntax.Func (Syntax.TVar $1, $3) }
| LPAREN RPAREN ARROW txp { Syntax.Func (Syntax.TVar Prelude.tUnit, $4) }
| LPAREN txp RPAREN ARROW txp { Syntax.Func ($2, $5) }
| LPAREN RPAREN { Syntax.TVar Prelude.tUnit }

func_body:
| func_piece PIPE func_body { $1 :: $3 }
| func_piece { [$1] }

func_piece:
| appl_body FATARROW appl_body { ($1, $3) }
| appl_body FATARROW sxp { ($1, $3) }
| sxp FATARROW appl_body { ($1, $3) }
| sxp FATARROW sxp { ($1, $3) }

appl_body:
| sxp sxp { Syntax.Appl ($1, $2) }
| appl_body sxp { Syntax.Appl ($1, $2) }
