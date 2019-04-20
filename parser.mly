%token <Syntax.symbol> ATOM
%token <Syntax.symbol> SYMBOL
%token LPAREN RPAREN QUOTE ARROW FATARROW EOF COLON
%type <Syntax.expr> sxp
%start sxp

%%

txp:
| SYMBOL { Syntax.TVar $1 }
| SYMBOL ARROW txp { Syntax.Func (Syntax.TVar $1, $3) }
| LPAREN txp RPAREN ARROW txp { Syntax.Func ($2, $5) }

sxp:
| ATOM { Syntax.Atom $1 }
| SYMBOL { Syntax.Symbol $1 }
| LPAREN RPAREN { Syntax.Unit }
| LPAREN func_body RPAREN { $2 }
| LPAREN ann_body RPAREN { $2 }
| LPAREN appl_body RPAREN { $2 }
| QUOTE sxp { Syntax.Quote $2 }

ann_body:
| sxp COLON txp { Syntax.Annotate ($1, $3) }
| func_body COLON txp { Syntax.Annotate ($1, $3) }

func_body:
| SYMBOL FATARROW func_body { Syntax.Lambda ($1, $3) }
| SYMBOL FATARROW sxp { Syntax.Lambda ($1, $3) }

appl_body:
| sxp sxp { Syntax.Appl ($1, $2) }
| sxp appl_body { Syntax.Appl ($1, $2) }
