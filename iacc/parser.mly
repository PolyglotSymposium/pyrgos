(* TODO error handling *)

%token <int64> INTEGER
%token <string> LOWER_IDENTIFIER
%token <string> UPPER_IDENTIFIER
%token LET EQ
%token PLUS MINUS
%token LPAREN RPAREN
%token MATCH WITH BAR ARROW WILDCARD
%token SEMICOLON NEWLINE EOF

%left PLUS MINUS

%start <Syntax.expr> program

                     %%

                       program:
| expr EOF { $1 }

expr:
| INTEGER { Syntax.Integer $1 }
| LOWER_IDENTIFIER { Syntax.Variable $1 }
| UPPER_IDENTIFIER { Syntax.Constructor $1 }
| LPAREN expr RPAREN { $2 }
| LET pattern EQ expr SEMICOLON expr { Syntax.Let ($2, $4, $6) }
| LET pattern EQ expr NEWLINE expr { Syntax.Let ($2, $4, $6) }
| expr expr { Syntax.Apply ($1, $2) }
| expr PLUS expr { Syntax.Apply (Syntax.Apply (Syntax.InfixOp "+", $1), $3) }
| expr MINUS expr { Syntax.Apply (Syntax.Apply (Syntax.InfixOp "-", $1), $3) }
| MATCH expr WITH NEWLINE match_cases { Syntax.Match { subject = $2; cases = $5 } }

match_cases:
| BAR pattern ARROW expr NEWLINE match_cases { ($2, $4) :: $6 }
| BAR pattern ARROW expr { [($2, $4)] }

pattern:
| tpattern { Syntax.Pattern_Terminal $1 }
| UPPER_IDENTIFIER patterns { Syntax.Pattern_Deconstruct ($1, $2) }
| UPPER_IDENTIFIER { Syntax.Pattern_Deconstruct ($1, []) }

tpattern:
| INTEGER { Syntax.TPat_Int $1 }
| LOWER_IDENTIFIER { Syntax.TPat_Var $1 }
| WILDCARD { Syntax.TPat_Wildcard }

patterns:
| pattern patterns { $1 :: $2 }
| pattern { [$1] }
