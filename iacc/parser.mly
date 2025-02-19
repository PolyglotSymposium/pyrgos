%token <int64> INTEGER
%token <string> IDENTIFIER
%token LET EQ
%token PLUS MINUS
%token LPAREN RPAREN
%token MATCH WITH BAR ARROW WILDCARD
%token SEMICOLON NEWLINE EOF

%start <Syntax.expr> program

%%

program:
| expr EOF { $1 }

expr:
| INTEGER { Syntax.Integer $1 }
| IDENTIFIER { Syntax.Variable $1 }
| LPAREN expr RPAREN { $2 }
| LET pattern EQ expr SEMICOLON expr { Syntax.Let ($2, $4, $6) }
| LET pattern EQ expr NEWLINE expr { Syntax.Let ($2, $4, $6) }
(* TODO infixes are right-associating *)
| expr PLUS expr { Syntax.ApplyInfix (Syntax.Add ($1, $3)) }
| expr MINUS expr { Syntax.ApplyInfix (Syntax.Sub ($1, $3)) }
| MATCH expr WITH NEWLINE match_cases { Syntax.Match { subject = $2; cases = $5 } }

match_cases:
| BAR pattern ARROW expr NEWLINE match_cases { ($2, $4) :: $6 }
| BAR pattern ARROW expr { [($2, $4)] }

pattern:
| INTEGER { Syntax.Pattern_Int $1 }
| IDENTIFIER { Syntax.Pattern_Var $1 }
| WILDCARD { Syntax.Pattern_Wildcard }
