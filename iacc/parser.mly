%token <int64> INTEGER
%token <string> IDENTIFIER
%token LET EQ
%token PLUS MINUS
%token LPAREN RPAREN
%token SEMICOLON NEWLINE EOF

%start <Syntax.expr> program

%%

program:
| expr EOF { $1 }

expr:
| INTEGER { Syntax.Integer $1 }
| LPAREN expr RPAREN { $2 }
| LET IDENTIFIER EQ expr SEMICOLON expr { Syntax.Let ($2, $4, $6) }
| LET IDENTIFIER EQ expr NEWLINE expr { Syntax.Let ($2, $4, $6) }
| IDENTIFIER { Syntax.Variable $1 }
| expr PLUS expr { Syntax.PrimCall (Syntax.Add ($1, $3)) }
| expr MINUS expr { Syntax.PrimCall (Syntax.Sub ($1, $3)) }
