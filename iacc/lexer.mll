let space = ['\t' ' ']
let num_char = ['0'-'9']
let lower_char = ['a'-'z']
let upper_char = ['A'-'Z']
let id_char = ['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' '\'']

rule token = parse
| space+ { token lexbuf }
| '\n' { Parser.NEWLINE }
| ';' { Parser.SEMICOLON }
| '|' { Parser.BAR }
| '+' { Parser.PLUS }
| '-' '>' { Parser.ARROW }
| '-' { Parser.MINUS }
| '(' { Parser.LPAREN }
| ')' { Parser.RPAREN }
| '=' { Parser.EQ }
| num_char+ as lexeme { Parser.INTEGER (Int64.of_string lexeme) }
| "let" { Parser.LET }
| "match" { Parser.MATCH }
| "with" { Parser.WITH }
| lower_char id_char* as id { Parser.LOWER_IDENTIFIER id }
| upper_char id_char* as id { Parser.UPPER_IDENTIFIER id }
| '_' id_char* { Parser.WILDCARD }
| eof { Parser.EOF }
| _ { failwith "lexical error" }
