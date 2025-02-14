let space = ['\t' ' ']
let num_char = ['0'-'9']
let id_char1 = ['a'-'z' 'A'-'Z' '_']
let id_charn = ['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' '\'']

rule token = parse
| space+ { token lexbuf }
| '\n' { Parser.NEWLINE }
| ';' { Parser.SEMICOLON }
| '+' { Parser.PLUS }
| '-' { Parser.MINUS }
| '(' { Parser.LPAREN }
| ')' { Parser.RPAREN }
| '=' { Parser.EQ }
| num_char+ as lexeme { Parser.INTEGER (Int64.of_string lexeme) }
| "let" { Parser.LET }
| id_char1 id_charn* as id { Parser.IDENTIFIER id }
| eof { Parser.EOF }
| _ { failwith "lexical error" }
