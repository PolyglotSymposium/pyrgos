(* Originally derived from usm-takl/dynamic-scoping-lis-in-ocmal; MIT license *)

let space = ['\t' '\n' '\r' ' ']
let symbol_char =
  ['!' '^' '%' '&' '*' '+' '-' '/' '0'-'9' '<' '=' '>' '?' '@' 'A'-'Z' '_'
   'a'-'z']

rule token = parse
| space+ { token lexbuf }
| "->" { Parser.ARROW }
| "=>" { Parser.FATARROW }
| ':' { Parser.COLON }
| symbol_char+ as lexeme { Parser.SYMBOL lexeme }
| '#' { atom lexbuf }
| '$' { Parser.DOLLAR }
| '|' { Parser.PIPE }
| '\'' { Parser.QUOTE }
| '(' { Parser.LPAREN }
| ')' { Parser.RPAREN }
| '[' { Parser.LSQUARE }
| ']' { Parser.RSQUARE }
| '{' { Parser.LBRACE }
| '}' { Parser.RBRACE }
| eof { Parser.EOF }

and atom = parse
| symbol_char+ as lexeme { Parser.ATOM lexeme }
