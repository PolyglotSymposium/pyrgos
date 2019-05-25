(* Originally derived from usm-takl/dynamic-scoping-lis-in-ocmal; MIT license *)

let space = ['\t' '\n' '\r' ' ']
let num_char = ['0'-'9']
let symbol_char =
  ['!' '^' '%' '&' '*' '+' '-' '/' '0'-'9' '<' '=' '>' '?' '@' 'A'-'Z' '_'
   'a'-'z' '#' ':' '\'']

rule token = parse
| space+ { token lexbuf }
| num_char+ as lexeme { Parser.NUMBER (int_of_string lexeme) }
| symbol_char+ as lexeme { Parser.SYMBOL lexeme }
| '$' { keyword lexbuf }
| '{' { Parser.LBRACE }
| '}' { Parser.RBRACE }
| '(' { Parser.LPAREN }
| ')' { Parser.RPAREN }
| '[' { Parser.LBRACK }
| ']' { Parser.RBRACK }
| eof { Parser.EOF }

and keyword = parse
| '$' { Parser.APPLY }
| '\\' { Parser.LAMBDA }
| ',' { Parser.CONS }
| '`' { Parser.UNCONS }
| '\'' { Parser.QUOTE }
| '>' { Parser.EVAL }
| '_' { Parser.NIL }
| '-' { Parser.ISNIL }
| '=' { Parser.CASE }
