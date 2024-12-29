{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+
    { token lexbuf }
| '"'
    { STRING(parse_string lexbuf) }
| "(*"
    { comment lexbuf;
      token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| "true"
    { BOOL(true) }
| "false"
    { BOOL(false) }
| "not"
    { NOT }
| ['-' '+']? digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| '-'
    { MINUS }
| '+'
    { PLUS }
| "*"
    { AST }
| "/"
    { SLASH }
| "mod"
    { MOD }
| '='
    { EQUAL }
| "<>"
    { LESS_GREATER }
| "<="
    { LESS_EQUAL }
| ">="
    { GREATER_EQUAL }
| '<'
    { LESS }
| '>'
    { GREATER }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET }
| "in"
    { IN }
| "rec"
    { REC }
| ','
    { COMMA }
| '_'
    { IDENT(Id.gentmp Type.unit) }
| "Array.create" | "Array.make"
    { ARRAY_CREATE }
| '.'
    { DOT }
| "<-"
    { LESS_MINUS }
| "->"
    { ARROW }
| ":"
    { COLON }
| ';'
    { SEMICOLON }
| eof
    { EOF }
| lower (digit|lower|upper|'_')*
    { IDENT(Id.mk (Lexing.lexeme lexbuf)) }
| _
    { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }
and parse_string = parse
| '"'
    { "" }
| "\\n" (* newline *)
    { "\n" ^ parse_string lexbuf }
| "\\r" (* carriage return *)
    { "\r" ^ parse_string lexbuf }
| "\\t" (* tab *)
    { "\t" ^ parse_string lexbuf }
| "\\\\" (* escape backslash *)
    { "\\" ^ parse_string lexbuf }
| "\\\"" (* escape double quote *)
    { "\"" ^ parse_string lexbuf }
| eof
    { Format.eprintf "warning: unterminated string@."; "" }
| _
    { Lexing.lexeme lexbuf ^ parse_string lexbuf }
