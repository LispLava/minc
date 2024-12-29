%{
open Syntax
let addtyp x = (x, Type.gentyp ())
let addptyp x = (x, Type.Mono (Type.gentyp ()))
let mktyp (x: e) ty : t = (x, ty)
let mkptyp x ty = (x, Type.Mono ty)
let settyp (x, _) ty = (x, ty)
let parse_type ((x, _): Id.t) = Type.parse_type x
%}

%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token NOT
%token MINUS
%token PLUS
%token AST
%token SLASH
%token MOD
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token ARROW
%token SEMICOLON
%token LPAREN
%token RPAREN
%token COLON
%token EOF

%nonassoc IN
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%right ARROW
%nonassoc prec_tuple
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS
%left AST SLASH MOD
%right prec_unary_minus
%left prec_app
%left DOT

%start prog
%type <Syntax.t> prog

%%

prog:
| exp EOF
    { $1 }

// simple_exp can be an argument of a function without extra parentheses
simple_exp:
| LPAREN exp RPAREN
    { $2 }
| LPAREN exp COLON type_exp RPAREN
    { settyp $2 $4 }
| LPAREN RPAREN
    { addtyp (unit) }
| BOOL
    { addtyp (mk_bool($1)) }
| INT
    { addtyp (mk_int($1)) }
| FLOAT
    { addtyp (mk_float($1)) }
| STRING
    { addtyp (mk_string($1)) }
| IDENT
    { addtyp (Var($1)) }
| simple_exp DOT LPAREN exp RPAREN
    { addtyp (mk_get $1 $4) }

type_exp:
| simple_type_exp
    { $1 }
| type_exp ARROW type_exp
    { Type.App(Type.Fun, [$1; $3]) }
| tuple_type
    { Type.App(Type.Tuple, $1) }
| type_args IDENT
    %prec prec_app
    { Type.App((parse_type $2), $1) }

type_args:
| type_args simple_type_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_type_exp
    %prec prec_app
    { [$1] }

simple_type_exp:
| IDENT
    { Type.App((parse_type $1), []) }
| LPAREN type_exp RPAREN
    { $2 }

tuple_type:
| type_exp AST type_exp
    { [$1; $3] }
| tuple_type AST type_exp
    { $1 @ [$3] }

exp:
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { addtyp (mk_not $2) }
| MINUS exp
    %prec prec_unary_minus
    { match $2 with
    | (C(Op.Float(f)), _) -> addtyp (mk_float (-.f))
    | e -> addtyp (mk_neg e) }
| exp PLUS exp
    { addtyp (mk_add $1 $3) }
| exp MINUS exp
    { addtyp (mk_sub $1 $3) }
| exp AST exp
    { addtyp (mk_mul $1 $3) }
| exp SLASH exp
    { addtyp (mk_div $1 $3) }
| exp MOD exp
    { addtyp (mk_mod $1 $3) }
| exp EQUAL exp
    { addtyp (mk_eq $1 $3) }
| exp LESS_GREATER exp
    { addtyp (mk_not (addtyp (mk_eq $1 $3))) }
| exp LESS exp
    { addtyp (mk_not (addtyp (mk_le $3 $1))) }
| exp GREATER exp
    { addtyp (mk_not (addtyp (mk_le $1 $3))) }
| exp LESS_EQUAL exp
    { addtyp (mk_le $1 $3) }
| exp GREATER_EQUAL exp
    { addtyp (mk_le $3 $1) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { addtyp (mk_if $2 $4 $6) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { addtyp (Let(addptyp $2, $4, $6)) }
| LET IDENT COLON type_exp EQUAL exp IN exp
    %prec prec_let
    { addtyp (Let(mkptyp $2 $4, $6, $8)) }
| LET REC IDENT fundef IN exp
    %prec prec_let
    { addtyp (mk_func $3 $4 $6) }
| simple_exp actual_args
    %prec prec_app
    { addtyp (mk_app ([$1] @ $2)) }
| elems
    %prec prec_tuple
    { addtyp (mk_tuple $1) }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    { mk_lettuple $3 $6 $8 }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { addtyp (mk_put $1 $4 $7) }
| exp SEMICOLON exp
    { mktyp (Let(Id.gen_typed_tmp Type.unit, $1, $3)) Type.unit }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { addtyp (mk_array $2 $3) }
| error
    { failwith
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ())) }

fundef:
| formal_args EQUAL exp
    { { args = $1; body = $3 }, Type.gentyp () }
| formal_args COLON type_exp EQUAL exp
    { { args = $1; body = $5 }, $3 }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }
| LPAREN maybe_typed_indent RPAREN formal_args
    { $2 :: $4 }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp
    { $1 @ [$3] }
| exp COMMA exp
    { [$1; $3] }

maybe_typed_indent:
| IDENT COLON type_exp
    { ($1, $3) }
| IDENT
    { addtyp $1 }

pat:
| pat COMMA maybe_typed_indent
    { $1 @ [$3] }
| maybe_typed_indent COMMA maybe_typed_indent
    { [$1; $3] }
