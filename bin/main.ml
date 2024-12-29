open Minc

let () = print_newline (); print_newline ()

let parse (s: string) =
  let lexbuf = Lexing.from_string s in
  try
    Parser.prog Lexer.token lexbuf
  with
  | _ ->
      let start_pos = lexbuf.Lexing.lex_start_p in
      let curr_pos = lexbuf.Lexing.lex_curr_p in
      let line = start_pos.Lexing.pos_lnum in
      let start_col = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
      let end_col = curr_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
      failwith (Printf.sprintf "Parse error at line %d, columns %d-%d" line start_col end_col)

(* let () =
  let rec gcd m n =
    if n = 0 then m
    else begin Printf.printf "%d, %d\n" m n; gcd n (m mod n) end in
  print_int (gcd 239232 1526580); print_newline ()

let prog = "let rec gcd (m: int) n : int =
  if n = 0 then m
  else (
    print_int m; print_string \", \"; print_int n; print_newline ();
    let m: int = (m mod (n: int)) in
    gcd n m
  ) in
print_int (gcd 239232 1526580)
"
let () = Format.printf "%s\n%a\n" prog Syntax.pp (parse prog) *)
let prog_str = "let _ = () in -(10 + 10) < 20 * 2"
let prog_str = "let rec f x = x in f 10"
let prog_str = "
let rec f1 x f = f x x in
let rec f2 x = f1 (f1 x) in
f2"
let prog_str = "let rec f1 x f = f x x in
let rec f2 x =
  let rec f1_x y = f1 x y in
  let rec f1_xx y = f1 f1_x y in
  f1_xx in
let rec f3 (x: int) = f2 (f2 x) in
f3 1"
let prog = parse prog_str
let () = Format.printf "%s\n%a\n" prog_str Syntax.pp prog
let () = Format.printf "\n\n%s\n%a\n" prog_str Infer.pp_m (Infer.infer prog)
