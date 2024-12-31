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

(* (a, b) == c(a, b) *)
(* (+, *, ...): (a, b) -> c *)
let prog_strs = ["let _ = () in -(10 + 10) < 20 * 2"]
let prog_strs = prog_strs @ ["let rec f x = x in f 10"]
let prog_strs = prog_strs @ ["let rec f1 x f = f x x in
let rec f2 x =
  let rec f1_x y = f1 x y in
  let rec f1_xx y = f1 f1_x y in
  f1_xx in
let rec f3 (x: int) = f2 (f2 x) in
f3 1"]
let prog_strs = prog_strs @ ["let rec gcd (m: int) n : int =
  if n = 0 then m
  else (
    print_int m; print_string \", \"; print_int n; print_newline ();
    let m: int = (m mod (n: int)) in
    let (m, n) = (n, m) in
    gcd m n
  ) in
print_int (gcd 239232 1526580)
"]
let () = List.iter (fun prog_str ->
  let prog = parse prog_str in
  let m = Infer.infer prog in
  let k = Knormal.g m prog in
  let alpha = Alpha.f k in
  let beta = Beta.f alpha in
  (* Format.printf "\n\nsource = %s\nast = %a\ntype inference = %a\n" prog_str Syntax.pp prog Infer.pp_m m; *)
  Format.printf "\n\nsource = %s\ntype inference =\n%a\n\n%a" prog_str Knormal.pp beta Infer.pp_m m;
) prog_strs
