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
let prog_strs = prog_strs @ ["let rec quad (x: int) : int =
  let rec double () = 2 * x in
  2 * double () in
print_int (quad 10)
"]
let ff () = List.iter (fun prog_str ->
  Format.printf "\n\nsource = %s" prog_str;
  let prog = parse prog_str in
  (* Format.printf "\n\nast = %a\n" Syntax.pp prog; *)
  let m = Infer.infer prog in
  let k = Knormal.g m prog in
  let alpha = Alpha.f k in
  let beta = Beta.f alpha in
  let assoc = Assoc.f beta in
  Format.printf "\n\nFlattened K-normal form =\n%a\n\n%a" Knormal.pp assoc Infer.pp_m m;
  Knormal.check_type k;
  let clos = Closure.f assoc in
  Format.printf "\n\nclosure conversion =\n%a\n" Closure.pp_prog clos;
) prog_strs;
  Llvm.simple_test ()

let tryfun risky_function =
  try
    risky_function ()
  with
  | exn ->
      Printf.eprintf "Exception: %s\n" (Printexc.to_string exn);
      Printf.eprintf "Backtrace:\n%s\n" (Printexc.get_backtrace ());
      exit 1

let () = Printexc.record_backtrace true; tryfun ff
