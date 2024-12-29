open Minc

let () = print_newline (); print_newline ()

let parse (s: string) =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.token lexbuf in
  ast

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
let prog = parse prog_str
let () = Format.printf "%s\n%a\n" prog_str Syntax.pp prog
let () = Format.printf "\n\n%s\n%a\n" prog_str Infer.pp_m (Infer.infer prog)
