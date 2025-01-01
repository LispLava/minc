open Knormal

(*
Assoc.f transforms
```ocaml
let x = (let y = e1 in e2) in
e3
```
to
```ocaml
let y = e1 in
let x = e2 in
e3
```
*)
let rec f xt =
  let (x, t) = xt in
  match x with
  | Let(x, e1, e2) ->
    (* insert re-associated e2 to the bottom of re-associated e1. *)
    let rec insert (ex, t) e2 = (match ex with
      | Let(y, e3, e4) -> Let(y, e3, insert e4 e2), t
      | LetTuple(xs, e3, e4) -> LetTuple(xs, e3, insert e4 e2), t
      | e -> Let(x, e1, e2), t
  ) in insert (f e1) (f e2)
  | Fix(x, {args; body}) -> Fix(x, {args; body = f body}), t
  | LetTuple(xs, e1, e2) -> LetTuple(xs, e1, f e2), t
  | _ -> xt
