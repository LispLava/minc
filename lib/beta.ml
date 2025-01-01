open Knormal

module E = Infer.E
let (find', find) = (Alpha.find', Alpha.find)

let rec g' env = function
  | C _ as c -> c
  | U (u, x) -> U (u, find env x)
  | B (b, x, y) -> B (b, find env x, find env y)
  | T (t, x, y, z) -> T (t, find env x, find env y, find env z)
  | Let(x, (e1, t), ((e2, t2) as et2)) -> (match g' env e1 with
    | Var(y) -> g' (E.add (fst x) y env) e2
    | e1 -> Let(x, (e1, t), g env et2))
  | Var(x) -> Var(find' env x)
  | Tuple xs -> Tuple (List.map (find env) xs)
  | App(x, xs) -> App(find env x, List.map (find env) xs)
  | Fix(x, {args; body}) -> Fix(x, {args; body = g env body})
  | LetTuple(xs, e1, e2) -> LetTuple(xs, find env e1, g env e2)
and g env (e, t) = (g' env e), t

let f = g E.empty
