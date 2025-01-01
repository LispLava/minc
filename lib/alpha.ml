open Knormal

module E = Infer.E
let find' env x = E.find_opt x env |> Option.value ~default:x
let find env xx =
  let (x, t) = xx in
  match E.find_opt x env with
  | Some x' -> (x', t)
  | None -> xx

let rec g' env = function
  | C _ as c -> c
  | U (u, x) -> U (u, find env x)
  | B (b, x, y) -> B (b, find env x, find env y)
  | T (t, x, y, z) -> T (t, find env x, find env y, find env z)
  | Let((x, p), e1, e2) ->
      let x' = Id.gen x in
      Let((x', p), g env e1, g (E.add x x' env) e2)
  | Var(x) -> Var(find' env x)
  | Tuple xs -> Tuple (List.map (find env) xs)
  | App(x, xs) -> App(find env x, List.map (find env) xs)
  | Fix((x, t), {args; body}) ->
      let x' = Id.gen x in
      let env = E.add x x' env in
      let (args, env) = List.fold_left (fun (args', env) (x, t) -> let x' = Id.gen x in
        (args' @ [x', t], E.add x x' env)) ([], env) args in
      Fix((x', t), {args; body = g env body})
  | LetTuple(xs, e1, e2) ->
      let (xs', env) = List.fold_left (fun (xs', env) (x, t) -> let x' = Id.gen x in
        (xs' @ [x', t], E.add x x' env)) ([], env) xs in
      LetTuple(xs', find env e1, g env e2)
and g env (e, t) = (g' env e), t

let f = g E.empty
