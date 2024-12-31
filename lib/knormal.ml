open Infer
type e =
  | C of Op.c
  | U of Op.u * Id.i
  | B of Op.b * Id.i * Id.i
  | T of Op.t * Id.i * Id.i * Id.i
  | Let of Id.p * t * e
  | Var of Id.t
  | Tuple of Id.i list
  | App of Id.i * (Id.i list)
  | Fix of Id.i * fundef
  | LetTuple of Id.i list * Id.i * t
  [@@deriving show]
and fundef = { args : Id.i list; body : t } [@@deriving show]
and t = e * Type.t [@@deriving show, eq, ord]

let rec pp_e ppf e = match e with
  | C c -> Format.fprintf ppf "%a" Op.pp_c c
  | U (u, x) -> Format.fprintf ppf "(%a %a)" Id.pp (Op.op_u_to_id u) Id.pp_i x
  | B (b, x, y) -> Format.fprintf ppf "(%a (%a) %a)" Id.pp_i x Op.pp_b b Id.pp_i y
  | T (t, x, y, z) -> Format.fprintf ppf "%a %a %a %a" Op.pp t Id.pp_i x Id.pp_i y Id.pp_i z
  | Let ((id, id_t), e, e') -> Format.fprintf ppf "let %a : %a = %a in\n%a" Id.pp id Type.pp_p id_t pp e pp_e e'
  | Var x -> Id.pp ppf x
  | Tuple xs -> Format.fprintf ppf "(%a)" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") Id.pp_i) xs
  | App (x, xs) -> Format.fprintf ppf "%a %a" Id.pp_i x (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") Id.pp_i) xs
  | Fix ((_, id_t), {args; body}) -> Format.fprintf ppf "(fun %a ->\n%a):%a" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") Id.pp_i) args pp body Type.pp id_t
  | LetTuple (ids, x, e) -> Format.fprintf ppf "let %a = %a in\n%a" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") Id.pp_i) ids Id.pp_i x pp e
and pp ppf (e, t) = Format.fprintf ppf "(%a : %a)" pp_e e Type.pp t

let insert_let ex k : t =
  let (e, t) = ex in
  match e with
  | Var x -> k (x, t)
  | _ -> let x = Id.gentmp t in
    let e', t' = k (x, t) in
    Let ((x, Mono t), ex, e'), t'

let rec g (m: m) ((e, t): Syntax.t): t =
  let aa = apply m in
  let t = aa t in
  match e with
  | Syntax.C c -> (C c, t)
  | Syntax.U(u, e) -> insert_let (g m e) (fun x -> (U(u, x), t))
  | Syntax.B(b, e1, e2) -> insert_let (g m e1) (fun x -> insert_let (g m e2) (fun y -> (B(b, x, y), t)))
  | Syntax.T(op, e1, e2, e3) -> insert_let (g m e1) (fun x -> insert_let (g m e2) (fun y -> insert_let (g m e3) (fun z -> (T(op, x, y, z), t))))
  | Syntax.Let((id, id_t), e1, e2) -> let (e1, t1) = g m e1 in
    assert ((aa t1) = t1);
    let (e2, t2) = g m e2 in
    assert ((aa t2) = t2);
    let id_t = apply_p m !id_t in
    (Let((id, id_t), (e1, t1), e2), t2)
  | Syntax.Var x -> (Var x, t)
  | Syntax.Tuple(es) -> let rec bind xs ts = function
    | [] -> Tuple(xs), Type.tuple ts
    | e :: es ->
        let _, t as g_e = g m e in
        insert_let g_e (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
    bind [] [] es
  | Syntax.App(e, es) -> insert_let (g m e) (fun x -> let rec bind xs = function
    | [] -> App(x, xs), aa t
    | e :: es -> insert_let (g m e) (fun y -> bind (xs @ [y]) es)
    in
    bind [] es)
  | Syntax.Fix ((id, id_t), {args; body}) ->
    let body = g m body in
    let id_t = aa id_t in
    let args = List.map (fun (x, t) -> (x, aa t)) args in
    Fix((id, id_t), {args; body}), id_t
  | LetTuple(ids, e1, e2) -> insert_let (g m e1) (fun x -> (LetTuple(ids, x, g m e2), t))
