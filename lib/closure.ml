type closure = { entry : Id.l * Type.p; actual_fv : Id.i list }
type e =
  | C of Op.c
  | U of Op.u * Id.i
  | B of Op.b * Id.i * Id.i
  | T of Op.t * Id.i * Id.i * Id.i
  | Let of Id.p * t * t
  | MakeCls of Id.p * closure * t
  | AppCls of Id.i * Id.i list
  | AppDir of (Id.l * Type.t) * Id.i list
  | Var of Id.t
  | Tuple of Id.i list
  | LetTuple of Id.i list * Id.i * t
and t = e * Type.t
type fundef = { name : Id.l * Type.p;
                args : Id.i list;
                formal_fv : Id.i list;
                body : t }
type prog = Prog of fundef list * t

let rec pp' ppf = function
  | C c -> Format.fprintf ppf "%a" Op.pp_c c
  | U (u, x) -> Format.fprintf ppf "(%a %a)" Id.pp (Op.op_u_to_id u) Id.pp_i x
  | B (b, x, y) -> Format.fprintf ppf "(%a (%a) %a)" Id.pp_i x Op.pp_b b Id.pp_i y
  | T (t, x, y, z) -> Format.fprintf ppf "%a %a %a %a" Op.pp t Id.pp_i x Id.pp_i y Id.pp_i z
  | Let (id, e, e') -> Format.fprintf ppf "let %a = %a in\n%a" Id.pp_p id pp e pp e'
  | MakeCls ((id, _), { entry; actual_fv }, e) -> Format.fprintf ppf "let %a = makecls %a %a in\n%a" Id.pp id
    Id.pp_l (fst entry)
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") Id.pp_i) actual_fv
    pp e
  | AppCls (x, xs) -> Format.fprintf ppf "%a %a" Id.pp_i x (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") Id.pp_i) xs
  | AppDir ((l, t), xs) -> Format.fprintf ppf "%a %a" Id.pp_l l (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") Id.pp_i) xs
  | Var x -> Id.pp ppf x
  | Tuple xs -> Format.fprintf ppf "(%a)" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") Id.pp_i) xs
  | LetTuple (ids, x, e) -> Format.fprintf ppf "let %a = %a in\n%a" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") Id.pp_i) ids Id.pp_i x pp e
(* and pp ppf (e, t) = Format.fprintf ppf "%a" pp' e *)
and pp ppf (e, t) = Format.fprintf ppf "(%a : %a)" pp' e Type.pp t

let pp_prog ppf (Prog(fundefs, e)) =
  List.iter (fun { name; args; formal_fv; body } ->
    Format.fprintf ppf "let rec (%a : %a) %a | %a = %a\n=======\n" Id.pp_l (fst name) Type.pp_p (snd name)
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") Id.pp_i) args
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") Id.pp_i) formal_fv
       pp body) fundefs;
  Format.fprintf ppf "%a" pp e

module S = Set.Make (struct type t = Id.t let compare = Id.compare end)
module E = Infer.E
let merge_f = (fun _ a b -> match a, b with
| Some a, Some b -> Some a
| Some a, None | None, Some a -> Some a
| None, None -> None)


let get_id x = List.map fst x
let rec fv (e, t) = match e with
  | C _ -> E.empty
  | Var x -> E.singleton x t
  | U(_, (x, x_t)) -> E.singleton x t
  | B(_, (x, x_t), (y, y_t)) -> E.add x x_t (E.singleton y y_t)
  | T(_, (x, x_t), (y, y_t), (z, z_t)) -> E.add x x_t (E.add y y_t (E.singleton z z_t))
  | Let((x, p), e1, e2) -> E.merge merge_f (fv e1) (E.remove x (fv e2))
  | MakeCls((x, p), { entry; actual_fv }, e) -> remove_es (E.remove x (fv e)) actual_fv
  | AppCls((x, x_t), es) -> add_es (E.singleton x x_t) es
  | AppDir(_, es) | Tuple(es) -> add_es E.empty es
  | LetTuple(xs, (y, y_t), e) -> E.add y y_t (remove_es (fv e) xs)
and add_es env es = List.fold_left (fun env (x, x_t) -> E.add x x_t env) env es
and remove_es es = List.fold_left (fun env (x, _) -> E.remove x env) es

let rec g known top (e, t): t = match e with
  | Knormal.C c -> C c, t
  | Knormal.U(o, x) -> U(o, x), t
  | Knormal.B(o, x, y) -> B(o, x, y), t
  | Knormal.T(o, x, y, z) -> T(o, x, y, z), t
  | Knormal.Let((x, p), e1, e2) -> Let((x, p), g known top e1, g known top e2), t
  | Knormal.LetRec((x, p), { args; body = e1 }, e2) ->
    let top_backup = !top in
    let known' = S.add x known in
    let e1' = g known' top e1 in
    let zs = remove_es (fv e1') args in
    let known', e1' =
      if E.is_empty zs then known', e1' else
      ( top := top_backup;
        let e1' = g known top e1 in
        known, e1') in
    let zs = E.to_list (remove_es (fv e1') args) in
    top := { name = (Id.to_l x, p); args; formal_fv = zs; body = e1' } :: !top;
    let (_, e2_t) as e2' = g known' top e2 in
    assert (t = e2_t);
    if E.mem x (fv e2') then
      MakeCls((x, p), { entry = (Id.to_l x, p); actual_fv = zs }, e2'), t
    else
      e2'
  | Knormal.Var(x) -> Var(x), t
  | Knormal.Tuple(xs) -> Tuple(xs), t
  | Knormal.App((f, f_t), xs) when S.mem f known -> AppDir((Id.to_l f, f_t), xs), t
  | Knormal.App(f, xs) -> AppCls(f, xs), t
  | Knormal.LetTuple(xs, y, e) -> LetTuple(xs, y, g known top e), t

let f t = let top = ref [] in
  let e = g S.empty top t in
  Prog(List.rev !top, e)
