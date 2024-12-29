(*
Algorithm M

See Fig. 3 in Oukseh Lee and Kwangkeun Yi. 1998. Proofs about a folklore
let-polymorphic type inference algorithm. ACM Trans. Program. Lang. Syst. 20, 4
(July 1998), 707–723.  https://doi.org/10.1145/291891.291892
*)
module E = M.Make(struct type t = Id.t let equal = Id.equal let compare = Id.compare end)
type env = Type.p E.t
module M = M.Make(struct type t = Type.v let equal = Type.equal_v let compare = Type.compare_v end)
type m = Type.t M.t
module S = Set.Make(struct type t = Type.v let compare = Type.compare_v end)
let rec apply (m: m) t = match t with
  | Type.Var x -> M.get m x t
  | Type.App (f, ts) -> Type.App (f, List.map (apply m) ts)
let rec apply_p (m: m) t = match t with
  | Type.Mono t -> Type.Mono (apply m t)
  | Type.Forall (x, t) -> if M.mem x m then
      failwith "Internal error: substitution conflicts with quantified variable."
    else
      Type.Forall (x, apply_p m t)
let rec apply_e (m: m) (env: env) = E.map (fun t -> apply_p m t) env

let combine (m2: m) (m1: m) = M.fold (fun k v m -> M.add k (apply m2 v) m) m1 m2

let inst (t: Type.p) =
  let rec aux (m: m) t = match t with
    | Type.Var x -> M.get m x t
    | Type.App (f, ts) -> Type.App (f, List.map (aux m) ts)
  in
  let rec aux' (m: m) t = match t with
    | Type.Mono t -> aux m t
    | Type.Forall (x, t) -> aux' (M.add x (Type.gentyp ()) m) t
  in
  aux' M.empty t

let rec free_vars (t: Type.t): S.t = match t with
  | Type.Var x -> S.singleton x
  | Type.App (_, ts) -> List.fold_left (fun acc t -> S.union (free_vars t) acc) S.empty ts
let rec free_vars_p (t: Type.p): S.t = match t with
  | Type.Mono t -> free_vars t
  | Type.Forall (x, t) -> S.remove x (free_vars_p t)
let free_vars_e (env: env): S.t = E.fold (fun _ t acc -> S.union (free_vars_p t) acc) env S.empty

let rec generalize env (t: Type.t): Type.p =
  let fv = free_vars t in
  let fv_e = free_vars_e env in
  let quantifiers = S.filter (fun x -> not (S.mem x fv_e)) fv in
  let g = S.fold (fun x t -> Type.Forall (x, t)) quantifiers (Type.Mono t) in
  Format.printf "=============\ngeneralize: %a ==> %a\n" Type.pp t Type.pp_p g; flush stdout;
  g

let rec (<$) (x: Type.v) (t: Type.t): bool = match t with
  | Type.Var y -> x == y
  | Type.App (_, ts) -> List.exists ((<$) x) ts

let type_mismatch t1 t2 = failwith (Format.asprintf "Type mismatch: %a != %a" Type.pp t1 Type.pp t2)
let rec unify (t1: Type.t) (t2: Type.t): m =
  Format.printf "unify: %a, %a\n" Type.pp t1 Type.pp t2; flush stdout;
  match t1, t2 with
  | Type.Var x, Type.Var y when x == y -> M.empty
  | Type.Var x, _ -> if x <$ t2 then failwith "Infinite type detected." else M.add x t2 M.empty
  | _, Type.Var _ -> unify t2 t1
  | Type.App (f1, ts1), Type.App (f2, ts2) ->
    if f1 != f2 then type_mismatch t1 t2
    else (try List.fold_left2 (fun m t1 t2 -> combine (unify (apply m t1) (apply m t2)) m) M.empty ts1 ts2 with
      | Invalid_argument _ -> type_mismatch t1 t2)

let infer_c env c t = unify t (Op.c_to_type c)

let pp_m ppf m =
  Format.fprintf ppf "{ ";
  M.iter (fun x t -> Format.fprintf ppf "%a -> %a, " Type.pp_v x Type.pp t) m;
  Format.fprintf ppf " }"

let rec infer_m (env: env) ((e, t): Syntax.t): m =
  let s = match e with
  | Syntax.C c -> infer_c env c t
  (* TODO: We shouldn't handle built-in functions any differently from user
  defined functions. To achieve that, we should cast*)
  | Syntax.U(u, e) -> infer_unary env u e t
  | Syntax.B(b, e1, e2) -> infer_binary env b e1 e2 t
  | Syntax.T(op, e1, e2, e3) -> failwith "T: Not implemented"
  | Syntax.Var id -> (match E.find_opt id env with
    | Some p -> unify t (inst p)
    | None -> failwith (Format.asprintf "Unbound variable: %a" Id.pp id))
  | Syntax.Abstraction { args; body = (body, rettyp) } ->
    let t' = Type.func (List.map snd args) rettyp in
    let s = unify t t' in
    let rettyp = apply s rettyp in
    let env = apply_e s env in
    let env = List.fold_left (fun env (id, typ) -> E.add id (Type.Mono (apply s typ)) env) env args in
    let s2 = infer_m env (body, rettyp) in
    combine s2 s
  | Syntax.A(Op.Tuple, es) -> failwith "A: Not implemented"
  | Syntax.A(Op.App, (f, f_t)::args) ->
    let f_t' = Type.func (List.map snd args) t in
    let s = infer_m env (f, f_t') in
    let env = apply_e s env in
    let s = List.fold_left (fun s (e, t) -> combine (infer_m env (e, apply s t)) s) s args in
    combine s (unify (apply s f_t') f_t) (* type-check f_t *)
  | Syntax.Let((id, p), (e1, t1), (e2, t2)) ->
    let s1 = infer_m env (e1, t1) in
    let t1 = apply s1 t1 in
    let env = apply_e s1 env in
    let s1, env = (match p with
      | Type.Mono mono -> let s1 = combine (unify mono t1) s1 in (* type-check p *)
        s1, E.add id (generalize env (apply s1 t1)) env
      | Type.Forall _ -> (
        Format.fprintf Format.err_formatter "Type checking of polymorphic types is not implemented yet. Got: %a\n" Type.pp_p p;
        s1, E.add id (generalize env (apply s1 t1)) env)
    ) in
    let s2 = infer_m env (e2, apply s1 t2) in
    let s = combine s2 s1 in
    combine s (unify (apply s t2) t)
  | Syntax.LetTuple(ids, e1, e2) -> failwith "LetTuple Not implemented"
  | _ -> failwith "_: Not implemented"
  in
  (* (Format.printf "s = %a\nex = \n%a\n\n\n\n\n" pp_m s Syntax.pp_e e; flush stdout; s) *)
  (Format.printf "s = %a\n\n" pp_m s; flush stdout; s)
and infer_unary env u (e, tt) t =
  let infer_u env e (tt, a) (t, b) =
    let s1 = unify tt a in
    let s2 = unify t b in
    let s = combine s2 s1 in
    let env = apply_e s env in
    combine (infer_m env (e, apply s tt)) s
  in match u with
  | Op.Not -> infer_u env e (tt, Type.bool) (t, Type.bool)
  | Op.Neg -> infer_u env e (tt, t) (t, tt)
and infer_binary env b (e1, tt1) (e2, tt2) t =
  let infer_b env e1 e2 (tt1, a) (tt2, b) (t, c) =
    let s1 = unify tt1 a in
    let s2 = unify tt2 b in
    let s3 = unify t c in
    let s = combine s3 (combine s2 s1) in
    let env = apply_e s env in
    let s1 = infer_m env (e1, apply s tt1) in
    let s1 = combine s1 s in
    let s2 = infer_m env (e2, apply s1 tt2) in
    let s = combine s2 s1 in
    s
  in match b with
  | Op.Add | Op.Sub | Op.Mul | Op.Div -> infer_b env e1 e2 (tt1, tt2) (tt2, tt1) (t, tt1)
  | Op.Mod -> infer_b env e1 e2 (tt1, Type.int) (tt2, Type.int) (t, Type.int)
  | Op.Eq | Op.LE -> infer_b env e1 e2 (tt1, tt2) (tt2, tt1) (t, Type.bool)
  | Op.Array -> infer_b env e1 e2 (tt1, tt1) (tt2, Type.int) (t, Type.array tt1)
  | Op.Get -> infer_b env e1 e2 (tt1, Type.array t) (tt2, Type.int) (t, t)

(* let rec apply_sub (m: m) (e: Syntax.t): Syntax.t = match e with
  | (Syntax.C c, t) -> (Syntax.C c, apply m t)
  | (Syntax.U(u, e), t) -> (Syntax.U(u, apply_sub m e), apply m t)
  | (Syntax.B(b, e1, e2), t) ->
    (Syntax.B(b, apply_sub m e1, apply_sub m e2), apply m t)
  | (Syntax.T(op, e1, e2, e3), t) -> Syntax.T(op, apply_sub m e1, apply_sub m e2, apply_sub m e3), apply m t
  | (Syntax.Var id, t) -> (Syntax.Var id, apply m t)
  | (Syntax.Abstraction { args; body = (body, rettyp) }, t) ->
    let rettyp = apply m rettyp in
    let body = apply_sub m body in
    let args = List.map (fun (id, typ) -> (id, apply m typ)) args in
    (Syntax.Abstraction { args; body = (body, rettyp) }, apply m t)
  | (Syntax.A(Op.Tuple, es), t) -> (Syntax.A(Op.Tuple, List.map (fun e -> apply_sub m e) es), apply m t)
  | (Syntax.A(Op.App, (f, f_t)::args), t) ->
    let f_t = apply m f_t in
    let f = apply_sub m f in
    let args = List.map (fun (e, t) -> (apply_sub m e, apply m t)) args in
    (Syntax.A(Op.App, (f, f_t)::args), apply m t)
  | (Syntax.Let((id, p), (e1, t1), (e2, t2)), t) ->
    let t1 = apply m t1 in
    let t2 = apply m t2 in
    let e1 = apply_sub m e1 in
    let e2 = apply_sub m e2 in
    (Syntax.Let((id, p), (e1, t1), (e2, t2)), apply m t)
  | (Syntax.LetTuple(ids, e1, e2), t) -> failwith "LetTuple Not implemented"
  | _ -> failwith "_: Not implemented" *)
let infer e = infer_m E.empty e
