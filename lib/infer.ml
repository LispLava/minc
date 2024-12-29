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

let rec free_vars (t: Type.t): Type.v list = match t with
  | Type.Var x -> [x]
  | Type.App (_, ts) -> List.concat (List.map free_vars ts)
let rec free_vars_p (t: Type.p): Type.v list = match t with
  | Type.Mono t -> free_vars t
  | Type.Forall (x, t) -> List.filter (fun y -> y != x) (free_vars_p t)
let rec free_vars_e (env: env): S.t =
  E.fold (fun _ t acc -> S.union (S.of_list (free_vars_p t)) acc) env S.empty

let rec generalize env (t: Type.t): Type.p =
  let fv = free_vars t in
  let fv_e = free_vars_e env in
  let quantifiers = List.filter (fun x -> not (S.mem x fv_e)) fv in
  List.fold_right (fun x t -> Type.Forall (x, t)) quantifiers (Type.Mono t)

let rec (<$) (x: Type.v) (t: Type.t): bool = match t with
  | Type.Var y -> x == y
  | Type.App (_, ts) -> List.exists ((<$) x) ts

let type_mismatch t1 t2 = failwith (Format.asprintf "Type mismatch: %a != %a" Type.pp t1 Type.pp t2)
let rec unify (t1: Type.t) (t2: Type.t): m =
  (* Format.printf "unify: %a, %a\n" Type.pp t1 Type.pp t2; flush stdout; *)
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
  | Syntax.U(u, e) -> infer_unary env u e t
  | Syntax.B(b, e1, e2) -> infer_binary env b e1 e2 t
  | _ -> failwith "Not implemented"
  in (Format.printf "s = %a\n" pp_m s; flush stdout; s)
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

let rec apply_sub (m: m) (e: Syntax.t): Syntax.t = match e with
  | (Syntax.C c, t) -> (Syntax.C c, apply m t)
  | (Syntax.U(u, e), t) -> (Syntax.U(u, apply_sub m e), apply m t)
  | (Syntax.B(b, e1, e2), t) ->
    (Syntax.B(b, apply_sub m e1, apply_sub m e2), apply m t)
  | _ -> failwith "Not implemented"
let infer (e, _) = match e with
  | Syntax.Let(_, _, e) -> apply_sub (infer_m E.empty e) e
  | _ -> failwith "Not implemented"
