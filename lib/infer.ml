(*
Algorithm M

See Fig. 3 in Oukseh Lee and Kwangkeun Yi. 1998. Proofs about a folklore
let-polymorphic type inference algorithm. ACM Trans. Program. Lang. Syst. 20, 4
(July 1998), 707–723.  https://doi.org/10.1145/291891.291892
*)
module E = M.Make(struct type t = Id.t let compare = Id.compare end)
type env = Type.p E.t
module M = M.Make(struct type t = Type.v let compare = Type.compare_v end)
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
let apply_e (m: m) (env: env) = E.map (fun t -> apply_p m t) env

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

let generalize env (t: Type.t): Type.p =
  let fv = free_vars t in
  let fv_e = free_vars_e env in
  let quantifiers = S.filter (fun x -> not (S.mem x fv_e)) fv in
  let g = S.fold (fun x t -> Type.Forall (x, t)) quantifiers (Type.Mono t) in
  (* Format.printf "=============\ngeneralize: %a ==> %a\n" Type.pp t Type.pp_p g; flush stdout; *)
  g

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

let pp_m ppf m =
  Format.fprintf ppf "{ ";
  M.iter (fun x t -> Format.fprintf ppf "%a -> %a, " Type.pp_v x Type.pp t) m;
  Format.fprintf ppf " }"

let mk_op str = (Syntax.Var str, Type.gentyp ())
let rec infer_m (env: env) ((e, t): Syntax.t): m =
  let s = match e with
  | Syntax.C c -> unify t (Op.c_to_type c)
  | Syntax.U(u, e) -> let op = mk_op (Op.op_u_to_id u) in
    infer_m env (Syntax.A(Op.App, [op; e]), t)
  | Syntax.B(b, e1, e2) -> let op = mk_op (Op.op_b_to_id b) in
    infer_m env (Syntax.A(Op.App, [op; e1; e2]), t)
  | Syntax.T(op, e1, e2, e3) -> let op = mk_op (Op.op_t_to_id op) in
    infer_m env (Syntax.A(Op.App, [op; e1; e2; e3]), t)
  | Syntax.Var id -> (match E.find_opt id env with
    | Some p -> unify t (inst p)
    | None -> failwith (Format.asprintf "Unbound variable: %a" Id.pp id))
  | Syntax.Abstraction fundef -> infer_abs env fundef t
  | Syntax.Fix((id, t1), fundef) -> let env = E.add id (Type.Mono t) env in
    let s = infer_m env (Syntax.Abstraction fundef, t) in
    combine s (unify (apply s t) t1)
  | Syntax.A(Op.Tuple, _) -> failwith "A: Not implemented"
  | Syntax.A(Op.App, args) -> infer_app env args t
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
  | Syntax.LetTuple(_, _, _) -> failwith "LetTuple Not implemented"
  in
  (* (Format.printf "s = %a\nex = \n%a\n\n\n\n\n" pp_m s Syntax.pp_e e; flush stdout; s) *)
  (* (Format.printf "s = %a\n\n" pp_m s; flush stdout; s) *)
  s
and infer_abs env { args; body = (body, rettyp) } t =
  let t' = Type.func (List.map snd args) rettyp in
  let s = unify t t' in
  let rettyp = apply s rettyp in
  let env = apply_e s env in
  let env = List.fold_left (fun env (id, typ) -> E.add id (Type.Mono (apply s typ)) env) env args in
  let s2 = infer_m env (body, rettyp) in
  combine s2 s
and infer_app env args t =
  match args with
  | ((f, f_t)::args) ->
    let f_t' = Type.func (List.map snd args) t in
    let s = infer_m env (f, f_t') in
    let env = apply_e s env in
    let s = List.fold_left (fun s (e, t) -> combine (infer_m env (e, apply s t)) s) s args in
    combine s (unify (apply s f_t') f_t) (* type-check f_t *)
  | _ -> failwith "Empty application"

let default_env = let env = E.empty in
  let env = E.add (Id.mk "print_int") (Type.Mono (Type.func [Type.int] Type.unit)) env in
  let env = E.add (Id.mk "print_float") (Type.Mono (Type.func [Type.float] Type.unit)) env in
  let env = E.add (Id.mk "print_string") (Type.Mono (Type.func [Type.string] Type.unit)) env in
  let env = E.add (Id.mk "print_newline") (Type.Mono (Type.func [Type.unit] Type.unit)) env in
  let a, b, c = Type.genvar (), Type.genvar (), Type.genvar () in
  let open Op in
  (* Unary functions *)
  (* bool -> bool *)
  let env = E.add (op_u_to_id Not) (Type.Mono (Type.func [Type.bool] Type.bool)) env in
  (* 'a -> 'a *)
  let f_a_a' = Type.(func [Var a] (Var a)) in
  let f_a_a = Type.(Forall (a, Mono f_a_a')) in
  let env = E.add (op_u_to_id Neg) f_a_a env in

  (* Binary functions *)
  (* ('a, 'b) -> 'c *)
  let f_abc' = Type.(func [Var a; Var b] (Var c)) in
  let f_abc = Type.(Forall (a, Forall (b, Forall (c, Mono f_abc')))) in
  let bop = [Add; Sub; Mul; Div] in
  let env = List.fold_left (fun env op -> E.add (op_b_to_id op) f_abc env) env bop in
  let f_int_int_int = Type.(Mono (func [int; int] int)) in
  let env = E.add (op_b_to_id Mod) f_int_int_int env in
  (* ('a, 'b) -> bool *)
  let f_a_b_bool' = Type.(func [Var a; Var b] bool) in
  let f_a_b_bool = Type.(Forall (a, Forall (b, Mono f_a_b_bool'))) in
  let bop = [Eq; LE] in
  let env = List.fold_left (fun env op -> E.add (op_b_to_id op) f_a_b_bool env) env bop in

  let f_a_int_arraya' = Type.(func [Var a; int] (array (Var a))) in
  let f_a_int_arraya = Type.(Forall (a, Mono f_a_int_arraya')) in
  let env = E.add (op_b_to_id Array) f_a_int_arraya env in
  (* ('a array, int) -> 'a *)
  let f_arraya_int_a' = Type.(func [array (Var a); int] (Var a)) in
  let f_arraya_int_a = Type.(Forall (a, Mono f_arraya_int_a')) in
  let env = E.add (op_b_to_id Get) f_arraya_int_a env in

  (* Ternary functions *)
  (* (bool, 'a, 'a) -> 'a *)
  let f_bool_a_a_a' = Type.(func [bool; Var a; Var a] (Var a)) in
  let f_bool_a_a_a = Type.(Forall (a, Mono f_bool_a_a_a')) in
  let env = E.add (op_t_to_id If) f_bool_a_a_a env in
  (* ('a array, int, 'a) -> unit *)
  let f_arraya_int_a_unit' = Type.(func [array (Var a); int; Var a] unit) in
  let f_arraya_int_a_unit = Type.(Forall (a, Mono f_arraya_int_a_unit')) in
  let env = E.add (op_t_to_id Put) f_arraya_int_a_unit env in
  env
let infer e = infer_m default_env e
