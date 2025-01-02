type e =
  | C of Op.c
  | U of Op.u * t
  | B of Op.b * t * t
  | T of Op.t * t * t * t
  | Let of (Id.t * Type.p ref) * t * t (* let polymorphism *)
  | Var of Id.t
  | Tuple of t list
  | App of t * (t list)
  (* Type.t is for the monomorphic function type pre-generalization. Also,
  LetRec needs to be specialized because a function can capture part of
  environment via closures. Thus, function definitions cannot be trivially
  flattened by the assoc.ml transformation. *)
  | LetRec of (Id.t * Type.p ref) * Type.t * fundef * t
  (*
  Generalizing for LetTuple would permit impredicable types like:
  (∀a. a->a) * (∀a. a->a)
  Thus, we should treat it as just tuple destructuring. Further reading:
  Alejandro Serrano, Jurriaan Hage, Simon Peyton Jones, and Dimitrios
  Vytiniotis. 2020. A quick look at impredicativity. Proc. ACM Program. Lang.
  4, ICFP, Article 89 (August 2020), 29 pages. https://doi.org/10.1145/3408971
  *)
  | LetTuple of Id.i list * t * t
  [@@deriving show]
and fundef = { args : Id.i list; body : t } [@@deriving show]
and t = e * Type.t [@@deriving show, eq, ord]

let unit = C(Op.Unit)
let mk_bool b = C(Op.Bool b)
let mk_int i = C(Op.Int i)
let mk_float f = C(Op.Float f)
let mk_string s = C(Op.String s)
let mk_not a = U(Op.Not, a)
let mk_neg a = U(Op.Neg, a)
let mk_get a b = B(Op.Get, a, b)
let mk_add a b = B(Op.Add, a, b)
let mk_sub a b = B(Op.Sub, a, b)
let mk_mul a b = B(Op.Mul, a, b)
let mk_div a b = B(Op.Div, a, b)
let mk_mod a b = B(Op.Mod, a, b)
let mk_eq a b = B(Op.Eq, a, b)
let mk_le a b = B(Op.LE, a, b)
let mk_array a b = B(Op.Array, a, b)
let mk_if a b c = T(Op.If, a, b, c)
let mk_put a b c = T(Op.Put, a, b, c)
let mk_tuple a = Tuple(a)
let mk_app f a = App(f, a)

let mk_let (id, p) rhs body =
  (Let((id, ref p), rhs, body), snd body)
let mk_lettuple typed_list rhs body =
  (LetTuple(typed_list, rhs, body), snd body)

let mk_func id (abstraction, rettyp) exp =
  let typs = List.map snd abstraction.args in
  let m = Type.func typs rettyp in
  let id = (id, ref(Type.Mono m)) in
  LetRec(id, m, abstraction, exp), snd exp
