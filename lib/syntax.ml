type e =
  | C of Op.c
  | U of Op.u * t
  | B of Op.b * t * t
  | T of Op.t * t * t * t
  | Let of (Id.t * Type.p) * t * t (* let polymorphism *)
  | Var of Id.t
  | Abstraction of fundef
  | A of Op.a * t list
  | LetTuple of Id.t list * t * t
  [@@deriving show]
and fundef = { args : (Id.t * Type.t) list; body : t } [@@deriving show]
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
let mk_tuple a = A(Op.Tuple, a)
let mk_app a = A(Op.App, a)

let mk_lettuple typed_list rhs body =
  let ids, typs = List.split typed_list in
  (LetTuple(ids, rhs, body), Type.App(Type.Tuple, typs))

let mk_func id (abstraction, rettyp) exp =
  let typs = List.map snd abstraction.args in
  let m = Type.(App(Fun, typs @ [rettyp])) in
  (* The aliasing here is deliberate and important for correctness *)
  Let((id, Type.Mono m), (Abstraction abstraction, m), exp)
