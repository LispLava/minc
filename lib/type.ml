(* Hindley-Milner type system *)
type p = (* polytype *)
  | Mono of t
  | Forall of v * p [@@deriving eq, ord]
and v = I of int (* type variable *)
and t = (* monotype *)
  | Var of v
  | App of f * t list
and f = (* type functions *)
  | Unit
  | Bool
  | Int
  | Float
  | String
  | Tuple
  | Array
  | Fun

let id_of_fun : (f -> string) = function
  | Unit -> "u"
  | Bool -> "b"
  | Int -> "i"
  | Float -> "d"
  | String -> "s"
  | Tuple -> "t"
  | Array -> "a"
  | Fun -> "f"
let pp_v ppf = function
  | I i -> Format.fprintf ppf "#%d" i
let rec pp ppf = function
  | Var v -> pp_v ppf v
  | App (Unit, []) -> Format.fprintf ppf "unit"
  | App (Bool, []) -> Format.fprintf ppf "bool"
  | App (Int, []) -> Format.fprintf ppf "int"
  | App (Float, []) -> Format.fprintf ppf "float"
  | App (String, []) -> Format.fprintf ppf "string"
  | App (Tuple, ts) -> Format.fprintf ppf "(%a)" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " * ") pp) ts
  | App (Array, [t]) -> Format.fprintf ppf "%a array" pp t
  | App (Fun, [args; ret]) -> Format.fprintf ppf "(%a -> %a)" pp args pp ret
  | App (f, ts) -> Format.fprintf ppf "!!!%s %a!!!" (id_of_fun f) (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp) ts
let rec pp_p ppf = function
  | Mono t -> Format.fprintf ppf "%a" pp t
  | Forall (v, p) -> Format.fprintf ppf "∀%a. %a" pp_v v pp_p p

let unit = App(Unit, [])
let bool = App (Bool, [])
let int = App (Int, [])
let float = App (Float, [])
let string = App (String, [])
let array x = App (Array, [x])
let tuple lst = App (Tuple, lst)
let func args ret = App (Fun, [tuple args; ret])
let genvar: unit -> v =
  let counter = Atomic.make 0 in
  fun () -> (Atomic.incr counter; I (Atomic.get counter))
let gentyp () = Var(genvar ())

let id_of_mono : (t -> string) = function
  | Var _ -> failwith "type variable must be instantiated to have an identifier"
  | App (f, _) -> id_of_fun f

let id_of_typ : (p -> string) = function
  | Mono m -> id_of_mono m
  | Forall _ -> "p"

let parse_type x = match x with
  | "unit" -> Unit
  | "bool" -> Bool
  | "int" -> Int
  | "float" -> Float
  | _ -> failwith ("unknown type: " ^ x)
let poly_unary = let a, b = (genvar ()), (genvar ()) in
  let ra, rb = Var a, Var b in
  Forall (a, Forall (b, Mono (App (Fun, [ra; rb]))))

let poly_binary = let a, b, c = (genvar ()), (genvar ()), (genvar ()) in
  let ra, rb, rc = Var a, Var b, Var c in
  Forall (a, Forall (b, Forall (c, Mono (App (Fun, [ra; rb; rc])))))
