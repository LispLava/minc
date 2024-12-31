type c =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | String of string [@@deriving eq, ord]
type u = Not | Neg [@@deriving show, eq, ord]
type b = Add | Sub | Mul | Div | Mod | Eq | LE | Array | Get [@@deriving show, eq, ord]
type t = If | Put [@@deriving show, eq, ord]

let pp_c ppf = function
  | Unit -> Format.fprintf ppf "()"
  | Bool b -> Format.fprintf ppf "%b" b
  | Int i -> Format.fprintf ppf "%d" i
  | Float f -> Format.fprintf ppf "%f" f
  | String s -> Format.fprintf ppf "%s" s

let op_u_to_id x = Id.mk @@ match x with
  | Not -> "!"
  | Neg -> "#-"

let op_b_to_id x = Id.mk @@ match x with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "="
  | LE -> "<="
  | Array -> "Array.create"
  | Get -> "Array.get"

let op_t_to_id x = Id.mk @@ match x with
  | If -> "if"
  | Put -> "Array.put"

let c_to_type = function
  | Unit -> Type.unit
  | Bool _ -> Type.bool
  | Int _ -> Type.int
  | Float _ -> Type.float
  | String _ -> Type.string
