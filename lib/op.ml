type c =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | String of string [@@deriving show, eq, ord]
type u = Not | Neg [@@deriving show, eq, ord]
type b = Add | Sub | Mul | Div | Mod | Eq | LE | Array | Get [@@deriving show, eq, ord]
type t = If | Put [@@deriving show, eq, ord]
type a = Tuple | App [@@deriving show, eq, ord]

let c_to_type = function
  | Unit -> Type.unit
  | Bool _ -> Type.bool
  | Int _ -> Type.int
  | Float _ -> Type.float
  | String _ -> Type.string
