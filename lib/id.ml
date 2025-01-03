type t = string * int [@@deriving eq, ord]
type i = t * Type.t [@@deriving eq, ord]
type p = t * Type.p [@@deriving eq, ord]
type l = L of string * int [@@deriving eq, ord]

let to_l (x, n) = L(x, n)
let pp ppf ((id, n): t) = match n with
  | 0 -> Format.fprintf ppf "%s" id
  | _ -> Format.fprintf ppf "%s_%d" id n
let pp_i ppf (i, t) = Format.fprintf ppf "(%a : %a)" pp i Type.pp t
let pp_p ppf (i, p) = Format.fprintf ppf "(%a : %a)" pp i Type.pp_p p
let pp_l ppf (L(x, n)) = match n with
  | 0 -> Format.fprintf ppf "%s" x
  | _ -> Format.fprintf ppf "%s_%d" x n


let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs
let counter = ref 0
let mk s = (s, 0)
let genid s =
  incr counter;
  (Printf.sprintf "%s" s), !counter

let gen (s, _) =
  incr counter;
  (Printf.sprintf "%s" s), !counter

let gentmp typ: t =
  incr counter;
  "#", !counter

let gen_typed_tmp typ =
  let id = gentmp typ in
  (id, Type.Mono typ)
