type t = string * int [@@deriving show, eq, ord]

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs
let counter = ref 0
let mk s = (s, 0)
let genid s =
  incr counter;
  (Printf.sprintf "%s" s), !counter

let gentmp typ: t =
  incr counter;
  (Printf.sprintf "T%s" (Type.id_of_mono typ)), !counter

let gen_typed_tmp typ =
  let id = gentmp typ in
  (id, Type.Mono typ)
