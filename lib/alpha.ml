(* alpha-conversion *)
module M = Map.Make(struct
  type t = Syntax.t
  let compare = Syntax.compare
end)
