(* alpha-conversion *)
module M = Map.Make(struct
  type t = Syntax.t
  let equal = Syntax.equal
  let compare = Syntax.compare
end)
