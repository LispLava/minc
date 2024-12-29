module Make (S: Map.OrderedType) = struct
  module M = Map.Make(S)
  include M
  let get m x d = match find_opt x m with
    | None -> d
    | Some y -> y
end
