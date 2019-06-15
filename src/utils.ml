(* Several utility functions, which are absent in Stdlib *)

(* Analog of Haskell/Data.List.span - applied to a predicate p and a list xs, 
   returns a tuple where first element is longest prefix (possibly empty) of 
   [lst] of elements that satisfy [pred] and second element is the remainder of the list. *)
let span pred lst =
    let rec iter acc = function
        | x::tail when pred x -> iter (x::acc) tail
        | lst                 -> List.rev acc, lst
    in
    iter [] lst

(* Takes pred2 : 'a -> 'a -> bool and traverses list, assembling
   list of pairs (x, [items right after x, for which pred2 x i is true]).
 *)
let bucket pred2 lst =
    let rec iter acc = function
        | [] -> List.rev acc
        | x::tail -> let (matches, rest) = span (pred2 x) tail in
                     iter ((x, matches) :: acc) rest
    in
    iter [] lst

(* Fold left with simultaneous processing of two elements of a list *)
let rec fold_left2 f acc = function [] -> acc
                                  | [_] -> acc
                                  | a::b::tail -> fold_left2 f (f acc a b) (b::tail)
