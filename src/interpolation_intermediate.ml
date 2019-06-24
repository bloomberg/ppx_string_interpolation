module P = Interpolation_parser
module E = Interpolation_emitter

let parser_to_emitter (tokens : (P.token*Location.t) list) : E.token list =
    List.rev @@ snd @@
    List.fold_left
        (fun (cur_fmt, acc) (token, loc) ->
            match token, cur_fmt with
            | P.Format fmt, None ->
                    Some (fmt, loc), acc
            | P.Expression e, curr_fmt ->
                    None, E.Expression ((e, loc), curr_fmt)::acc
            | P.Variable v, curr_fmt ->
                    None, E.Variable ((v, loc), curr_fmt)::acc
            | _, Some (_, loc) -> raise (P.ParseStringError (loc, "Format is not followed by variable/expression. Missing %?"))
            | P.String s, None ->
                    None, E.String (s, loc)::acc
            | P.DollarChar, None ->
                    None, E.String ("$", loc)::acc
            | P.PercentChar, None ->
                    None, E.String ("%%", loc)::acc
        ) (None, []) tokens
