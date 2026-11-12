module P = Interpolation_parser
module E = Interpolation_emitter

let raise_format_error ~loc =
  Location.raise_errorf ~loc "Format is not followed by variable/expression. Missing %%?"

let parser_to_emitter (tokens : (P.token * Location.t) list) : E.token list =
  let cur_fmt, acc =
    List.fold_left
      (fun (cur_fmt, acc) (token, loc) ->
         match token, cur_fmt with
         | P.Format fmt, None ->
           Some (fmt, loc), acc
         | P.Expression e, curr_fmt ->
           None, E.Expression ((e, loc), curr_fmt) :: acc
         | P.Variable v, curr_fmt ->
           None, E.Variable ((v, loc), curr_fmt) :: acc
         | _, Some (_, loc) ->
           raise_format_error ~loc
         | P.String s, None ->
           None, E.String (s, loc) :: acc
         | P.DollarChar, None ->
           None, E.String ("$", loc) :: acc
         | P.PercentChar, None ->
           None, E.EscapedPercent loc :: acc)
      (None, [])
      tokens
  in
  match cur_fmt with Some (_, loc) -> raise_format_error ~loc | None -> List.rev acc
