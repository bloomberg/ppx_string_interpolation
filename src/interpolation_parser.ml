type token =
  | String of string (* Plain string, does not start with % and contain $ *)
  | Format of string (* String, which starts with % and is treated as prinf-like format string *)
  | Expression of string (* Expression to interpolate; contains both '(' and ')' *)
  | Variable of string (* Name of variable, does not contain '(' and ')' *)
  | DollarChar (* Just a single '$' char, which comes from '$$' in raw string *)
  | PercentChar (* Just a single '%' char, which comes from '%%' in raw string *)

let token_to_string = function
  | String s ->
    s
  | Format s ->
    "`" ^ s ^ "`"
  | DollarChar ->
    "$"
  | PercentChar ->
    "%"
  | Expression e ->
    "{" ^ e ^ "}"
  | Variable v ->
    "[" ^ v ^ "]"

let print_tokens tokens =
  List.iter (fun (p, _) -> print_string (token_to_string p)) tokens

module Parser = struct
  (** Parse string, producing a list of tokens from this module. *)
  let from_string ~(payload_loc : Location.t) (str : string) =
    let lexbuf = Sedlexing.Utf8.from_string str in
    Sedlexing.set_position lexbuf payload_loc.loc_start;
    let loc (lexbuf : Sedlexing.lexbuf) =
      let adjust base rel = Lexing.{ rel with pos_fname = base.pos_fname } in
      let loc_start, loc_end = Sedlexing.lexing_positions lexbuf in
      Location.
        { loc_start = adjust payload_loc.loc_start loc_start
        ; loc_end = adjust payload_loc.loc_start loc_end
        ; loc_ghost = false
        }
    in
    let raise_error lexbuf msg = Location.raise_errorf ~loc:(loc lexbuf) msg in
    let remove_head_char str = String.sub str 1 (String.length str - 1) in

    (* TODO: take into account comments/strings of both syntaxes, which can contain parentheses! *)
    (* [p_level] - the level of parentheses. We need to check for balancing.
     * [c_level] - the level of comments. *)
    let rec parse_expression acc ~p_level ~c_level lexbuf =
      if c_level == 0 then
        match%sedlex lexbuf with
        | Star (Compl ('(' | ')')), '(' ->
          parse_expression (Sedlexing.Utf8.lexeme lexbuf :: acc) ~p_level:(p_level + 1) ~c_level:0 lexbuf
        | Star (Compl ('(' | ')')), ')' ->
          if p_level > 1 then
            parse_expression (Sedlexing.Utf8.lexeme lexbuf :: acc) ~p_level:(p_level - 1) ~c_level:0 lexbuf
          else
            List.rev @@ (Sedlexing.Utf8.lexeme lexbuf :: acc)
        | _ ->
          raise_error lexbuf "Incomplete expression (unmatched parentheses)..."
      else
        failwith "Unimplemented"
    in

    let rec parse acc lexbuf =
      let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z'] in
      let ident = [%sedlex.regexp? (letter | '_'), Star (letter | '0' .. '9' | '_')] in
      let lCaseIdent =
        [%sedlex.regexp? ('a' .. 'z' | '_'), Star (letter | '0' .. '9' | '_')]
      in
      let longVarIdent = [%sedlex.regexp? Star (ident, '.'), lCaseIdent] in
      match%sedlex lexbuf with
      | "$$" ->
        parse ((DollarChar, loc lexbuf) :: acc) lexbuf
      | "%%" ->
        parse ((PercentChar, loc lexbuf) :: acc) lexbuf
      | '%', Plus (Compl ('$' | '%')) ->
        parse ((Format (Sedlexing.Utf8.lexeme lexbuf), loc lexbuf) :: acc) lexbuf
      | Plus (Compl ('$' | '%')) ->
        parse ((String (Sedlexing.Utf8.lexeme lexbuf), loc lexbuf) :: acc) lexbuf
      | "$", longVarIdent ->
        parse
          ((Variable (remove_head_char @@ Sedlexing.Utf8.lexeme lexbuf), loc lexbuf)
          :: acc)
          lexbuf
      | "$(" ->
        parse
          (( Expression (List.fold_left ( ^ ) "(" (parse_expression [] ~p_level:1 ~c_level:0 lexbuf))
           , loc lexbuf )
          :: acc)
          lexbuf
      | eof ->
        acc
      | "$", Compl '$' ->
        raise_error lexbuf "Invalid character after $. Second $ is missing?"
      | "%$" ->
        raise_error lexbuf "Empty format. Another %% is missing?"
      | '%' ->
        raise_error lexbuf "Single %%. Another %% is missing?"
      | '$' ->
        raise_error lexbuf "Single $. Another $ is missing?"
      | _ ->
        raise_error lexbuf "Internal error in 'string_to_tokens'"
    in
    List.rev @@ parse [] lexbuf
end
