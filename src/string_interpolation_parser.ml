module Parser = struct

type token = String of string           (* Plain string, does not start with % and contain $ *)
           | Format of string           (* String, which starts with % and is treated as prinf-like format string *)
           | Expression of string       (* Expression to interpolate; contains both '(' and ')' *)
           | Variable of string         (* Name of variable, does not contain '(' and ')' *)
           | DollarChar                 (* Just a single '$' char, which comes from '$$' in raw string *)
           | PercentChar                (* Just a single '%' char, which comes from '%%' in raw string *)

let token_to_string = function String s     -> s
                             | Format s -> "`" ^ s ^ "`"
                             | DollarChar   -> "$"
                             | PercentChar  -> "%"
                             | Expression e -> "{" ^ e ^ "}"
                             | Variable v   -> "[" ^ v ^ "]"

let print_tokens = List.iter (fun p -> print_string (token_to_string p))

(** Parse string, producing a list of tokens from this module. *)
let string_to_tokens str =
    let lexbuf = Sedlexing.Utf8.from_string str in

    let remove_head_char str = String.sub str 1 (String.length str - 1) in

    (* TODO: take into account comments/strings of both syntaxes, which can contain parentheses! *)
    let rec parse_expression acc level lexbuf =
        match%sedlex lexbuf with
        | Star (Compl ('('|')')),'(' -> parse_expression (Sedlexing.Utf8.lexeme lexbuf::acc) (level + 1) lexbuf
        | Star (Compl ('('|')')),')' -> if level > 1 then
                                        parse_expression (Sedlexing.Utf8.lexeme lexbuf::acc) (level - 1) lexbuf
                                      else
                                        List.rev @@ Sedlexing.Utf8.lexeme lexbuf::acc
        | _ -> failwith "Incomplete expression (unmatched parentheses)..."
    in

    let rec parse acc lexbuf =
        let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z'] in
        let ident  = [%sedlex.regexp? (letter | '_'),Star (letter | '0' .. '9' | '_')] in
        let lCaseIdent = [%sedlex.regexp? ('a'..'z' | '_'),Star (letter | '0' .. '9' | '_')] in
        let longVarIdent = [%sedlex.regexp? Star (ident,'.'), lCaseIdent] in

        match%sedlex lexbuf with
        | "$$" -> parse (DollarChar::acc) lexbuf
        | "%%" -> parse (PercentChar::acc) lexbuf
        | '%', Plus (Compl ('$' | '%')) -> parse (Format (Sedlexing.Utf8.lexeme lexbuf)::acc) lexbuf
        | Plus (Compl ('$' | '%'))      -> parse (String           (Sedlexing.Utf8.lexeme lexbuf)::acc) lexbuf
        | "$", longVarIdent -> parse (Variable (remove_head_char @@ Sedlexing.Utf8.lexeme lexbuf)::acc) lexbuf
        | "$(" -> parse (Expression (List.fold_left (^) "(" (parse_expression [] 1 lexbuf))::acc) lexbuf
        | eof -> acc
        | "$", (Compl '$') -> failwith "Invalid character after $. Second $ is missing?"
        | "%$" -> failwith "Empty format. Another % is missing?"
        | '%' -> failwith "Single %. Another % is missing?"
        | '$' -> failwith "Single $. Another $ is missing?"
        | _ -> failwith "Unhandled failure"
    in
    List.rev @@ parse [] lexbuf

(* Add %s formats to strings without them. *)
let insert_default_formats = function [] -> []
    | x::tokens -> let sFormat = Format "%s" in
        let result = snd @@ List.fold_left
        (fun (current, acc) y -> match current, y with
                      | Variable _, String _
                      | Expression _, String _ -> (y, y::sFormat::acc)
                      | _ -> (y, y::acc)
        ) (x, [x]) tokens in
        List.rev result

(* Join several consecutive strings into one. *)
let collapse_strings tokens =
    let join_strings_in_bucket (a, lst) =
        let unsafe_join (a, lst) =
            let add_token_to_buf buf = function
                | String str
                | Format str -> Buffer.add_string buf str
                | _ -> failwith "Internal error: unreachable point in collapse_strings."
            in
            let buf = Buffer.create 16 in
            add_token_to_buf buf a;
            List.iter (add_token_to_buf buf) lst;
            Buffer.contents buf
        in
        match a with
        | Format _ -> Format (unsafe_join (a, lst))
        | String _ -> String (unsafe_join (a, lst))
        | _ -> a
    in
    let is_string = function | String _ | Format _ -> true
                             | _ -> false
    in
    List.map join_strings_in_bucket @@
        Utils.bucket (fun a b -> is_string a && is_string b) tokens


(* |||||||||||||||||||||||||||||| *)
let aggregate_tokens tokens =
    let aggregate tokens = List.rev @@ Utils.fold_left2
        (fun acc a b ->
            match a, b with
            | Variable v, Format str -> (v, str)::acc
            | Expression e, Format str -> (e, str)::acc
            | _ -> acc
        ) [] tokens
    in
    match tokens with
    | (String str)::tokens -> (Some str, aggregate tokens)
    | (Format _)::_ -> failwith "FAILURE, internal error"
    | _ -> (None, aggregate tokens)

let parse_string str =
    let replace a b = List.map (fun x -> if x <> a then x else b) in

    string_to_tokens str |> replace DollarChar (String "$") |> replace PercentChar (String "%%") 
        |> insert_default_formats |> collapse_strings |> aggregate_tokens

end
