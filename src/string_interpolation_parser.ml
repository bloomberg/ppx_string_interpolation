module Parser = struct

(* Order Format - Value *)
type token = String of string           (* String, which does not start with % and contain $ *)
           | Format of string (* String, which starts with % *)
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


(** Parse string, producing a list of tokens. We also transform '%%' after expression
    to single '%' and convert commented out expressions to strings. *)
let string_to_tokens str =
    let lexbuf = Sedlexing.Utf8.from_string str in

    let remove_head_char str = String.sub str 1 (String.length str - 1) in

    (* here we also rely on UTF8/single codepage - '(','*' and ')' occupy only one byte. *)
    let convert_commented_out = function
        Expression e -> if String.length str >= 4 && String.get str 1 = '*' &&
                           String.get str (String.length str - 2) = '*' then
                            String ("$" ^ e)
                        else
                            Expression e
      | x -> x
    in

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
    List.rev_map convert_commented_out @@ parse [] lexbuf

(* Convert Format, which is not prepended by Expression or Variable,
   so we do not need to escape '%'. *)
let unmark_format tokens =
    List.rev @@ snd @@ List.fold_left
        (fun (current, tokens) token ->
            match current, token with
            | (  Variable _, Format _)
            | (Expression _, Format _) -> (token, token::tokens)

            | (_, Format str) -> (token, (String str)::tokens)

            | _ -> (token, token::tokens)
        )
        (DollarChar, []) tokens

(* Add %s formats to strings without them. *)
let insert_default_formats = function [] -> []
    | x::tokens -> let sFormat = Format "%s" in
        let result = snd @@ List.fold_left
        (fun (current, acc) y -> match current, y with
                      | Variable _, String _
                      | Expression _, String _ -> (y, y::sFormat::acc)
                      | _ -> (y, y::acc)
        ) (x, [x]) tokens in
        List.rev (
        match result with
        | Expression _ :: _
        | Variable _ :: _ -> sFormat::result
        | _ -> result)

(* See Haskell/Data.List.span *)
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
        bucket (fun a b -> is_string a && is_string b) tokens

(* ----------------- *)
let rec fold_left2 f acc = function [] -> acc
                                  | [_] -> acc
                                  | a::b::tail -> fold_left2 f (f acc a b) (b::tail)

(* |||||||||||||||||||||||||||||| *)
let aggregate_tokens tokens =
    let aggregate tokens = List.rev @@ fold_left2
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

let parse_string_to_prefix_expression str =
    let replace a b = List.map (fun x -> if x <> a then x else b) in
    string_to_tokens str |> replace DollarChar (String "$") |> replace PercentChar (String "%%") 
        |> unmark_format |> insert_default_formats |> collapse_strings |> aggregate_tokens

let parse_string str = parse_string_to_prefix_expression str

end
