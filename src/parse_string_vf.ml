(* Order Format - Value *)
type token = String of string           (* String, which does not start with % and contain $ *)
           | StringWithFormat of string (* String, which starts with % *)
           | Expression of string       (* Expression to interpolate; contains both '(' and ')' *)
           | Variable of string         (* Name of variable, does not contain '(' and ')' *)
           | DollarChar                 (* Just a single '$' char, which comes from '$$' in raw string *)

let token_to_string = function String s     -> s
                             | StringWithFormat s -> "`" ^ s ^ "`"
                             | DollarChar   -> "$"
                             | Expression e -> "{" ^ e ^ "}"
                             | Variable v   -> "[" ^ v ^ "]"

let print_tokens = List.iter (fun p -> print_string (token_to_string p))


(** Parse string, producing a list of tokens. We also transform '%%' after expression
    to single '%' and convert commented out expressions to strings. *)
let string_to_tokens str =
    let lexbuf = Sedlexing.Utf8.from_string str in

    let remove_head_char str = String.sub str 1 (String.length str - 1) in

    (* here we rely on UTF8/single codepage encoding - first '%' should occupy only one char *)
    let remove_double_percent = function
        StringWithFormat str -> if String.length str > 1 && String.get str 1 = '%' then
                                    String (remove_head_char str)
                                else
                                    StringWithFormat str
      | x -> x
    in

    (* here we also rely on UTF8/single codepage similar to [remove_double_percent] *)
    let convert_commented_out = function
        Expression e -> if String.length str >= 4 && String.get str 1 = '*' &&
                           String.get str (String.length str - 2) = '*' then
                            String ("$" ^ e)
                        else
                            Expression e
      | x -> x
    in

    (* TODO: take into account comments/strings, which can contain parentheses! *)
    let rec read_expression acc level lexbuf =
        match%sedlex lexbuf with
        | Star (Compl ('('|')')),'(' -> read_expression (Sedlexing.Utf8.lexeme lexbuf::acc) (level + 1) lexbuf
        | Star (Compl ('('|')')),')' -> if level > 1 then
                                        read_expression (Sedlexing.Utf8.lexeme lexbuf::acc) (level - 1) lexbuf
                                      else
                                        List.rev @@ Sedlexing.Utf8.lexeme lexbuf::acc
        | _ -> failwith "Incomplete expression (unmatched parentheses)..."
    in

    let rec fold acc lexbuf =
        let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z'] in
        let ident  = [%sedlex.regexp? (letter | '_'),Star (letter | '0' .. '9' | '_')] in
        let lCaseIdent = [%sedlex.regexp? ('a'..'z' | '_'),Star (letter | '0' .. '9' | '_')] in
        let longVarIdent = [%sedlex.regexp? Star (ident,'.'), lCaseIdent] in

        match%sedlex lexbuf with
        | '%', Plus (Compl '$') -> fold (StringWithFormat (Sedlexing.Utf8.lexeme lexbuf)::acc) lexbuf
        | Plus (Compl '$')      -> fold (String           (Sedlexing.Utf8.lexeme lexbuf)::acc) lexbuf
        | "$$"                  -> fold (DollarChar::acc) lexbuf
        | "$", longVarIdent     -> fold (Variable (remove_head_char @@ Sedlexing.Utf8.lexeme lexbuf)::acc) lexbuf
        | "$(" -> fold (Expression (List.fold_left (^) "(" (read_expression [] 1 lexbuf))::acc) lexbuf
        | eof -> acc
        | "$", (Compl '$') -> failwith "Invalid character after $. Second $ is missing?"
        | _ -> failwith "Unhandled failure"
    in
    List.rev_map convert_commented_out @@ List.map remove_double_percent @@ fold [] lexbuf

(* Convert StringWithFormat, which is not prepended by Expression or Variable,
   so we do not need to escape '%'. *)
let unmark_format tokens =
    List.rev @@ snd @@ List.fold_left
        (fun (current, tokens) token ->
            match current, token with
            | (  Variable _, StringWithFormat _)
            | (Expression _, StringWithFormat _) -> (token, token::tokens)

            | (_, StringWithFormat str) -> (token, (String str)::tokens)

            | _ -> (token, token::tokens)
        )
        (DollarChar, []) tokens

(* Add %s formats to strings without them. *)
let insert_default_formats = function [] -> []
    | x::tokens -> let sFormat = StringWithFormat "%s" in
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
                | StringWithFormat str -> Buffer.add_string buf str
                | _ -> failwith "Internal error: unreachable point in collapse_strings."
            in
            let buf = Buffer.create 16 in
            add_token_to_buf buf a;
            List.iter (add_token_to_buf buf) lst;
            Buffer.contents buf
        in
        match a with
        | StringWithFormat _ -> StringWithFormat (unsafe_join (a, lst))
        | String _ -> String (unsafe_join (a, lst))
        | _ -> a
    in
    let is_string = function | String _ | StringWithFormat _ -> true
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
            | Variable v, StringWithFormat str -> (v, str)::acc
            | Expression e, StringWithFormat str -> (e, str)::acc
            | _ -> acc
        ) [] tokens
    in
    match tokens with
    | (String str)::tokens -> (Some str, aggregate tokens)
    | (StringWithFormat _)::_ -> failwith "FAILURE, internal error"
    | _ -> (None, aggregate tokens)

let parse_string_to_prefix_expression str =
    let replace a b = List.map (fun x -> if x <> a then x else b) in
    string_to_tokens str |> replace DollarChar (String "$") |> unmark_format
        |> insert_default_formats |> collapse_strings |> aggregate_tokens

let str = "$Unix.getenv"

let prefix_expressions_to_ast (prefix, expressions) =
    let op_concat = let open Longident in
        Parsetree.{
            pexp_desc = Pexp_ident {txt = Lident "^"; loc = Location.none};
            pexp_loc = Location.none;
            pexp_attributes = []
        } in
    let sprintf = let open Longident in
        Parsetree.{
            pexp_desc = Pexp_ident {txt = Ldot (Lident "Printf", "sprintf"); loc = Location.none};
            pexp_loc = Location.none;
            pexp_attributes = []
        } in
    let args = List.map
                    (fun (e, _) -> (Asttypes.Nolabel, Parse.expression (Lexing.from_string e)))
                    expressions in
    let format_str =
        let joined = String.concat "" @@ List.map snd expressions in
        Parsetree.{
            pexp_desc = Pexp_constant (Pconst_string (joined, None));
            pexp_loc = Location.none;
            pexp_attributes = []
        } in
    let apply func args =
        Parsetree.{
            pexp_desc = Pexp_apply (func, args);
            pexp_loc = Location.none;
            pexp_attributes = []
        } in
    let sprintf_applied = apply sprintf @@ (Asttypes.Nolabel,format_str)::args in
    match prefix with
    | None -> sprintf_applied
    | Some prefix ->
    (
        let prefix = Parsetree.{
            pexp_desc = Pexp_constant (Pconst_string (prefix, None));
            pexp_loc = Location.none;
            pexp_attributes = []
        } in
        match expressions with
        | [] -> prefix
        | _ -> apply op_concat [(Asttypes.Nolabel,prefix); (Asttypes.Nolabel,sprintf_applied)]
    )

let parse_string str = parse_string_to_prefix_expression str |> prefix_expressions_to_ast
