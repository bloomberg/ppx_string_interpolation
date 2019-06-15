type token = String of string | Expression of string * string option

(* here we also rely on UTF8/single codepage - '(','*' and ')' occupy only one byte. *)
let convert_commented_out = function
    Expression (str, fmt) -> if String.length str >= 4 && String.get str 1 = '*' &&
                       String.get str (String.length str - 2) = '*' then
                       match fmt with
                       | Some fmt -> String (fmt ^ "$" ^ str)
                       | None -> String ("$" ^ str)
                    else
                        Expression (str, fmt)
  | x -> x

(* Convert list of expressions with formats to ast.
   This function works for both format before value "%f$var" and
   value before format "$var%f" (scala style).
*)
let generate formatAfterVar (freeString, expressions) =
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
            pexp_attributes = [] (*[Ast_mapper.attribute_of_warning Location.none
                                "Test warning"] *)
        } in
    let apply func args =
        Parsetree.{
            pexp_desc = Pexp_apply (func, args);
            pexp_loc = Location.none;
            pexp_attributes = []
        } in
    let sprintf_applied = apply sprintf @@ (Asttypes.Nolabel,format_str)::args in
    match freeString with
    | None -> sprintf_applied
    | Some freeString ->
    (
        let freeString = Parsetree.{
            pexp_desc = Pexp_constant (Pconst_string (freeString, None));
            pexp_loc = Location.none;
            pexp_attributes = []
        } in
        match expressions, formatAfterVar with
        | [], _ -> freeString
        | _, true -> apply op_concat [(Asttypes.Nolabel,freeString); (Asttypes.Nolabel,sprintf_applied)]
        | _, false -> apply op_concat [(Asttypes.Nolabel,sprintf_applied); (Asttypes.Nolabel,freeString)]
    )