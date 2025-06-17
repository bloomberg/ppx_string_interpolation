open Ppxlib
open Ppxlib.Ast_builder.Default

type element = string * Location.t

type token =
  | String of element
  | EscapedPercent of Location.t
  | Expression of element * element option
  | Variable of element * element option

let token_to_string = function
  | String (s, _) ->
    s
  | Expression ((e, _), _) ->
    "{" ^ e ^ "}"
  | Variable ((v, _), _) ->
    "[" ^ v ^ "]"
  | EscapedPercent _ -> "%"

(* Debug printing of tokens. Compresses %% into % *)
let print_tokens = List.iter (fun p -> print_string (token_to_string p))

(* Convert commented out expression to a string.
   Here we also rely on UTF8/single codepage - '(','*' and ')' occupy only one byte. *)
let convert_commented_out = function
  | Expression ((str, loc), fmt) ->
    if String.length str >= 4 && str.[1] = '*' && str.[String.length str - 2] = '*' then (
      match fmt with
      | Some (fmt, _) ->
        String ("%" ^ fmt ^ "$" ^ str, loc)
      | None ->
        String ("$" ^ str, loc))
    else
      Expression ((str, loc), fmt)
  | x ->
    x

(* Generate a list of sprintf arguments from tokens. *)
let to_arguments tokens =
  let shift by ({ Location.loc_start; _ } as loc) =
    { loc with
      Location.loc_start = { loc_start with Lexing.pos_cnum = loc_start.pos_cnum + by }
    }
  in
  List.rev
  @@ List.fold_left
       (fun acc token ->
         match token with
         | Expression ((e, loc), _) ->
           let lexbuf = Lexing.from_string e in
           let open Lexing in
           let open Location in
           lexbuf.lex_curr_p <- loc.loc_start;
           lexbuf.lex_abs_pos <- loc.loc_start.pos_cnum + 1;
           (Nolabel, Parse.expression lexbuf) :: acc
         | Variable ((v, loc), _) ->
           let loc = shift 1 loc in
           (Nolabel, pexp_ident ~loc { txt = Lident v; loc }) :: acc
         | _ ->
           acc)
       []
       tokens

(* Check invalid format before composing whole string to report correct location. *)
let verify_formats tokens =
  let check fmt loc =
    try
      let _ = CamlinternalFormat.fmt_ebb_of_string fmt in
      ()
    with
    | Failure msg ->
      Location.raise_errorf ~loc "%s" msg
    | _ ->
      ()
  in
  List.iter
    (fun token ->
      match token with
      | Expression (_, Some (fmt, loc)) ->
        check fmt loc
      | Variable (_, Some (fmt, loc)) ->
        check fmt loc
      | _ ->
        ())
    tokens

(* Generate format string for sprintf from tokens. NOTE, we always manually compress "$$"
   into "$" since [sprintf] does not treat is specially. However, "%%" is compressed into "%"
   only if the string does not contain interpolations, and therefore [sprintf] call is not emitted. *)
let to_format_string ~escape_percent tokens =
  let joined =
    String.concat ""
    @@ List.rev
    @@ List.fold_left
         (fun acc token ->
           match token with
           | Expression (_, Some (fmt, _)) ->
             fmt :: acc
           | Expression (_, None) ->
             "%s" :: acc
           | Variable (_, Some (fmt, _)) ->
             fmt :: acc
           | Variable (_, None) ->
             "%s" :: acc
           | String (s, _) ->
             s :: acc
           | EscapedPercent _ when escape_percent -> "%%" :: acc
           | EscapedPercent _ -> "%" :: acc)
         []
         tokens
  in
  pexp_constant ~loc:Location.none (Pconst_string (joined, Location.none, None))

(* Convert list of expressions with formats to ast. *)
let generate tokens =
  let sprintf =
    let open Longident in
    pexp_ident
      ~loc:Location.none
      { txt = Ldot (Lident "Printf", "sprintf"); loc = Location.none }
  in
  let apply func args = pexp_apply ~loc:Location.none func args in
  match to_arguments tokens with
  | [] -> to_format_string ~escape_percent:false tokens
  | args -> apply sprintf @@
    ((Nolabel, to_format_string ~escape_percent:true tokens) :: args)

let emit_ast tokens =
  verify_formats tokens;
  List.map convert_commented_out tokens |> generate
