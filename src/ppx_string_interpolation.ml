open Ppxlib

let expand ~(loc:Location.t) ~(path:string) (str:string) : expression =
  let parsed = Interpolation_parser.Parser.from_string str in
  let intermediate = Interpolation_intermediate.parser_to_emitter parsed in
  let ast = Interpolation_emitter.emit_ast intermediate in
  { pexp_desc = ast.pexp_desc;
    pexp_loc = Lexing.{ loc_start = dummy_pos; loc_end = dummy_pos; loc_ghost = false};
    pexp_attributes = []}

let extension = Extension.declare "string"
                    Extension.Context.Expression
                    Ast_pattern.(single_expr_payload (estring __)) expand

let () = Ppxlib.Driver.register_transformation "string_interpolation"
                              ~rules:[Context_free.Rule.extension extension]

(*
(* Now registering Value Format transformations -- remove later *)
let expand ~(loc:Location.t) ~(path:string) (str:string) : expression =
  let parsed = String_interpolation_parser_vf.Parser.parse_string str in
  let ast = String_interpolation_emitter.generate true parsed in
  { pexp_desc = ast.pexp_desc;
    pexp_loc = Lexing.{ loc_start = dummy_pos; loc_end = dummy_pos; loc_ghost = false};
    pexp_attributes = []}

let extension = Extension.declare "stringVF"
                    Extension.Context.Expression
                    Ast_pattern.(single_expr_payload (estring __)) expand

let () = Ppxlib.Driver.register_transformation "string_interpolationValueFormat"
                              ~rules:[Context_free.Rule.extension extension]
*)
