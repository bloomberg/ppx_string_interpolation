open Ppxlib

open Parse_string_fv

let expand ~(loc:Location.t) ~(path:string) (str:string) : expression =
  let parsed = Parse_string_fv.parse_string str in
  let ast = Generate_ast.prefix_expressions_to_ast true parsed in
  { pexp_desc = ast.pexp_desc;
    pexp_loc = Lexing.{ loc_start = dummy_pos; loc_end = dummy_pos; loc_ghost = false};
    pexp_attributes = []}

let extension = Extension.declare "string"
                    Extension.Context.Expression
                    Ast_pattern.(single_expr_payload (estring __)) expand

let () = Ppxlib.Driver.register_transformation "string_interpolation"
                              ~rules:[Context_free.Rule.extension extension]

open Parse_string_vf
(* Now registering Value Format transformations -- remove later *)
let expand ~(loc:Location.t) ~(path:string) (str:string) : expression =
  let parsed = Parse_string_vf.parse_string str in
  let ast = Generate_ast.prefix_expressions_to_ast true parsed in
  { pexp_desc = ast.pexp_desc;
    pexp_loc = Lexing.{ loc_start = dummy_pos; loc_end = dummy_pos; loc_ghost = false};
    pexp_attributes = []}

let extension = Extension.declare "stringVF"
                    Extension.Context.Expression
                    Ast_pattern.(single_expr_payload (estring __)) expand

let () = Ppxlib.Driver.register_transformation "string_interpolationValueFormat"
                              ~rules:[Context_free.Rule.extension extension]
