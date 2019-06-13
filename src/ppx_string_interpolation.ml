open Ppxlib

open Parse_string_vf

let expand ~(loc:Location.t) ~(path:string) (str:string) : expression =
  let parsed = parse_string str in
  let ast = Generate_ast.prefix_expressions_to_ast true parsed in
  { pexp_desc = ast.pexp_desc;
    pexp_loc = Lexing.{ loc_start = dummy_pos; loc_end = dummy_pos; loc_ghost = false};
    pexp_attributes = []}

let extension = Extension.declare "string" 
                    Extension.Context.Expression 
                    Ast_pattern.(single_expr_payload (estring __)) expand

let () = Ppxlib.Driver.register_transformation "string_interpolation" 
                              ~rules:[Context_free.Rule.extension extension]
