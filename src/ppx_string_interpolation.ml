open Ppxlib
open Pprintast

let expand ~(loc:Location.t) ~(path:string) (str:string) : expression =
  let parsed = Interpolation_parser.Parser.from_string loc str in
  let intermediate = Interpolation_intermediate.parser_to_emitter parsed in
  let ast = Interpolation_emitter.emit_ast intermediate in
  Pprintast.expression Format.std_formatter ast;
  ast

let extension = Extension.declare "string"
                    Extension.Context.Expression
                    Ast_pattern.(single_expr_payload (estring __)) expand

let () = Ppxlib.Driver.register_transformation "string_interpolation"
                              ~rules:[Context_free.Rule.extension extension]
