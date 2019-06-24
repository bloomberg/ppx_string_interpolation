open Ppxlib

let expand ~(loc:Location.t) ~path:_ (expr:expression) : expression =
    let str, loc = (* payload and location of the string contents, inside "" or {||} *)
        let adjust shift loc =
            let adjust shift p = {p with Lexing.pos_cnum = p.pos_cnum + shift} in
            {loc with Location.loc_start = adjust   shift  loc.loc_start;
                      Location.loc_end   = adjust (-shift) loc.loc_end}
        in
        match expr.pexp_desc with
        | Pexp_constant (Pconst_string (str, None)) ->
            str, adjust 1 expr.pexp_loc
        | Pexp_constant (Pconst_string (str, Some x)) ->
            str, adjust (String.length x + 2) expr.pexp_loc
        | _ -> Location.raise_errorf ~loc "Expecting string payload"
    in
    let parsed = Interpolation_parser.Parser.from_string ~payload_loc:loc str in
    let intermediate = Interpolation_intermediate.parser_to_emitter parsed in
    let ast = Interpolation_emitter.emit_ast intermediate in
    Pprintast.expression Format.std_formatter ast;
    ast

let extension = Extension.declare "string"
                    Extension.Context.Expression
                    Ast_pattern.(single_expr_payload __) expand

let () = Ppxlib.Driver.register_transformation "string_interpolation"
                              ~rules:[Context_free.Rule.extension extension]
