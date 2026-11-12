open OUnit2

let expand src =
  let expr = Ppxlib.Parse.expression (Lexing.from_string src) in
  let loc = expr.pexp_loc in
  match
    Ppxlib.Driver.map_structure [ Ppxlib.Ast_builder.Default.pstr_eval ~loc expr [] ]
  with
  | [ { pstr_desc = Pstr_eval (expr, _); _ } ] ->
    expr
  | _ ->
    assert_failure ("Unexpected expansion for: " ^ src)

let assert_error ~msg src =
  match expand src with
  | _ ->
    assert_failure ("Expected a compile error for: " ^ src)
  | exception Ppxlib.Location.Error e ->
    assert_equal ~printer:(fun s -> s) msg (Ppxlib.Location.Error.message e)

let assert_expands ~expected src =
  assert_equal
    ~printer:(fun s -> s)
    expected
    (Ppxlib.Pprintast.string_of_expression (expand src))

let format_error = "Format is not followed by variable/expression. Missing %?"

(* Format not followed by variable/expression *)
let error_test1 _ = assert_error ~msg:format_error {|[%string "50% off $$ today"]|}
let error_test2 _ = assert_error ~msg:format_error {|[%string "%d"]|}
let error_test3 _ = assert_error ~msg:format_error {|[%string "Sale: 50% off today"]|}

let valid_test1 _ =
  assert_expands ~expected:{|"Sale: 50% off today"|} {|[%string "Sale: 50%% off today"]|}

let valid_test2 _ =
  assert_expands ~expected:{|Printf.sprintf "%d" x|} {|[%string "%d$x"]|}

let valid_test3 _ =
  assert_expands ~expected:{|Printf.sprintf "%d%%" x|} {|[%string "%d$x%%"]|}

let () =
  let suite =
    "errors"
    >::: [ "Error test1 " >:: error_test1
         ; "Error test2 " >:: error_test2
         ; "Error test3 " >:: error_test3
         ; "Valid test1 " >:: valid_test1
         ; "Valid test2 " >:: valid_test2
         ; "Valid test3 " >:: valid_test3
         ]
  in
  run_test_tt_main suite
