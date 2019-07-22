open OUnit2

(* Examples from README.md *)
let readme_test1 _ =
  let name = "world" in
  assert_equal [%string "Hello $name!"] "Hello world!"

let readme_test2 _ =
  let hello = "Hello"
  and world = "world" in
  assert_equal [%string "$(hello ^ \" \" ^ world)!"] "Hello world!"

let readme_test3 _ = assert_equal [%string "This is $$ and %%!"] "This is $ and %!"

let readme_test4 _ =
  let a = 1
  and b = 1.0 in
  assert_equal
    [%string {|We know, that %d$a == %f$b is %b$(a = int_of_float b)!|}]
    "We know, that 1 == 1.000000 is true!"

let readme_test5 _ =
  assert_equal
    [%string "The first pythagorean triple: %d$(3), 4, %d$(*5?*)"]
    "The first pythagorean triple: 3, 4, %d$(*5?*)"

(* Other tests *)
let test1 _ = assert_equal [%string "Hello!"] "Hello!"
let test2 _ = assert_equal [%string {j|Hello!%d$(1)|j}] "Hello!1"
let test3 _ = assert_equal ("Hello!" ^ "1") "Hello!1"
let test4 _ = assert_equal [%string "Hello!%d$(1 + 2 + 3)"] "Hello!6"

let test5 _ =
  let name = "Me!" in
  assert_equal
    [%string "Hello, my name is $name! Good bye, $name."]
    "Hello, my name is Me!! Good bye, Me!."

let test6 _ = assert_equal [%string {key|Hello!|key}] "Hello!"
let test7 _ = assert_equal [%string {|Hello!$("1")|}] "Hello!1"

let test8 _ =
  let l = "l" in
  assert_equal
    [%string
      {|A $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l|}]
    "A l l l l l l l l l l l l l l l l l l l l l l l l l l"

let test9 _ =
  let l = "*" in
  assert_equal [%string {|$("(" ^ l ^ (* HI *) ")")|}] "(*)"

let () =
  let suite =
    "tests"
    >::: [ "Test 1 from README" >:: readme_test1
         ; "Test 2 from README" >:: readme_test2
         ; "Test 3 from README" >:: readme_test3
         ; "Test 4 from README" >:: readme_test4
         ; "Test 5 from README" >:: readme_test5
         ; "Test1 " >:: test1
         ; "Test2 " >:: test2
         ; "Test3 " >:: test3
         ; "Test4 " >:: test4
         ; "Test5 " >:: test5
         ; "Test6 " >:: test6
         ; "Test7 " >:: test7
         ; "Test8 " >:: test8
         ; "Test9 " >:: test9
         ]
  in
  run_test_tt_main suite
