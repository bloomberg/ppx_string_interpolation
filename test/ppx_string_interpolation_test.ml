let%test _ = "HI" = "HI"

let%test _ = [%string "Hello!6"] = "Hello!6"

let%test _ = [%string {j|Hello!%d$(1)|j}] = "Hello!1"

let%test _ = "Hello!" ^ "1" = "Hello!1"

let%test _ = [%string "Hello!%d$(1 + 2 + 3)"] = "Hello!6"

let%test _ = let name = "Me!" in [%string "Hello, my name is $name! Good bye, $name."] = "Hello, my name is Me!! Good bye, Me!."

let%test _ = let a = 1 and b = 1.0 in
    [%string {|We know, that %d$a == %f$b is %b$(a = int_of_float b)!|}] = "We know, that 1 == 1.000000 is true!"

let%test _ = [%string "Hello!"] = "Hello!"

let%test _ = [%string {|Hello!$("1")|}] = "Hello!1"

let%test _ = let l = "l" in
    [%string {|A $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l|}] =
    "A l l l l l l l l l l l l l l l l l l l l l l l l l l"

let%test _ = let l = "*" in
    [%string {|$("(" ^ l ^ (* HI *) ")")|}] = "(*)"
