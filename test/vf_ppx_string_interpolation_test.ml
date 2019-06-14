let%test _ = "HI" = "HI"

let%test _ = [%stringVF {|Hello!$(1)%d|}] = "Hello!1"

let%test _ = "Hello!" ^ "1" = "Hello!1"

let%test _ = [%stringVF "Hello!$(1 + 2 + 3)%d"] = "Hello!6"

let%test _ = let name = "Me!" in [%stringVF "Hello, my name is $name! Good bye, $name."] = "Hello, my name is Me!! Good bye, Me!."

let%test _ = let a = 1 and b = 1.0 in
    [%stringVF {|We know, that $a%d == $b%f is $(a = int_of_float b)%b!|}] = "We know, that 1 == 1.000000 is true!"

let%test _ = [%stringVF "Hello!"] = "Hello!"

let%test _ = [%stringVF {|Hello!$("1")|}] = "Hello!1"

let%test _ = let l = "l" in
    [%stringVF {|A $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l $l|}] = 
    "A l l l l l l l l l l l l l l l l l l l l l l l l l l"

let%test _ = let l = "*" in
    [%stringVF {|$("(" ^ l ^ (* HI *) ")")|}] = "(*)"

