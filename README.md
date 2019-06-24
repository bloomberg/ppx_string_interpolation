ppx_string_interpolation
========================

PPX rewriter that enables string interpolation.

Basic Usage
-----------

Plugin uses bash-style syntax for string interpolation and
the type of variables and expressions is assumed to be string
by-default:
```ocaml
let name = "world" in
[%string "Hello $name!"]
```

Embedded expressions should be enclosed in parentheses:
```ocaml
let hello = "Hello" and world = "world" in
[%string {|$(hello ^ " " ^ world)!|}]
```

Non-string types
----------------

Since PPX run before type inference and type checking, rewriter should
have the information about the types of embedded values via `Printf.printf`
format specifiers:
```ocaml
let a = 1 and b = 1.0 in
    [%string {|We know, that %d$a == %f$b is %b$(a = int_of_float b)!|}]
```
