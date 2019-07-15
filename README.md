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
[%string "$(hello ^ \" \" ^ world)!"]
```

Both `$` and `%` symbols are escaped, i.e. double `%` and
double `$` result in single character:

```ocaml
[%string "This is $$ and %%!"] = "This is $ and %!"
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

Advanced
--------

The extension is essentially a syntax sugar for `Printf.sprintf` function.
The format assumed to be anything between `%` and next `$`. The expression
string is determined by searching for balanced parentheses.

If the expression is commented out, the PPX dumps the comment with format,
without checking for the type of the format:

```ocaml
[%string "The first pythagorean triple: %d$(3), 4, %d$(*5?*)"] =
    "The first pythagorean triple: 3, 4, %d$(*5?*)"]
```
