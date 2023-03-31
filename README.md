# PPX rewriter that enables string interpolation.

## Menu

- [Rationale](#rationale)
- [Quick start](#quick-start)
- [Building](#building)
- [Installation](#installation)
- [Contributions](#contributions)
- [License](#license)
- [Code of Conduct](#code-of-conduct)
- [Security Vulnerability Reporting](#security-vulnerability-reporting)

## Rationale

This PPX implements string interpolation with embedded expressions,
thus enabling simple and powerful templates for code generation and
pretty printing. In contrast to `printf`, string interpolation does not
separate embedded variables and context, which is important for
medium-size strings (several lines).

We tried our best to be compatible with future Compiler versions,
so please do not be surprised if the package is not updated for
a couple years — it works. Please feel free to file an ISSUE if
the Compiler update breaks this PPX.

## Quick Start

Plugin uses bash-style syntax for string interpolation and
the type of variables and expressions is assumed to be string
by default:
```ocaml
let name = "world" in
    [%string "Hello $name!"]
```

Embedded expressions should be enclosed in parentheses:
```ocaml
let hello = "Hello" and world = "world" in
    [%string "$(hello ^ \" \" ^ world)!"]
```

### Non-string types

Since this PPX run before type inference and type checking, rewriter should
have the information about the types of embedded values via `Printf.printf`
format specifiers:
```ocaml
let a = 1 and b = 1.0 in
    [%string {|We know, that %d$a == %f$b is %b$(a = int_of_float b)!|}]
```

### Advanced usage

The extension is essentially syntax sugar for the `Printf.sprintf` function.
The format is assumed to be anything between `%` and the next `$`. The expression
string is determined by searching for balanced parentheses.

Both `$` and `%` symbols can be escaped, i.e. double `%` and
double `$` result in single characters:

```ocaml
[%string "This is $$ and %%!"] = "This is $ and %!"
```

If the expression is commented out, the PPX dumps the comment with format,
without checking for the type of the format:

```ocaml
[%string "The first pythagorean triple: %d$(3), 4, %d$(*5?*)"] =
    "The first pythagorean triple: 3, 4, %d$(*5?*)"
```

## Building

To build the project use `dune build`.

## Installation

To install the package run `dune install`.

## Contributions

We :heart: contributions.

Have you had a good experience with this project? 
Why not share some love and contribute code, or just let us know about any issues you had with it?

We welcome issue reports [here](../../issues); be sure to choose the 
proper issue template for your issue, so that we can be sure you're providing 
the necessary information.

Before sending a [Pull Request](../../pulls), please make sure you read our
[Contribution Guidelines](https://github.com/bloomberg/.github/blob/master/CONTRIBUTING.md).

## License

Please read the [LICENSE](LICENSE) file.

## Code of Conduct

This project has adopted a [Code of Conduct](https://github.com/bloomberg/.github/blob/master/CODE_OF_CONDUCT.md).
If you have any concerns about the Code, or behavior which you have experienced in the project, please
contact us at opensource@bloomberg.net.

## Security Vulnerability Reporting

If you believe you have identified a security vulnerability in this project, please send email to the project
team at opensource@bloomberg.net, detailing the suspected issue and any methods you've found to reproduce it.

Please do NOT open an issue in the GitHub repository, as we'd prefer to keep vulnerability reports private until
we've had an opportunity to review and address them.
