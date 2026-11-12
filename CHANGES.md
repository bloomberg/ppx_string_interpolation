1.2.0 (11/12/2026)
------------------

- A format specifier at the end of a string (for example `[%string "50% off"]`)
  now reports "Format is not followed by variable/expression" instead of
  silently dropping the rest of the string. Use `%%` for a literal percent sign.
- Require OCaml >= 4.11.0 and declare ounit as a test dependency

1.1.0 (06/30/2025)
------------------

- Fix incorrect collapse of `%%` and `$$`
- Correct examples in README

1.0.1 (01/28/2021)
------------------

- Support the new version of ppxlib (requires ppxlib >= 0.18.0)
- Minor cleanups in README

1.0.0 (12/04/2019)
------------------

- Initial release
