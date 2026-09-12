# Changelog

## elixir (development version)

- Non-ASCII tokens are now recognised by reindent().
- Indentation levels can now be “compressed” in reindent().
- Fixes to parenthesis and \[\[ behaviour in translate().

## elixir 0.1.2

CRAN release: 2026-09-02

- Extended expr_replace() to accept functions as replacements.
- Fixed a bug in expr_replace() that would sometimes not replace all
  instances of a pattern.
- Added parameter subloc to expr_match().

## elixir 0.1.1

CRAN release: 2025-12-02

- elixir now works with formulas.
- Removed dependencies on data.table and methods packages.

## elixir 0.1.0

CRAN release: 2025-09-24

- Initial CRAN submission.
