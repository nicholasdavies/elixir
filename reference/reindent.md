# Reindent some lines of code

Using some fairly unsophisticated metrics, `reindent()` will take some
lines of code and, according to its understanding of the rules for that
language, reindent those lines. This is intended to help prettify
automatically generated code.

This function is experimental.

## Usage

``` r
reindent(lines, rules, tab = "    ", start = 0L)
```

## Arguments

- lines:

  Character vector with lines of text; can have internal newlines.

- rules:

  Which
  [rules](https://nicholasdavies.github.io/elixir/reference/elixir-rules.md)
  to follow. You can pass a string from among `"C"`, `"C++"`, `"Lua"`,
  or `"R"`, or a list with elements:

  - `indent_more` Character vector of tokens which increase the indent
    level.

  - `indent_less` Character vector of tokens which decrease the indent
    level.

  - `indent_both` Character vector of tokens which decrease, then
    increase the indent level (see Details).

  - `ignore` Comment and string literal delimiters (see Details).

- tab:

  Character string; what to use as an indent.

- start:

  Indent level to start at.

## Value

Reindented lines as a character vector.

## Details

Conceptually, the function first ignores any comments or string
literals. Then, line by line, `reindent` looks for tokens that signal
either an increase in the indent level, a decrease in the indent level,
or both at the same time. For example, in this Lua code:

    if x == 1 then
        print 'one'
    else
        print 'not one'
    end

the `if` keyword increases the indent level, the `else` keyword both
decreases and increases the indent level, and the `end` keyword
decreases the indent level.

If provided, the `ignore` element of `rules` should be a list of
character vectors. A character vector of length one is assumed to start
a comment that runs to the end of the line (e.g. `"#"` in R). If length
two, the two symbols are assumed to start and end a comment or string
(e.g. `"/*"` and `"*/"` in C). If length three, then the first two
symbols are start and end delimiters of a comment or string, while the
third symbol is an "escape" character that escapes the end delimiter
(and can also escape itself). This is typically a backslash.

`reindent()` supports "raw strings" in R, C, C++, and Lua code but only
in limited cases. In R, when using [raw character
constants](https://rdrr.io/r/base/Quotes.html) you must use an uppercase
`R`, the double quote symbol and zero to two hyphens. In C/C++, when
using [raw string
literals](https://en.cppreference.com/w/cpp/language/string_literal.html)
you must use the prefix `R`, and zero to two hyphens as the delimiter
char sequence (plus parentheses). In Lua, you can use [long
brackets](https://www.lua.org/manual/5.2/manual.html#3.1) with zero to
two equals signs. Any other attempt to use raw strings will probably
break `reindent()`.

Other unusual character sequences may also break `reindent()`.

## Examples

``` r
reindent(
    c(
        "if x == 1 then",
        "print 'one'",
        "else",
        "print 'not one'",
        "end"
    ),
    rules = "Lua")
#> [1] "if x == 1 then"      "    print 'one'"     "else"               
#> [4] "    print 'not one'" "end"                
```
