# Translate an R expression

Takes an R expression (in the sense of
[`rlang::is_expression()`](https://rlang.r-lib.org/reference/is_expression.html))
and translates it into a character string giving the equivalent
expression in another programming language, according to the supplied
[rules](https://nicholasdavies.github.io/elixir/reference/elixir-rules.md).

This function is experimental.

## Usage

``` r
translate(expr, rules, env = parent.frame())
```

## Arguments

- expr:

  [Expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
  or list of
  [expressions](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
  to be translated.

- rules:

  Which
  [rules](https://nicholasdavies.github.io/elixir/reference/elixir-rules.md)
  to follow. You can pass a string from among `"C"`, `"C++"`, `"Lua"`,
  or `"R"`, or a list with translation rules (see Details).

- env:

  Environment for injections in `expr` (see
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)).

## Value

The translated expression as a single character string.

## Details

The parameter `rules` can be a character string naming a "built-in"
[ruleset](https://nicholasdavies.github.io/elixir/reference/elixir-rules.md).
Otherwise, `rules` should be a list with the following elements:

- `ops`: an unnamed list of operator definitions, each of which should
  be a list with four elements:

  - `arity` the number of operands

  - `prec` the precedence of the operator (lower numbers equal higher
    precedence)

  - `assoc` the associativity of the operator, either `"LTR"`, `"RTL"`,
    or anything else for no associativity

  - `str` a
    [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
    format string with `{A[1]}`, `{A[2]}`, etc., standing in for the
    first, second, etc. operands.

  - `nopar` a numeric vector with indices of arguments to the operator
    which should never be enclosed in parentheses. The default and usual
    value is integer(0), but (for example) it can be 2 for the `[`
    operator, as parentheses within the second argument (the content of
    the brackets) are redundant.

  The function `elixir:::op` can help to assemble such lists.

- `paren` a
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  format string with `{x}` standing in for the enclosed expression.
  Describes how parentheses are expressed in the target language.
  Example: `"({x})"` is correct for virtually all programming languages.

- `symbol`: a function which takes a symbol and returns a character
  string, representing the name of that symbol in the target language.
  This could just be equal to
  [base::as.character](https://rdrr.io/r/base/character.html), but it
  can be changed to something else in case you want name mangling, or
  e.g. some processing to replace `.` in symbols with some other
  character (as `.` are often not allowed as part of symbols in popular
  languages).

- `literal`: a named list in which the name refers to the class of the
  operand to translate, and the value should be a function of a single
  argument (the operand) returning a character string.

It may be helpful to inspect `elixir:::ruleset` to clarify the above
format.

There are some important shortcomings to `translate()`. Here are some
potential pitfalls:

- Named arguments are not supported, because we cannot translate an R
  function call like `mean(x, na.rm = TRUE)` without knowing which
  parameter of `mean` matches to `na.rm`.

- Division: An R expression like `1/3` gets translated into `1./3.` in
  C/C++, as numeric literals are coerced to type `double`. So both of
  these evaluate to 0.333. However, the R expression `1L/3L` will get
  translated into `1/3` in C/C++, which evaluates to 0 (as it is integer
  division).

- Modulo: R uses "Knuth's modulo", where `a %% b` has the same sign as
  `b`. Lua also uses Knuth's modulo, but C/C++ use "truncated modulo",
  where `a % b` has the same sign as `a`. (see
  [Wikipedia](https://en.wikipedia.org/wiki/Modulo#Variants_of_the_definition)
  for details). So when converting a modulo expression from R to C/C++,
  a call to the function `kmod` is generated in the C/C++ expression.
  This is not a standard library function, so you will have to provide a
  definition yourself. A workable definition is:
  `double kmod(double x, double y) { double r = fmod(x, y); return r && r < 0 != y < 0 ? r + y : r; }`

- Types: In R, the type of `a %% b` and of `a %/% b` depends on the type
  of `a` and `b` (if both are integers, the result is an integer; if at
  least one is numeric, the result is numeric).

- Chained assignment does not work in Lua.

## Examples

``` r
translate({x ^ y}, "C++")
#> pow(x, y)
```
