# Replace patterns within expressions

Match and replace elements of patterns in an
[expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
or a list of expressions.

## Usage

``` r
expr_replace(expr, ..., patterns, replacements,
    n = Inf, env = parent.frame())
```

## Arguments

- expr:

  Input. An
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md),
  [expr_list](https://nicholasdavies.github.io/elixir/reference/expr_list.md),
  or [`list()`](https://rdrr.io/r/base/list.html) of expressions. Also
  works with [formulas](https://rdrr.io/r/base/tilde.html) or lists of
  formulas.

- ...:

  Alternating series of patterns and replacements. Each pattern should
  be a single
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
  (though alternatives can be specified with `?`). Each replacement
  should be either a single expression or a function (see *Function
  replacements*).

- patterns:

  Patterns to look for. An
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md),
  [expr_list](https://nicholasdavies.github.io/elixir/reference/expr_list.md),
  or [`list()`](https://rdrr.io/r/base/list.html) of expressions.

- replacements:

  Replacements, one for each pattern. Each can be an
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
  or a function (see *Function replacements*).

- n:

  Maximum number of times for each expression to make each replacement;
  default is `Inf`.

- env:

  Environment for injections in `expr`, `pattern` (see
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)).

## Value

The input expression(s) with any replacements made.

## Details

Patterns follow the syntax for
[`expr_match()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md).

## Function replacements

Normally, each replacement is an expression template with capture names
(like `.A` or `..B`) standing in for the matched parts, e.g.
`{ .A + ..B }`. But if a replacement is a function, it is called for
each match and the return value is used as the replacement, e.g.
`function(m) substitute(A - B, m)`.

The function takes a single argument: a named list containing the match
details. This list has elements `match` (the matched expression), `loc`
(its location), and all captures named without leading dots. For
example, with the pattern `{ ..A ^ .N }` matching `x^2`, the function
receives:

    list(match = quote(x^2), loc = ..., A = quote(x), N = 2)

To build a new expression from captures, you can pass the list as the
second argument to
[`base::substitute()`](https://rdrr.io/r/base/substitute.html). A full
example:

    expr_replace({ x^2 + y^2 + z^3 }, { ..A ^ .N },
        function(m) {
            if (m$N == 2) substitute(square(A), m) else substitute(A ^ N, m)
        })
    # => square(x) + square(y) + z^3

## See also

[`expr_match()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md)
to find patterns in expressions, and its cousins
[`expr_count()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
[`expr_detect()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
[`expr_extract()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
and
[`expr_locate()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md).

## Examples

``` r
# Example with alternating patterns and replacements
expr_replace({ 1 + 2 }, {1}, {one}, {2}, {two})
#> one + two

# Example with patterns and replacements in a list
expr_replace({ 1 + 2 }, patterns = expr_list({1}, {2}),
    replacements = expr_list({one}, {two}))
#> one + two

# Replace with captures
expr_replace({ 1 + 2 }, ~{ .A + .B }, { .A - .B })
#> 1 - 2

# Function replacement with conditional logic
expr_replace({ x^2 + y^2 + z^3 }, { ..A ^ .N },
    function(m) {
        if (m$N == 2) substitute(square(A), m) else substitute(A ^ N, m)
    })
#> square(x) + square(y) + z^3
```
