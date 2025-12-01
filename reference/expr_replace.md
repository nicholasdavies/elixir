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

  Alternating series of patterns and replacements, each a single
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
  (though alternatives can be specified with `?`).

- patterns:

  Patterns to look for. An
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md),
  [expr_list](https://nicholasdavies.github.io/elixir/reference/expr_list.md),
  or [`list()`](https://rdrr.io/r/base/list.html) of expressions.

- replacements:

  Replacements, one for each pattern.

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
```
