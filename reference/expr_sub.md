# Get or set a subexpression

These functions allow you to extract and/or modify a subexpression
within an expression.

## Usage

``` r
expr_sub(expr, idx, env = parent.frame())

expr_sub(expr, idx, env = parent.frame()) <- value
```

## Arguments

- expr:

  The expression to select from. Can also be a list of expressions, in
  which case the first element of `index` selects the expression from
  the list. Can also be a formula.

- idx:

  A valid index: `NULL` or an integer vector.

- env:

  Environment for any injections in `expr` (see
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)).

- value:

  Replacement; an expression.

## Value

The element of the expression selected by `idx`.

## Details

The `elixir` functions
[`expr_match()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md)
and
[`expr_locate()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md)
find matching "subexpressions" within expressions and return indices
that allow accessing these subexpressions. For example, the expression
`1 + 2 + 3` contains all the following subexpressions:

|           |                   |                                       |
|-----------|-------------------|---------------------------------------|
| **index** | **subexpression** | **accessed with R code**              |
| `NULL`    | `1+2+3`           | `expr`                                |
| `1`       | `+`               | `expr[[1]]`                           |
| `2`       | `1+2`             | `expr[[2]]`                           |
| `3`       | `3`               | `expr[[3]]`                           |
| `c(2,1)`  | `+`               | `expr[[2]][[1]]` or `expr[[c(2, 1)]]` |
| `c(2,2)`  | `1`               | `expr[[2]][[2]]` or `expr[[c(2, 2)]]` |
| `c(2,3)`  | `2`               | `expr[[2]][[3]]` or `expr[[c(2, 3)]]` |

Any index returned by
[`expr_match()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md)
or
[`expr_locate()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md)
will either be `NULL` (meaning the whole expression / expression list)
or an integer vector (e.g. `1` or `c(2,3)` in the table above).

Suppose you have an index, `idx`. If `idx` is an integer vector, you can
just use `expr[[idx]]` to access the subexpression. But in the case
where `idx` is `NULL`, R will complain that you are trying to select
less than one element. The sole purpose of `expr_sub()` is to get around
that issue and allow you to pass either `NULL` or an integer vector as
the index you want for an expression or list of expressions.

## See also

[`expr_match()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
[`expr_locate()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md)
which return indices to subexpressions.

## Examples

``` r
expr = quote(y == a * x + b)
expr_sub(expr, NULL)
#> y == a * x + b
expr_sub(expr, 3)
#> a * x + b
expr_sub(expr, c(3, 3))
#> b

expr_sub(expr, c(3, 3)) <- quote(q)
print(expr)
#> y == a * x + q
```
