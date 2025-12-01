# Apply a function over expressions

Recursively apply a function over an
[expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md),
or any expression elements of a list, and optionally the subexpressions
within any expressions.

## Usage

``` r
expr_apply(
  x,
  f,
  depth = Inf,
  into = FALSE,
  order = c("pre", "post"),
  how = c("replace", "unlist", "unique"),
  env = parent.frame()
)
```

## Arguments

- x:

  The R object; can be an
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md),
  a [formula](https://rdrr.io/r/base/tilde.html), or a list of arbitrary
  nestedness potentially containing multiple expressions or formulas.

- f:

  Function to apply to all expressions within `x`; takes 1 to 3
  arguments.

- depth:

  How many levels to recurse into lists; default is `Inf`.

- into:

  Whether to recurse into expressions. Can be `TRUE` to visit all
  subexpressions, `FALSE` to not recurse, or `"leaves"` to recurse and
  only apply `f` to terminal nodes of expressions (i.e. the symbols and
  syntactic literals comprising the expressions).

- order:

  Whether a parent node is visited before ("pre") or after ("post") its
  children (the terminology comes from pre-order and post-order
  depth-first search). This only has an effect if `into == TRUE`.

- how:

  How to structure the result.

- env:

  Environment for injections in `x` (see
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)).

## Value

If `how = "replace"` (the default), the original object `x` with `f`
applied to expressions within it. If `how = "unlist"`, the same but with
[`unlist()`](https://rdrr.io/r/base/unlist.html) applied to it. If
`how = "unique"`, first [`unlist()`](https://rdrr.io/r/base/unlist.html)
then [`unique()`](https://rdrr.io/r/base/unique.html) are applied.

## Details

The function `f` can take one to three arguments. The first argument is
the expression itself for `f` to apply to, and `f` should return some
kind of replacement for, or modified version of, this argument.

The second argument is a list with information about the name of the
expression in the list `x` and of its parents. Specifically, the first
element of the list is the name of the expression, the second element of
the list is the name of the "parent" of the expression, and so on. If
any elements in this chain are unnamed, an integer is provided as the
name. If the expression is within another expression (which only happens
with `into = TRUE`), this is signalled as a `NULL` at the top of the
list, one for each level of recursion into the expression.

The third argument is an integer vector, the index into `x` where `f` is
currently operating. This is suitable for use with
[`expr_sub()`](https://nicholasdavies.github.io/elixir/reference/expr_sub.md).

## Examples

``` r
expr_apply(list(quote(a + b), quote(c)), function(x) all.vars(x), how = "unlist")
#> [1] "a" "b" "c"
```
