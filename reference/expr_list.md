# Make a list of expressions

Constructs a list of expressions, with support for `elixir`'s special
[expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
syntax (expression literals with
[`{}`](https://rdrr.io/r/base/Paren.html) or `~{}`, and alternatives
with `?`).

## Usage

``` r
expr_list(..., env = parent.frame())

# S3 method for class 'expr_list'
xl[i]

# S3 method for class 'expr_list'
xl[i] <- value
```

## Arguments

- ...:

  [Expressions](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
  to include in the list. If the arguments are named, these will be
  passed on to the returned list.

- env:

  Environment for injections in `...` (see
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)).

- xl:

  An `expr_list`.

- i:

  Index for subsetting the `expr_list`; an integer, numeric, logical, or
  character vector (for named `expr_list`s) interpreted in the usual R
  way.

- value:

  Replacement; an `expr_list`, an expression, or a list of expressions.

## Value

A list of expressions, of class `expr_list`.

## Details

Be aware that using the `[[` indexing operator on an object of class
`expr_list` discards information about whether that element of the list
is marked as anchored. In other words, if
`xl <- expr_list({.A}, ~{.A})`, then `xl[[1]]` and `xl[[2]]` are both
equal to the "bare" symbol `.A`, so the information that the second
element of the list is anchored has been lost. Consequently, in e.g.
`expr_match(expr, xl[[2]])`, it will be as though the tilde isn't there,
and `xl[[2]]` will not just match with the top level of `expr` as was
probably intended. Use the `[` operator instead, which retains anchoring
information; `expr_match(expr, xl[2])` will work as expected.

Note that when you replace part of an `expr_list` with another
`expr_list`, the anchoring information from the "replacement"
`expr_list` is copied over, while replacing part of an `expr_list` with
an expression or a "plain" list of expressions retains the existing
anchoring information.

## Examples

``` r
expr_list(
   ~{ 1 + 1 = 2 } ? ~{ 2 + 2 = 4 },
   ~{ y = a * x + b },
   { .A }
)
#> expr_list of length 3: ~{ 1 + 1 = 2 } ? ~{ 2 + 2 = 4 }, ~{ y = a * x + b }, { .A }

# There is support for rlang's injection operators.
var = as.name("myvar")
expr_list({ 1 }, { !!var }, { (!!var)^2 })
#> expr_list of length 3: { 1 }, { myvar }, { myvar^2 }
```
