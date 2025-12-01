# Convert an expression into a string

The opposite of [`str2lang()`](https://rdrr.io/r/base/parse.html),
`lang2str()` converts an
[expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
into a character string. Note that `lang2str()` does not support the
normal expression syntax for `elixir`, so just expects an already-parsed
expression.

## Usage

``` r
lang2str(x)
```

## Arguments

- x:

  Expression to convert to a string.

## Value

A character string suitable for printing.

## Details

This function is essentially identical to
[`deparse1()`](https://rdrr.io/r/base/deparse.html), which has been
available since R 4.0.0, except with `collapse = ""` instead of
`collapse = " "`.

## Examples

``` r
lang2str(quote(a + b + c))
#> [1] "a + b + c"
```
