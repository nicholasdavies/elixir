# Find patterns in expressions

Match and extract patterns in an
[expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
or a list of expressions.

## Usage

``` r
expr_match(expr, pattern, n = Inf,
    dotnames = FALSE, env = parent.frame())

expr_count(expr, pattern, n = Inf, env = parent.frame())
expr_detect(expr, pattern, n = Inf, env = parent.frame())
expr_extract(expr, pattern, what = "match", n = Inf,
    dotnames = FALSE, gather = FALSE, env = parent.frame())
expr_locate(expr, pattern, n = Inf, gather = FALSE,
    env = parent.frame())
```

## Arguments

- expr:

  Input. An
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md),
  [expr_list](https://nicholasdavies.github.io/elixir/reference/expr_list.md),
  or [`list()`](https://rdrr.io/r/base/list.html) of expressions. Also
  works with [formulas](https://rdrr.io/r/base/tilde.html) or lists of
  formulas.

- pattern:

  Pattern to look for. An
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md),
  a length-one
  [expr_list](https://nicholasdavies.github.io/elixir/reference/expr_list.md),
  or a length-one [list](https://rdrr.io/r/base/list.html) of
  expressions. The question mark syntax (see
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md))
  can be used to specify alternatives.

- n:

  Maximum number of matches to make in each expression; default is
  `Inf`.

- dotnames:

  Normally, patterns like `.A`, `..B`, `...C`, etc, are named just `A`,
  `B`, `C`, etc., in the returned matches, without the dot(s) before
  each name. With `dotnames = TRUE`, the dots are kept.

- env:

  Environment for injections in `expr`, `pattern` (see
  [expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)).

- what:

  (`expr_extract` only) Name of the pattern to extract (or `"match"`,
  the default, to extract the entire match).

- gather:

  (`expr_extract` and `expr_locate` only) Whether to only return the
  successful matches, in a single unnested list.

## Value

`expr_match` returns, for each expression in `expr`, either `NULL` if
there is no match, or an object of class `expr_match` if there is a
match. If `expr` is a single expression, just a single `NULL` or
`expr_match` object will be returned, but if `expr` is a list of
expressions, then a list of all results will be returned.

An `expr_match` object is a list containing the elements `alt` (if the
pattern contains several alternatives), `match`, `loc`, and further
elements corresponding to the capture tokens in `pattern` (see below).

For return values of `expr_count`, `expr_detect`, `expr_extract`, and
`expr_locate`, see below.

## Details

All of these functions are used to check whether an
[expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
matches a specific pattern, and if it does, retrieve the details of the
match. These functions are inspired by similar functions in the
`stringr` package.

## Details for expr_match

`expr_match` is the most general of the bunch. As an example, suppose
you had an expression containing the sum of two numbers (e.g.
`3.14159 + 2.71828`) and you wanted to extract the two numbers. You
could use the pattern `{ .A + .B }` to extract the match:

    expr_match({ 3.14159 + 2.71828 }, { .A + .B })

This gives you a list containing all the matches found. In this case,
there is one match, the details of which are contained in an object of
class `expr_match`. This object contains the following elements:

- `match = quote(3.14159 + 2.71828)`, the entire match;

- `loc = NULL`, the location of the match within the expression;

- `A = 3.14159`, the part of the match corresponding to the *capture
  token* `.A`;

- `B = 2.71828`, the part of the match corresponding to the *capture
  token* `.B`.

We can also use a list of expressions for `expr`, as in:

    ex <- expr_list({ x + y }, { kappa + lambda }, { p * z })
    expr_match(ex, { .A + .B })

This returns a list with one entry for each element of the list `ex`;
for the expressions that match (`ex[[1]]` and `ex[[2]]`) an `expr_match`
object is returned, while for the expression that does not match
(`ex[[3]]`), `NULL` is returned.

## Pattern syntax

The `pattern` expression (e.g. `{.A + .B}` in the above) follows a
special syntax.

### Capture tokens

First, these patterns can contain *capture tokens*, which are names
starting with one to three periods and match to the following:

- `.A` matches any single token

- `..A` matches any sub-expression

- `...A` matches any number of function arguments

Above, "A" can be any name consisting of an alphabetical character
(`a-z`, `A-Z`) followed by any number of alphanumeric characters (`a-z`,
`A-Z`, `0-9`), underscores (`_`), or dots (`.`). This is the name given
to the match in the returned list. Alternatively, it can be any name
starting with an underscore (e.g. so the entire token could be `._` or
`..._1`), in which case the match is made but the capture is discarded.

Additionally, the single-token pattern (e.g. `.A`) can be extended as
follows:

- Use `` `.A:classname` `` to require that the class of the object be
  "classname" (or contain "classname" if the object has multiple
  classes); so e.g. `` `.A:name` `` matches a single name (i.e. symbol).

- Use `` `.A/regexp` `` to require a regular expression match `regexp`;
  so e.g. `` `.A:name/ee` `` will match symbols with two consecutive
  lowercase letter 'e's;

- Use `` `.A|test` `` to require that the expression `test` evaluates to
  `TRUE`, where `.` can be used as a stand-in for the matched token; so
  e.g. `` `.A:numeric|.>5` `` will match numbers greater than 5.

The `regexp` and `test` specifiers cannot be used together, and have to
come after the `classname` specifier if one appears. These special
syntaxes require the whole symbol to be wrapped in backticks, as in the
examples above, so that they parse as symbols.

### Matching function arguments

If you wish to match a single, unnamed function argument, you can use a
capture token of the form `.A` (single-token argument) or `..B`
(expression argument). To match all arguments, including named ones, use
a capture token of the form `...C`. For example, these all match:

    expr_match({ myfunc() }, { .F() })
    expr_match({ myfunc(1) }, { .F(.X) })
    expr_match({ myfunc(1 + 1) }, { myfunc(..X) })
    expr_match({ myfunc(1, 2) }, { .F(.X, .Y) })
    expr_match({ myfunc() }, { myfunc(...A) })
    expr_match({ myfunc(1) }, { .F(...A) })
    expr_match({ myfunc(2, c = 3) }, { myfunc(...A) })

but these do not:

    expr_match({ myfunc() }, { .F(.X) })
    expr_match({ myfunc() }, { .F(..X) })
    expr_match({ myfunc(a = 1) }, { .F(.X) })
    expr_match({ myfunc(a = 1 + 1) }, { .F(..X) })
    expr_match({ myfunc(1,2) }, { .F(..X) })
    expr_match({ myfunc(a = 1, b = 2) }, { .F(...X, ...Y) })

There may be support for named arguments in patterns in the future, e.g.
a pattern such as `{ f(a = .X) }` that would match an expression like
`{ f(a = 1) }`, but that is currently not supported. So currently you
can only match named function arguments using the `...X` syntax.

### Anchoring versus recursing into expressions

If you want your anchor your pattern, i.e. ensure that the pattern will
only match at the "outer level" of your expression(s), without matching
to any sub-expressions within, use a tilde (`~`) outside the braces (see
[expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
for details). For example, `expr_match({1 + 2 + 3 + 4}, ~{..A + .B})`
only gives one match, to the addition at the outermost level of
`1 + 2 + 3` plus `4`, but `expr_match({1 + 2 + 3 + 4}, {..A + .B})` also
matches to the inner additions of `1 + 2` plus `3` and `1` plus `2`.

### Alternatives

Finally, `pattern` can be a series of alternatives, using the operator
`?` for specifying alternatives (see
[expression](https://nicholasdavies.github.io/elixir/reference/elixir-expression.md)
for details). Results from the first matching pattern among these
alternatives will be returned, and the returned `expr_match` object will
include a special element named "alt" giving the index of the matching
alternative (see examples).

## Details for `expr_count`, `expr_detect`, `expr_extract`, and `expr_locate`

These shortcut functions return only some of the information given by
`expr_match`, but often in a more convenient format.

`expr_count` returns an integer vector with one element for every
expression in `expr`, each element giving the number of matches of
`pattern` found.

`expr_detect` returns a logical vector with one element for every
expression in `expr`, each element giving whether at least one match of
`pattern` was found.

`expr_extract` returns, for each expression in `expr`, a list of all the
complete matches. Or, by specifing a capture token name in the argument
`which`, those can be extracted instead. For example:

    expr_extract(expr_list({(a+b)+(x+y)},
        {"H"*"I"}, {3+4}), {.A + .B}, "A")

gives `list(list(quote(a), quote(x)), NULL, list(3))`.

Using `gather = TRUE` with `expr_extract` returns only the succesful
matches in a single, unnested list; so the above call to `expr_extract`
with `gather = TRUE` would give `list(quote(a), quote(x), 3)`.

Finally, `expr_locate` is similar to `expr_extract` but it returns the
location within `expr` of each successful match.

## See also

[`expr_replace()`](https://nicholasdavies.github.io/elixir/reference/expr_replace.md)
to replace patterns in expressions.

## Examples

``` r
expr_match({ 1 + 2 }, { .A + .B })
#> expr_match: list(
#>   list(match = quote(1 + 2), loc = NULL, A = 1, B = 2)
#> )

# match to one of several alternatives
expr_match({ 5 - 1 }, { .A + .B } ? { .A - .B })
#> expr_match: list(
#>   list(alt = 2L, match = quote(5 - 1), loc = NULL, A = 5, B = 1)
#> )
```
