# Expressions in `elixir`

`elixir` is primarily a package for working with what it calls
"expressions", in the sense of any R object for which
[`rlang::is_expression()`](https://rlang.r-lib.org/reference/is_expression.html)
returns `TRUE`. This includes calls, like the results of evaluating
`quote(f(x))` or `quote(a:b)`, symbols like `quote(z)`, and syntactic
literals like `2.5`, `"hello"`, `NULL`, `FALSE`, and so on. In many
cases, you can also use `elixir` to work with
[formulas](https://rdrr.io/r/base/tilde.html), even though
[`rlang::is_expression()`](https://rlang.r-lib.org/reference/is_expression.html)
returns `FALSE` for formulas.

This is not to be confused with the built-in type
[base::expression](https://rdrr.io/r/base/expression.html), which is
essentially a special way of storing a vector of multiple "expressions".
`elixir` does not use this type; see
[`expr_list()`](https://nicholasdavies.github.io/elixir/reference/expr_list.md)
instead.

## Usage

    expr_list(number = { `.A:numeric` } ? { `.A:integer` },
        string = { `.A:character` }, symbol = { `.A:name` })
    expr_match({ 1 * 2 }, ~{ .A * .B })
    expr_match({ 1 * 2 }, { `.A:numeric` })
    expr_replace({ y = a*x^3 + b*x^2 + c*x^1 + d*x^0 },
        { ..X ^ ..N }, { pow(..X, ..N) })

## Specifying expressions in `elixir`

The `elixir` package functions starting with `expr_` work with
expressions. These functions all accept a special (optional) syntax for
specifying expressions which involves the symbols
[`{}`](https://rdrr.io/r/base/Paren.html), `?`, and `~`, as well as the
rlang [injection operator,
!!](https://rlang.r-lib.org/reference/injection-operator.html) and
[splice operator,
!!!](https://rlang.r-lib.org/reference/splice-operator.html)).

With base R, if you want to store an expression such as `x + y` in a
variable or pass it to a function, you need to use
[`base::quote()`](https://rdrr.io/r/base/substitute.html) or
[`rlang::expr()`](https://rlang.r-lib.org/reference/expr.html), but any
Elixir `expr_` function will also accept an "expression literal" wrapped
in braces, [`{}`](https://rdrr.io/r/base/Paren.html).

So, for example, rather than

`translate(quote(x ^ y), "C++")`

you can write

`translate({ x ^ y }, "C++")`.

This only works if the braces are provided "directly"; that is, in

`expr <- quote({ x ^ y }); translate(expr, "C++")`,

the braces are not interpreted in any special way.

Anything between the braces essentially gets put through
[`rlang::expr()`](https://rlang.r-lib.org/reference/expr.html), so you
can use `!!` (i.e.
[rlang::injection-operator](https://rlang.r-lib.org/reference/injection-operator.html))
and `!!!` (i.e.
[rlang::splice-operator](https://rlang.r-lib.org/reference/splice-operator.html)).
There is an `env` parameter to all relevant `elixir` functions,
defaulting to
[`parent.frame()`](https://rdrr.io/r/base/sys.parent.html), in which
these injection operations are evaluated.

## Special syntax for patterns and replacements

Additionally, some functions
([`expr_match()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
[`expr_count()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
[`expr_detect()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
[`expr_extract()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
[`expr_locate()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
and
[`expr_replace()`](https://nicholasdavies.github.io/elixir/reference/expr_replace.md))
take `pattern` and/or `replacement` arguments to specify patterns to
match to an expression and/or replacement expressions to replace those
matches with.

For both `pattern` and `replacement` arguments, you can use the question
mark operator `?` to specify *alternatives*. For example, to match
*either* the token `cat` or `dog`, you can use

`expr_match(expr, { cat } ? { dog })`.

You can chain together as many alternatives as are needed.
Alternatively, if you have a list of expressions `z`, you can use a
single question mark before the name of the list, like so:

`expr_match(expr, ?z)`

and `elixir` will treat the list as a set of alternatives. When using
[`expr_replace()`](https://nicholasdavies.github.io/elixir/reference/expr_replace.md)
with a set of alternatives as the pattern, the replacement needs to be
either a single expression, or a set of alternative expressions which
has the same number of alternatives as in the pattern.

You can also use the tilde operator `~` to specify that a given pattern
should be "anchored" at the top level of an expression, and will not
"recurse into" the expression. For example, in

    exprs = expr_list(2, 5, {1 + 4})
    expr_match(exprs, ~{ `.A:numeric` })

only the numbers `2` and `5` will match. However, in

    exprs = expr_list(2, 5, {1 + 4})
    expr_match(exprs, { `.A:numeric` })

all numbers `2`, `5`, `1` and `4` will match, because the `pattern` can
recurse into the third expression `1 + 4`.
