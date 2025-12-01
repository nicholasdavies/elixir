# `elixir`: Transmutation of languages

`elixir` is a set of tools for transforming R expressions, including
into other programming languages.

## Details

One of the neat features of R is that you can use the language to
inspect itself. Expressions, functions, indeed entire R scripts can be
examined and manipulated just like any list, data.frame, or other R
object.

However, the syntax for manipulating R language objects is a little
tricky. Packages such as `rlang` help to make this task easier. `elixir`
makes a few extra shortcuts available, and is geared for advanced R
users.

`elixir` provides functions for finding, extracting, and replacing
patterns in 'R' language objects, similarly to how regular expressions
can be used to find, extract, and replace patterns in text. It also
provides functions for generating code using specially-formatted
template files and for translating 'R' expressions into similar
expressions in other programming languages.

The package may be helpful for advanced uses of 'R' expressions, such as
developing domain-specific languages.

## Find and replace for language objects

Sometimes you want to detect certain patterns within an expression or
list of expressions, or easily replace a certain pattern with another.
When working with strings, regular expressions are a handy way of
accomplishing such tasks. `elixir` provides a sort of "regular
expressions for R expressions" functionality through the functions
[`expr_match()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
[`expr_replace()`](https://nicholasdavies.github.io/elixir/reference/expr_replace.md),
and the "shortcut" functions
[`expr_count()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
[`expr_detect()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
[`expr_extract()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md),
and
[`expr_locate()`](https://nicholasdavies.github.io/elixir/reference/expr_match.md).

## Other `elixir` features

The function
[`expr_apply()`](https://nicholasdavies.github.io/elixir/reference/expr_apply.md)
allows you to transform and extract information from nested list
structures which contain expressions, so if you have a big structure and
you want to check all the variable names or make certain replacements,
this may be useful.

[`expr_sub()`](https://nicholasdavies.github.io/elixir/reference/expr_sub.md)
offers an interface for extracting or replacing part of an expression;
the one advantage this has over `[[` is that it allows you to use `NULL`
as the index, which gives back the whole expression.

[`lang2str()`](https://nicholasdavies.github.io/elixir/reference/lang2str.md)
does the opposite of
[`base::str2lang()`](https://rdrr.io/r/base/parse.html); it is like
[`deparse1()`](https://rdrr.io/r/base/deparse.html) which is new since R
4.0.0, but with `collapse = ""` instead of `collapse = " "`.

Finally,
[`meld()`](https://nicholasdavies.github.io/elixir/reference/meld.md),
[`translate()`](https://nicholasdavies.github.io/elixir/reference/translate.md),
and
[`reindent()`](https://nicholasdavies.github.io/elixir/reference/reindent.md)
are various experimental functions for constructing code using R.
