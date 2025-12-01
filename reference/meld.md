# Code generation from template file

`meld` reads a specially-formatted file from filename `file` or as lines
of text passed via unnamed arguments and returns these lines of text
after performing substitutions of R code.

This function is experimental.

## Usage

``` r
meld(
  ...,
  file = NULL,
  rules = NULL,
  reindent = TRUE,
  ipath = ".",
  env = rlang::env_clone(parent.frame())
)
```

## Arguments

- ...:

  Lines to be interpreted as the text. If there are any embedded
  newlines in a line, the line is split into multiple lines.

- file:

  File to be read in as the text.

- rules:

  Which
  [rules](https://nicholasdavies.github.io/elixir/reference/elixir-rules.md)
  to follow. You can pass a string from among `"C"`, `"C++"`, `"Lua"`,
  or `"R"`, or a list with elements:

  - `comment` Character vector for comments (used when backticked lines
    are skipped); either NA for no comments, one string for end-of-line
    comments or two strings for delimited comments.

  - `indent_more` Character vector of tokens which increase the indent
    level.

  - `indent_less` Character vector of tokens which decrease the indent
    level.

  - `indent_both` Character vector of tokens which decrease, then
    increase the indent level (see
    [`reindent()`](https://nicholasdavies.github.io/elixir/reference/reindent.md)).

  - `ignore` Comment and string literal delimiters (see
    [`reindent()`](https://nicholasdavies.github.io/elixir/reference/reindent.md)).

  If `NULL`, the default, either guess rules from the file extension, or
  if that is not possible, do not put in 'skipped' comments and do not
  reindent the result. `NA` to not try to guess.

- reindent:

  If `TRUE`, the default, reindent according to `rules`. If `FALSE`, do
  not reindent.

- ipath:

  Path to search for `#include`d files

- env:

  Environment in which to evaluate R expressions. The default is
  `rlang::env_clone(parent.frame())`, and it is best to clone the
  environment so that new declarations do not pollute the environment in
  question.

## Value

The interpreted text as a single character string.

## Details

As `meld` works through each line of the text, any blocks of text
starting with the delimiter `/***R` and ending with `*/` are run as R
code.

Outside these blocks, any substrings in the text delimited by
`` `backticks` `` are interpreted as R expressions to be substituted
into the line. If any of the backticked expressions are length 0, the
line is commented out (with the message "\[skipped\]" appended) using
the `comment` element of `rules`. If any of the backticked expressions
are length L \> 1, the entire interpreted line is repeated L times,
separated by newlines and with elements of the expression in sequence.

There are some special sequences:

- `` `^expr` `` subs in `expr` only on the first line of a multi-line
  expansion

- `` `!^expr` `` subs in `expr` on all but the first line of a
  multi-line expansion

- `` `$expr` ``subs in `expr` only on the last line of a multi-line
  expansion

- `` `!$expr` `` subs in `expr` on all but the last line of a multi-line
  expansion

- `` `#include file` `` interprets `file` as an R expression resolving
  to a filename, runs that file through `meld`, and pastes in the result

The `#include` command must appear by itself on a line, and searches for
files in the path `ipath`.

The function tries to guess `rules` from the file extension if that is
possible. If the file extension is .c, then `"C"` is guessed; for .h,
.hpp, or .cpp, `"C++"` is guessed; for .R, `"R"` is guessed; for .lua,
`"Lua"` is guessed. Case is ignored for file extensions.

R blocks are evaluated immediately prior to the next-occurring
backticked line, so variables modified in an R block are available to
any backticked expression following the R block. Any remaining R blocks
are run after remaining lines are interpreted.

If any line from the text ends with a single backslash `\`, the next
line is concatenated to it. If any line from the text ends with a double
backslash `\\`, the next line is concatenated to it with a newline as a
separator. This allows backticked expressions to apply over multiple
lines.

## Examples

``` r
meld(
    "/***R",
    "names = c('a', 'b', 'c');",
    "dontdothis = NULL;",
    "*/",
    "double foo()",
    "{",
    "    double `names` = `1:3`;",
    "    double `dontdothis` = this_doesnt_matter;",
    "    return `paste(names, collapse = ' + ')`;",
    "}")
#> [1] "double foo()\n{\n    double a = 1;\n    double b = 2;\n    double c = 3;\n    return a + b + c;\n}"
```
