# Rules for understanding languages

Several `elixir` functions – namely
[`meld()`](https://nicholasdavies.github.io/elixir/reference/meld.md),
[`reindent()`](https://nicholasdavies.github.io/elixir/reference/reindent.md),
and
[`translate()`](https://nicholasdavies.github.io/elixir/reference/translate.md)
– take an argument `rules` which assists those functions in interpreting
their arguments.

## Details

In all cases, `rules` can either be a character string identifying a set
of built-in rules for a specific language or purpose – currently,
`elixir` accepts `"C"`, `"C++"`, `"Lua"`, or `"R"` – or a list with
elements required for interpretation.

`elixir:::ruleset` contains the built-in rules. Passing an empty
[`list()`](https://rdrr.io/r/base/list.html) as the `rules` argument to
an `elixir` function will cause it to complain about the missing
components, which is one way of discerning what is needed for a given
function, but usually these error messages do not quite cover all
details of what is needed.
