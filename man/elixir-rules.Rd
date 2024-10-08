% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rules.R
\name{elixir-rules}
\alias{elixir-rules}
\title{Rules for understanding languages}
\description{
Several Elixir functions -- namely \code{\link[=meld]{meld()}}, \code{\link[=reindent]{reindent()}}, and \code{\link[=translate]{translate()}}
-- take an argument \code{rules} which assists those functions in interpreting
their arguments.
}
\details{
In all cases, \code{rules} can either be a character string identifying a set of
built-in rules for a specific language or purpose -- currently, Elixir
accepts \code{"C"}, \code{"C++"}, \code{"Lua"}, or \code{"R"} -- or a list with elements
required for interpretation.

\code{elixir:::ruleset} contains the built-in rules. Passing an empty \code{list()} as
the \code{rules} argument to an Elixir function will cause it to complain about
the missing components, which is one way of discerning what is needed for
a given function, but usually these error messages do not quite cover all
details of what is needed.
}
