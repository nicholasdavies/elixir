% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meld.R
\name{meld}
\alias{meld}
\title{Code generation from template file}
\usage{
meld(
  ...,
  file = NULL,
  rules = NULL,
  reindent = TRUE,
  ipath = ".",
  env = rlang::env_clone(parent.frame())
)
}
\arguments{
\item{...}{Lines to be interpreted as the text.}

\item{file}{File to be read in as the text.}

\item{rules}{Which \link[=elixir-rules]{rules} to follow. You can pass a string
from among \code{"C"}, \code{"C++"}, \code{"Lua"}, or \code{"R"}, or a list with elements:
\itemize{
\item \code{comment} Character vector for comments (used when backticked lines are
skipped); either NA for no comments, one string for end-of-line comments or
two strings for delimited comments.
\item \code{indent_more} Character vector of tokens which increase the indent level.
\item \code{indent_less} Character vector of tokens which decrease the indent level.
\item \code{indent_both} Character vector of tokens which decrease, then increase the
indent level (see \code{\link[=reindent]{reindent()}}).
\item \code{ignore} Comment and string literal delimiters (see \code{\link[=reindent]{reindent()}}).
If \code{NULL}, the default, either guess rules from the file extension, or if
that is not possible, do not put in 'skipped' comments and do not reindent
the result. \code{NA} to not try to guess.
}}

\item{reindent}{If \code{TRUE}, the default, reindent according to \code{rules}. If
\code{FALSE}, do not reindent.}

\item{ipath}{Path to search for \verb{#include}d files}

\item{env}{Environment in which to evaluate R expressions. The default is
\code{rlang::env_clone(parent.frame())}, and it is best to clone the environment
so that new declarations do not pollute the environment in question.}
}
\value{
The interpreted text as a single character string.
}
\description{
\code{meld} reads a specially-formatted file from filename \code{file} or
as lines of text passed via unnamed arguments and returns these lines of
text after performing substitutions of R code.

This function is experimental.
}
\details{
First, any blocks of text starting with the delimiter \verb{/***R} and
ending with \verb{*/} are run as R code.

Second, any substrings in the text delimited by \code{`backticks`}
are interpreted as R expressions to be substituted into the line. If
any of the backticked expressions are length 0, the line is commented
out (with the message "[skipped]" appended) using the \code{comment} element
of \code{rules}. If any of the backticked expressions are length L > 1, the
entire interpreted line is repeated L times, separated by newlines and
with elements of the expression in sequence.

There are some special sequences:
\itemize{
\item \code{`^expr`} subs in \code{expr} only on the first line of a multi-line expansion
\item \code{`!^expr`} subs in \code{expr} on all but the first line of a multi-line expansion
\item \code{`$expr`}subs in \code{expr} only on the last line of a multi-line expansion
\item \code{`!$expr`} subs in \code{expr} on all but the last line of a multi-line expansion
\item \code{`#include <file>`} runs \code{file} through \code{meld} and pastes in the result
}

The \verb{#include} command must appear by itself on a line, and searches for
\code{file} in the path \code{ipath}.

The function tries to guess \code{rules} from the file extension if that is
possible. If the file extension is .c, then \code{"C"} is guessed; for .h, .hpp,
or .cpp, \code{"C++"} is guessed; for .R, \code{"R"} is guessed; for .lua, \code{"Lua"} is
guessed. Case is ignored for file extensions.

R blocks are evaluated immediately prior to the next-occurring backticked
line, so variables modified in an R block are available to any backticked
expression following the R block. Any remaining R blocks are run after
remaining lines are interpreted.

If any line from the text ends with a single backslash \code{\\}, the next
line is concatenated to it. If any line from the text ends with a double
backslash \code{\\\\}, the next line is concatenated to it with a newline as
a separator. This allows backticked expressions to apply over multiple
lines.
}
\examples{
meld(
    "/***R",
    "names = c('a', 'b', 'c');",
    "dontdothis = NULL;",
    "*/",
    "double foo()",
    "{",
    "    double `names` = `1:3`;",
    "    double `dontdothis` = this_doesnt_matter;",
    "    return a + b + c;",
    "}")
}
