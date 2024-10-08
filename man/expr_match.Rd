% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match.R, R/convenience.R
\name{expr_match}
\alias{expr_match}
\alias{expr_count}
\alias{expr_detect}
\alias{expr_extract}
\alias{expr_locate}
\title{Find patterns in expressions}
\usage{
expr_match(expr, pattern, n = Inf,
    dotnames = FALSE, env = parent.frame())

expr_count(expr, pattern, n = Inf, env = parent.frame())
expr_detect(expr, pattern, n = Inf, env = parent.frame())
expr_extract(expr, pattern, what = "match", n = Inf,
    dotnames = FALSE, gather = FALSE, env = parent.frame())
expr_locate(expr, pattern, n = Inf, gather = FALSE,
    env = parent.frame())
}
\arguments{
\item{expr}{Input. An \link[=elixir-expression]{expression}, \link{expr_list}, or
\code{\link[=list]{list()}} of expressions.}

\item{pattern}{Pattern to look for. An \link[=elixir-expression]{expression}, a
length-one \link{expr_list}, or a length-one \link{list} of expressions. The
question mark syntax (see \link[=elixir-expression]{expression}) can be used
to specify alternatives.}

\item{n}{Maximum number of matches to make in each expression; default is
\code{Inf}.}

\item{dotnames}{Normally, patterns like \code{.A}, \code{..B}, \code{...C}, etc, are named
just \code{A}, \code{B}, \code{C}, etc., in the returned matches, without the dot(s)
before each name. With \code{dotnames = TRUE}, the dots are kept.}

\item{env}{Environment for injections in \code{expr}, \code{pattern} (see
\link[=elixir-expression]{expression}).}

\item{what}{(\code{expr_extract} only) Name of the pattern to extract (or
\code{"match"}, the default, to extract the entire match).}

\item{gather}{(\code{expr_extract} and \code{expr_locate} only) Whether to only return
the successful matches, in a single unnested list.}
}
\value{
\code{expr_match} returns, for each expression in \code{expr}, either \code{NULL}
if there is no match, or an object of class \code{expr_match} if there is a
match. If \code{expr} is a single expression, just a single \code{NULL} or
\code{expr_match} object will be returned, but if \code{expr} is a list of
expressions, then a list of all results will be returned.

An \code{expr_match} object is a list containing the elements \code{alt} (if the
pattern contains several alternatives), \code{match}, \code{loc}, and
further elements corresponding to the capture tokens in \code{pattern} (see
below).

For return values of \code{expr_count}, \code{expr_detect}, \code{expr_extract}, and
\code{expr_locate}, see below.
}
\description{
Match and extract patterns in an \link[=elixir-expression]{expression} or a list of expressions.
}
\section{Details}{
All of these functions are used to check whether an
\link[=elixir-expression]{expression} matches a specific pattern, and if it does,
retrieve the details of the match. These functions are inspired by similar
functions in the \code{stringr} package.
}

\section{Details for expr_match}{
\code{expr_match} is the most general of the bunch. As an example, suppose you
had an expression containing the sum of two numbers (e.g.
\code{3.14159 + 2.71828}) and you wanted to extract the two numbers. You
could use the pattern \code{{ .A + .B }} to extract the match:

\if{html}{\out{<div class="sourceCode">}}\preformatted{expr_match(\{ 3.14159 + 2.71828 \}, \{ .A + .B \})
}\if{html}{\out{</div>}}

This gives you a list containing all the matches found. In this case,
there is one match, the details of which are contained in an object of
class \code{expr_match}. This object contains the following elements:
\itemize{
\item \code{match = quote(3.14159 + 2.71828)}, the entire match;
\item \code{loc = NULL}, the location of the match within the expression ;
\item \code{A = 3.14159}, the part of the match corresponding to the \emph{capture token}
\code{.A};
\item \code{B = 2.71828}, the part of the match corresponding to the \emph{capture token}
\code{.B}.
}

We can also use a list of expressions for \code{expr}, as in:

\if{html}{\out{<div class="sourceCode">}}\preformatted{ex <- expr_list(\{ x + y \}, \{ kappa + lambda \}, \{ p * z \})
expr_match(ex, \{ .A + .B \})
}\if{html}{\out{</div>}}

This returns a list with one entry for each element of the list \code{ex}; for
the expressions that match (\code{ex[[1]]} and \code{ex[[2]]}) an \code{expr_match} object
is returned, while for the expression that does not match (\code{ex[[3]]}),
\code{NULL} is returned.
}

\section{Pattern syntax}{
The \code{pattern} expression (e.g. \code{{.A + .B}} in the above) follows a special
syntax.
\subsection{Capture tokens}{

First, these patterns can contain \emph{capture tokens}, which are names starting
with one to three periods and match to the following:
\itemize{
\item \code{.A} matches any single token
\item \code{..A} matches any sub-expression
\item \code{...A} matches any number of function arguments
}

Above, "A" can be any name consisting of an alphabetical character (\code{a-z},
\code{A-Z}) followed by any number of alphanumeric characters (\code{a-z}, \code{A-Z},
\code{0-9}), underscores (\verb{_}), or dots (\code{.}). This is the name given to the
match in the returned list. Alternatively, it can be any name starting with
an underscore (e.g. so the entire token could be \code{._} or \code{..._1}), in which
case the match is made but the capture is discarded.

Additionally, the single-token pattern (e.g. \code{.A}) can be extended as
follows:
\itemize{
\item Use \code{`.A:classname`} to require that the class of the object be
"classname" (or contain "classname" if the object has multiple classes); so
e.g. \code{`.A:name`} matches a single name (i.e. symbol).
\item Use \code{`.A/regexp`} to require a regular expression match \code{regexp}; so
e.g. \code{`.A:name/ee`} will match symbols with two consecutive lowercase
letter 'e's;
\item Use \code{`.A|test`} to require that the expression \code{test} evaluates to
\code{TRUE}, where \code{.} can be used as a stand-in for the matched token; so e.g.
\code{`.A:numeric|.>5`} will match numbers greater than 5.
}

The \code{regexp} and \code{test} specifiers cannot be used together, and have to come
after the \code{classname} specifier if one appears. These special syntaxes
require the whole symbol to be wrapped in backticks, as in the examples
above, so that they parse as symbols.
}

\subsection{Matching function arguments}{

If you wish to match a single, unnamed function argument, you can use a
capture token of the form \code{.A} (single-token argument) or \code{..B} (expression
argument). To match all arguments, including named ones, use a capture token
of the form \code{...C}. For example, these all match:

\if{html}{\out{<div class="sourceCode">}}\preformatted{expr_match(\{ myfunc() \}, \{ .F() \})
expr_match(\{ myfunc(1) \}, \{ .F(.X) \})
expr_match(\{ myfunc(1 + 1) \}, \{ myfunc(..X) \})
expr_match(\{ myfunc(1, 2) \}, \{ .F(.X, .Y) \})
expr_match(\{ myfunc() \}, \{ myfunc(...A) \})
expr_match(\{ myfunc(1) \}, \{ .F(...A) \})
expr_match(\{ myfunc(2, c = 3) \}, \{ myfunc(...A) \})
}\if{html}{\out{</div>}}

but these do not:

\if{html}{\out{<div class="sourceCode">}}\preformatted{expr_match(\{ myfunc() \}, \{ .F(.X) \})
expr_match(\{ myfunc() \}, \{ .F(..X) \})
expr_match(\{ myfunc(a = 1) \}, \{ .F(.X) \})
expr_match(\{ myfunc(a = 1 + 1) \}, \{ .F(..X) \})
expr_match(\{ myfunc(1,2) \}, \{ .F(..X) \})
expr_match(\{ myfunc(a = 1, b = 2) \}, \{ .F(...X, ...Y) \})
}\if{html}{\out{</div>}}

There may be support for named arguments in patterns in the future, e.g. a
pattern such as \code{{ f(a = .X) }} that would match an expression like
\code{{ f(a = 1) }}, but that is currently not supported. So currently you can
only match named function arguments using the \code{...X} syntax.
}

\subsection{Anchoring versus recursing into expressions}{

If you want your anchor your pattern, i.e. ensure that the pattern will only
match at the "outer level" of your expression(s), without matching to any
sub-expressions within, use a tilde (\code{~}) outside the braces (see
\link[=elixir-expression]{expression} for details). For example,
\code{expr_match({1 + 2 + 3 + 4}, ~{..A + .B})} only gives one match, to the
addition at the outermost level of \code{1 + 2 + 3} plus \code{4}, but
\code{expr_match({1 + 2 + 3 + 4}, {..A + .B})} also matches to the inner
additions of \code{1 + 2} plus \code{3} and \code{1} plus \code{2}.
}

\subsection{Alternatives}{

Finally, \code{pattern} can be a series of alternatives, using the operator \verb{?}
for specifying alternatives (see \link[=elixir-expression]{expression} for
details). Results from the first matching pattern among these alternatives
will be returned, and the returned \code{expr_match} object will include a
special element named "alt" giving the index of the matching alternative
(see examples).
}
}

\section{Details for \code{expr_count}, \code{expr_detect}, \code{expr_extract}, and \code{expr_locate}}{
These shortcut functions return only some of the information given by
\code{expr_match}, but often in a more convenient format.

\code{expr_count} returns an integer vector with one element for every expression
in \code{expr}, each element giving the number of matches of \code{pattern} found.

\code{expr_detect} returns a logical vector with one element for every expression
in \code{expr}, each element giving whether at least one match of \code{pattern} was
found.

\code{expr_extract} returns, for each expression in \code{expr}, a list of all the
complete matches. Or, by specifing a capture token name in the argument
\code{which}, those can be extracted instead. For example:

\if{html}{\out{<div class="sourceCode">}}\preformatted{expr_extract(expr_list(\{(a+b)+(x+y)\},
    \{"H"*"I"\}, \{3+4\}), \{.A + .B\}, "A")
}\if{html}{\out{</div>}}

gives \code{list(list(quote(a), quote(x)), NULL, list(3))}.

Using \code{gather = TRUE} with \code{expr_extract} returns only the succesful matches
in a single, unnested list; so the above call to \code{expr_extract} with
\code{gather = TRUE} would give \code{list(quote(a), quote(x), 3)}.

Finally, \code{expr_locate} is similar to \code{expr_extract} but it returns the
location within \code{expr} of each successful match.
}

\examples{
expr_match({ 1 + 2 }, { .A + .B })

# match to one of several alternatives
expr_match({ 5 - 1 }, { .A + .B } ? { .A - .B })
}
\seealso{
\code{\link[=expr_replace]{expr_replace()}} to replace patterns in expressions.
}
