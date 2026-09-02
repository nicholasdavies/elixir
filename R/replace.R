#' Replace patterns within expressions
#'
#' Match and replace elements of patterns in an [expression][elixir-expression]
#' or a list of expressions.
#'
#' Patterns follow the syntax for [expr_match()].
#'
#' @usage
#' expr_replace(expr, ..., patterns, replacements,
#'     n = Inf, env = parent.frame())
#' @param expr Input. An [expression][elixir-expression], [expr_list], or
#'     [list()] of expressions.
#' @param ... Alternating series of patterns and replacements. Each pattern
#'     should be a single [expression][elixir-expression] (though alternatives
#'     can be specified with `?`). Each replacement should be either a single
#'     expression or a function (see *Function replacements*).
#' @param patterns Patterns to look for. An [expression][elixir-expression],
#'     [expr_list], or [list()] of expressions.
#' @param replacements Replacements, one for each pattern. Each can be an
#'     [expression][elixir-expression] or a function (see *Function
#'     replacements*).
#' @param n Maximum number of times for each expression to make each
#'     replacement; default is `Inf`.
#' @param env Environment for injections in `expr`, `pattern` (see
#'     [expression][elixir-expression]).
#' @return The input expression(s) with any replacements made.
#'
#' @details
#' # Function replacements
#'
#' Normally, each replacement is an expression template with capture names (like
#' `.A` or `..B`) standing in for the matched parts, e.g. `{ .A + ..B }`. But
#' if a replacement is a function, it is called for each match and the return
#' value is used as the replacement, e.g.
#' `function(m) substitute(A - B, m)`.
#'
#' The function takes a single argument: a named list containing the match
#' details. This list has elements `match` (the matched expression), `loc` (its
#' location), and all captures named without leading dots. For example, with the
#' pattern `{ ..A ^ .N }` matching `x^2`, the function receives:
#'
#' ```
#' list(match = quote(x^2), loc = ..., A = quote(x), N = 2)
#' ```
#'
#' To build a new expression from captures, you can pass the list as the second
#' argument to [base::substitute()]. A full example:
#'
#' ```
#' expr_replace({ x^2 + y^2 + z^3 }, { ..A ^ .N },
#'     function(m) {
#'         if (m$N == 2) substitute(square(A), m) else substitute(A ^ N, m)
#'     })
#' # => square(x) + square(y) + z^3
#' ```
#'
#' @seealso [expr_match()] to find patterns in expressions, and its cousins
#' [expr_count()], [expr_detect()], [expr_extract()], and [expr_locate()].
#' @examples
#' # Example with alternating patterns and replacements
#' expr_replace({ 1 + 2 }, {1}, {one}, {2}, {two})
#'
#' # Example with patterns and replacements in a list
#' expr_replace({ 1 + 2 }, patterns = expr_list({1}, {2}),
#'     replacements = expr_list({one}, {two}))
#'
#' # Replace with captures
#' expr_replace({ 1 + 2 }, ~{ .A + .B }, { .A - .B })
#'
#' # Function replacement with conditional logic
#' expr_replace({ x^2 + y^2 + z^3 }, { ..A ^ .N },
#'     function(m) {
#'         if (m$N == 2) substitute(square(A), m) else substitute(A ^ N, m)
#'     })
#' @export
expr_replace = function(expr, ..., patterns, replacements, n = Inf, env = parent.frame())
{
    # Parse expr
    expr = do.call(do_parse_simple, list(substitute(expr), env));

    # Assemble patterns and replacements
    if (...length() == 0 && !missing(patterns) && !missing(replacements)) {
        # Get patterns & replacements directly supplied
        patterns     = do.call(expr_list, list(substitute(patterns), env = env));
        replacements = do.call(expr_list, list(substitute(replacements), env = env));
        if (length(patterns) != length(replacements)) {
            stop("Both patterns and replacements must be of equal length.")
        }
    } else if (...length() > 0 && ...length() %% 2 == 0 && missing(patterns) && missing(replacements)) {
        # Get patterns & replacements from pairs of expressions in ...
        a.args = substitute(a(...));
        patterns     = do.call(expr_list, c(as.list(a.args[seq(2, ...length() + 1, 2)]), env = env));
        replacements = do.call(expr_list, c(as.list(a.args[seq(3, ...length() + 1, 2)]), env = env));
    } else {
        stop("Must supply pairs of expressions in ..., or equal-length lists in patterns and replacements.")
    }

    # Do replacement
    result = lapply(expr, function(x) {
        for (j in seq_along(patterns)) {
            matches = expr_match(x, patterns[j], n = n, dotnames = TRUE)
            for (m in rev(seq_along(matches))) {
                if (is.function(replacements[[j]])) {
                    # Function replacement: call function with captures
                    substitution = do_fn_replace(replacements[[j]], matches[[m]])
                } else {
                    # Expression replacement
                    if (is(replacements[[j]], "expr_alt")) {
                        if (!is(patterns[[j]], "expr_alt") || length(patterns[[j]]) != length(replacements[[j]])) {
                            stop("When replacement is a set of alternatives, pattern must also be a set of alternatives of the same length.")
                        }
                        substitution = replacements[[j]][[matches[[m]]$alt]];
                    } else {
                        substitution = replacements[[j]];
                    }

                    # Replace matchable names in substitution with correct contents
                    for (k in seq_along(matches[[m]])) {
                        nm = names(matches[[m]])[k];
                        if (nm %like% "^\\.") {
                            substitution = replace_name(substitution, as.name(nm), matches[[m]][[k]]);
                        }
                    }
                }
                x = `expr_sub<-`(x, matches[[m]]$loc, value = substitution)
            }
        }
        return (x)
    })

    # Return result
    if (is(expr, "expr_wrap")) {
        return (result[[1]])
    } else {
        attributes(result) = attributes(expr)
        return (result)
    }
}

# Replace a name within an expression with another expression
replace_name = function(expr, name, replacement)
{
    if (expr == name) {
        expr = replacement;
    } else if (length(expr) > 1) {
        for (i in seq_along(expr)) {
            expr[[i]] = replace_name(expr[[i]], name, replacement)
        }
    }
    return (expr)
}

# Call a replacement function with captures from a match
do_fn_replace = function(fn, match_obj)
{
    # Build match list with leading dots stripped from capture names
    m = list()
    for (k in seq_along(match_obj)) {
        nm = names(match_obj)[k]
        if (nm %like% "^\\.") {
            m[[sub("^\\.+", "", nm)]] = match_obj[[k]]
        } else {
            m[[nm]] = match_obj[[k]]
        }
    }

    fn(m)
}
