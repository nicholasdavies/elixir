#' Replace patterns within expressions
#'
#' Match and replace elements of patterns in an [expression] or a list of
#' expressions.
#'
#' Patterns follow the syntax for [expr_match()].
#'
#' @usage
#' expr_replace(expr, ..., patterns, replacements,
#'     n = Inf, env = parent.frame())
#' @param expr Input. An [expression], [expr_list], or [list()] of expressions.
#' @param ... Alternating series of patterns and replacements, each a single
#'     [expression] (though alternatives can be specified with `?`).
#' @param patterns Patterns to look for. An [expression], [expr_list], or
#'     [list()] of expressions.
#' @param replacements Replacements, one for each pattern.
#' @param n Maximum number of times for each expression to make each
#'     replacement; default is `Inf`.
#' @param env Environment for injections in `expr`, `pattern` (see
#'     [expression]).
#' @return The input expression(s) with any replacements made.
#' @seealso [expr_match()] to find patterns in expressions, and its cousins
#' [expr_count()], [expr_detect()], [expr_extract()], and [expr_locate()].
#' @examples
#' # Example with alternating patterns and replacements
#' expr_replace({ 1 + 2 }, ~{1}, {one}, ~{2}, {two})
#'
#' # Example with patterns and replacements in a list
#' expr_replace({ 1 + 2 }, patterns = expr_list(~{1}, ~{2}),
#'     replacements = expr_list({one}, {two}))
#'
#' # Replace with captures
#' expr_replace({ 1 + 2 }, { .A + .B }, { .A - .B })
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
            if (is(patterns[[j]], "expr_alt")) {
                if (is(replacements[[j]], "expr_alt") && length(replacements[[j]]) != length(patterns[[j]])) {
                    stop("If there are alternatives for replacements, there must be the same number as alternatives for patterns.")
                }
                # Alternatives: try each, stop after first successful replacement.
                for (p in seq_along(patterns[[j]])) {
                    repl = if (is(replacements[[j]], "expr_alt")) replacements[[j]][[p]] else replacements[[j]];
                    sub = replace_sub(x, patterns[[j]][[p]], repl, n, attr(patterns[[j]], "into")[p], env);
                    x = sub[[1]];
                    if (sub[[2]]) {
                        break;
                    }
                }
            } else {
                x = replace_sub(x, patterns[[j]], replacements[[j]], n, attr(patterns, "into")[j], env)[[1]];
            }
        }
        return (x)
    });

    # Return result
    if (is(expr, "expr_wrap")) {
        return (result[[1]])
    } else {
        attributes(result) = attributes(expr)
        return (result)
    }
}

# Workhorse for replace_language
replace_sub = function(expr, pattern, replacement, n, into, env)
{
    replaced = FALSE;
    if (n <= 0) {
        return (list(expr, replaced))
    }

    if (!is.null(m <- match_sub(expr, pattern, n, FALSE, longnames = TRUE, NULL, NULL, env))) {
        # If match: substitute placeholders in 'replacement' with matches
        n = n - 1;
        expr = replacement;
        for (j in seq_along(m[[1]])) {
            nm = names(m[[1]])[j];
            if (nm %like% "^\\.") {
                expr = replace_name(expr, as.name(nm), m[[1]][[j]]);
            }
        }
        replaced = TRUE;
        if (n <= 0) {
            return (list(expr, replaced))
        }
    }

    # Step down a level if requested
    if (into && is.call(expr)) {
        result = list()
        for (i in seq_along(expr)) {
            repl = replace_sub(expr[[i]], pattern, replacement, n, into, env)
            expr[[i]] = repl[[1]];
            replaced = replaced || repl[[2]];
            n = n - repl[[2]];
            if (n <= 0) {
                break;
            }
        }
    }

    return (list(expr, replaced))
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
