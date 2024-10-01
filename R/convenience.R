#' @usage NULL
#' @rdname expr_match
#' @export
expr_count = function(expr, pattern, n = Inf, env = parent.frame())
{
    matches = do.call(expr_match, list(substitute(expr), substitute(pattern),
        n = n, env = env))
    if (is(matches, "expr_match") || is.null(matches)) {
        return (length(matches))
    } else { # Implies `expr` was multi-part
        return (lengths(matches))
    }
}

#' @usage NULL
#' @rdname expr_match
#' @export
expr_detect = function(expr, pattern, n = Inf, env = parent.frame())
{
    matches = do.call(expr_match, list(substitute(expr), substitute(pattern),
        n = n, env = env))
    if (is(matches, "expr_match") || is.null(matches)) {
        return (class(matches) == "expr_match")
    } else { # Implies `expr` was multi-part
        return (!sapply(matches, is.null))
    }
}

#' @usage NULL
#' @rdname expr_match
#' @export
expr_extract = function(expr, pattern, what = "match", n = Inf,
    dotnames = FALSE, gather = FALSE, env = parent.frame())
{
    matches = do.call(expr_match, list(substitute(expr), substitute(pattern),
        n = n, dotnames = dotnames, env = env))
    if (is.null(matches)) {
        return (if (gather) list() else NULL)
    } else if (is(matches, "expr_match")) {
        x = lapply(matches, `[[`, what)
        return (x)
    } else { # Implies `expr` was multi-part
        x = lapply(matches, function(m) if (is.null(m)) NULL else lapply(m, `[[`, what))
        return (if (gather) unlist(x, recursive = FALSE) else x)
    }
}

#' @usage NULL
#' @rdname expr_match
#' @export
expr_locate = function(expr, pattern, n = Inf, gather = FALSE, env = parent.frame())
{
    do.call(expr_extract, list(substitute(expr), substitute(pattern),
        what = "loc", n = n, dotnames = FALSE, gather = gather, env = env))
}
