#' Find patterns in expressions
#'
#' Match and extract patterns in an [expression][elixir-expression] or a list of expressions.
#'
#' @usage
#' expr_match(expr, pattern, n = Inf,
#'     dotnames = FALSE, env = parent.frame())
#'
#' expr_count(expr, pattern, n = Inf, env = parent.frame())
#' expr_detect(expr, pattern, n = Inf, env = parent.frame())
#' expr_extract(expr, pattern, what = "match", n = Inf,
#'     dotnames = FALSE, gather = FALSE, env = parent.frame())
#' expr_locate(expr, pattern, n = Inf, gather = FALSE,
#'     env = parent.frame())
#' @param expr Input. An [expression][elixir-expression], [expr_list], or
#' [list()] of expressions.
#' @param pattern Pattern to look for. An [expression][elixir-expression], a
#'     length-one [expr_list], or a length-one [list] of expressions. The
#'     question mark syntax (see [expression][elixir-expression]) can be used
#'     to specify alternatives.
#' @param n Maximum number of matches to make in each expression; default is
#'     `Inf`.
#' @param dotnames Normally, patterns like `.A`, `..B`, `...C`, etc, are named
#'     just `A`, `B`, `C`, etc., in the returned matches, without the dot(s)
#'     before each name. With `dotnames = TRUE`, the dots are kept.
#' @param what (`expr_extract` only) Name of the pattern to extract (or
#'     `"match"`, the default, to extract the entire match).
#' @param gather (`expr_extract` and `expr_locate` only) Whether to only return
#'     the successful matches, in a single unnested list.
#' @param env Environment for injections in `expr`, `pattern` (see
#'     [expression][elixir-expression]).
#' @return `expr_match` returns, for each expression in `expr`, either `NULL`
#' if there is no match, or an object of class `expr_match` if there is a
#' match. If `expr` is a single expression, just a single `NULL` or
#' `expr_match` object will be returned, but if `expr` is a list of
#' expressions, then a list of all results will be returned.
#'
#' An `expr_match` object is a list containing the elements `alt` (if the
#' pattern contains several alternatives), `match`, `loc`, and
#' further elements corresponding to the capture tokens in `pattern` (see
#' below).
#'
#' For return values of `expr_count`, `expr_detect`, `expr_extract`, and
#' `expr_locate`, see below.
#'
#' @details
#' # Details
#'
#' All of these functions are used to check whether an
#' [expression][elixir-expression] matches a specific pattern, and if it does,
#' retrieve the details of the match. These functions are inspired by similar
#' functions in the `stringr` package.
#'
#' # Details for expr_match
#'
#' `expr_match` is the most general of the bunch. As an example, suppose you
#' had an expression containing the sum of two numbers (e.g.
#' `3.14159 + 2.71828`) and you wanted to extract the two numbers. You
#' could use the pattern `{ .A + .B }` to extract the match:
#'
#' ```
#' expr_match({ 3.14159 + 2.71828 }, { .A + .B })
#' ```
#'
#' This gives you a list containing all the matches found. In this case,
#' there is one match, the details of which are contained in an object of
#' class `expr_match`. This object contains the following elements:
#' - `match = quote(3.14159 + 2.71828)`, the entire match;
#' - `loc = NULL`, the location of the match within the expression ;
#' - `A = 3.14159`, the part of the match corresponding to the *capture token*
#'   `.A`;
#' - `B = 2.71828`, the part of the match corresponding to the *capture token*
#'   `.B`.
#'
#' We can also use a list of expressions for `expr`, as in:
#'
#' ```
#' ex <- expr_list({ x + y }, { kappa + lambda }, { p * z })
#' expr_match(ex, { .A + .B })
#' ```
#'
#' This returns a list with one entry for each element of the list `ex`; for
#' the expressions that match (`ex[[1]]` and `ex[[2]]`) an `expr_match` object
#' is returned, while for the expression that does not match (`ex[[3]]`),
#' `NULL` is returned.
#'
#' # Pattern syntax
#'
#' The `pattern` expression (e.g. `{.A + .B}` in the above) follows a special
#' syntax.
#'
#' ## Capture tokens
#'
#' First, these patterns can contain *capture tokens*, which are names starting
#' with one to three periods and match to the following:
#'
#' * `.A` matches any single token
#' * `..A` matches any sub-expression
#' * `...A` matches any number of function arguments
#'
#' Above, "A" can be any name consisting of an alphabetical character (`a-z`,
#' `A-Z`) followed by any number of alphanumeric characters (`a-z`, `A-Z`,
#' `0-9`), underscores (`_`), or dots (`.`). This is the name given to the
#' match in the returned list. Alternatively, it can be any name starting with
#' an underscore (e.g. so the entire token could be `._` or `..._1`), in which
#' case the match is made but the capture is discarded.
#'
#' Additionally, the single-token pattern (e.g. `.A`) can be extended as
#' follows:
#'
#' * Use `` `.A:classname` `` to require that the class of the object be
#' "classname" (or contain "classname" if the object has multiple classes); so
#' e.g. `` `.A:name` `` matches a single name (i.e. symbol).
#' * Use `` `.A/regexp` `` to require a regular expression match `regexp`; so
#' e.g. `` `.A:name/ee` `` will match symbols with two consecutive lowercase
#' letter 'e's;
#' * Use `` `.A|test` `` to require that the expression `test` evaluates to
#' `TRUE`, where `.` can be used as a stand-in for the matched token; so e.g.
#' `` `.A:numeric|.>5` `` will match numbers greater than 5.
#'
#' The `regexp` and `test` specifiers cannot be used together, and have to come
#' after the `classname` specifier if one appears. These special syntaxes
#' require the whole symbol to be wrapped in backticks, as in the examples
#' above, so that they parse as symbols.
#'
#' ## Matching function arguments
#'
#' If you wish to match a single, unnamed function argument, you can use a
#' capture token of the form `.A` (single-token argument) or `..B` (expression
#' argument). To match all arguments, including named ones, use a capture token
#' of the form `...C`. For example, these all match:
#'
#' ```
#' expr_match({ myfunc() }, { .F() })
#' expr_match({ myfunc(1) }, { .F(.X) })
#' expr_match({ myfunc(1 + 1) }, { myfunc(..X) })
#' expr_match({ myfunc(1, 2) }, { .F(.X, .Y) })
#' expr_match({ myfunc() }, { myfunc(...A) })
#' expr_match({ myfunc(1) }, { .F(...A) })
#' expr_match({ myfunc(2, c = 3) }, { myfunc(...A) })
#' ```
#'
#' but these do not:
#'
#' ```
#' expr_match({ myfunc() }, { .F(.X) })
#' expr_match({ myfunc() }, { .F(..X) })
#' expr_match({ myfunc(a = 1) }, { .F(.X) })
#' expr_match({ myfunc(a = 1 + 1) }, { .F(..X) })
#' expr_match({ myfunc(1,2) }, { .F(..X) })
#' expr_match({ myfunc(a = 1, b = 2) }, { .F(...X, ...Y) })
#' ```
#'
#' There may be support for named arguments in patterns in the future, e.g. a
#' pattern such as `{ f(a = .X) }` that would match an expression like
#' `{ f(a = 1) }`, but that is currently not supported. So currently you can
#' only match named function arguments using the `...X` syntax.
#'
#' ## Anchoring versus recursing into expressions
#'
#' If you want your anchor your pattern, i.e. ensure that the pattern will only
#' match at the "outer level" of your expression(s), without matching to any
#' sub-expressions within, use a tilde (`~`) outside the braces (see
#' [expression][elixir-expression] for details). For example,
#' `expr_match({1 + 2 + 3 + 4}, ~{..A + .B})` only gives one match, to the
#' addition at the outermost level of `1 + 2 + 3` plus `4`, but
#' `expr_match({1 + 2 + 3 + 4}, {..A + .B})` also matches to the inner
#' additions of `1 + 2` plus `3` and `1` plus `2`.
#'
#' ## Alternatives
#'
#' Finally, `pattern` can be a series of alternatives, using the operator `?`
#' for specifying alternatives (see [expression][elixir-expression] for
#' details). Results from the first matching pattern among these alternatives
#' will be returned, and the returned `expr_match` object will include a
#' special element named "alt" giving the index of the matching alternative
#' (see examples).
#'
#' # Details for `expr_count`, `expr_detect`, `expr_extract`, and `expr_locate`
#'
#' These shortcut functions return only some of the information given by
#' `expr_match`, but often in a more convenient format.
#'
#' `expr_count` returns an integer vector with one element for every expression
#' in `expr`, each element giving the number of matches of `pattern` found.
#'
#' `expr_detect` returns a logical vector with one element for every expression
#' in `expr`, each element giving whether at least one match of `pattern` was
#' found.
#'
#' `expr_extract` returns, for each expression in `expr`, a list of all the
#' complete matches. Or, by specifing a capture token name in the argument
#' `which`, those can be extracted instead. For example:
#'
#' ```
#' expr_extract(expr_list({(a+b)+(x+y)},
#'     {"H"*"I"}, {3+4}), {.A + .B}, "A")
#' ```
#'
#' gives `list(list(quote(a), quote(x)), NULL, list(3))`.
#'
#' Using `gather = TRUE` with `expr_extract` returns only the succesful matches
#' in a single, unnested list; so the above call to `expr_extract` with
#' `gather = TRUE` would give `list(quote(a), quote(x), 3)`.
#'
#' Finally, `expr_locate` is similar to `expr_extract` but it returns the
#' location within `expr` of each successful match.
#'
#' @seealso [expr_replace()] to replace patterns in expressions.
#' @order 1
#' @examples
#' expr_match({ 1 + 2 }, { .A + .B })
#'
#' # match to one of several alternatives
#' expr_match({ 5 - 1 }, { .A + .B } ? { .A - .B })
#' @export
expr_match = function(expr, pattern, n = Inf, dotnames = FALSE, env = parent.frame())
{
    # Parse arguments
    expr = do.call(do_parse_simple, list(substitute(expr), env));
    pattern = do.call(do_parse, list(substitute(pattern), env));
    if (length(pattern) != 1) {
        stop("expr_match requires one pattern.")
    }

    if (is(pattern[[1]], "expr_alt")) {
        into = attr(pattern[[1]], "into")
    } else {
        into = attr(pattern, "into")
    }

    # Do matching
    result = lapply(seq_along(expr), function(i) {
        loc_start = if (is(expr, "expr_wrap")) NULL else i
        match = match_sub(expr[[i]], pattern[[1]], n, into, dotnames = dotnames, loc_start, NULL, env);
        if (!is.null(match)) {
            return (structure(lapply(match, function(m) m[!names(m) %like% "^_"]), class = "expr_match"))
        }
        return (NULL)
    });

    # Return result
    if (is(expr, "expr_wrap")) {
        return (result[[1]])
    } else {
        return (result)
    }
}

check_pattern = function(pattern)
{
    # Check for disallowed naming of elements of the pattern
    # i.e. this picks up a pattern of f(a = 1)
    if (!is.null(names(pattern))) {
        stop("Support for named elements of pattern is not implemented. ",
            "If you wish to match function arguments, use the .F(...X) syntax. ",
            "Pattern with named elements is ", deparse(pattern), ".")
    }
    # Now check for conflicts in the actual symbols in pattern
    names = all.names(pattern);
    names = c(
        ".match", ".loc", ".alt",
        "..match", "..loc", "..alt",
        "...match", "...loc", "...alt",
        names[names %like% "^\\.+[a-zA-Z]|^\\.+_[a-zA-Z0-9._]"]);
    duplicates = names[duplicated(names)];
    if (length(duplicates) > 0) {
        warning("Name ", paste0('"', duplicates, '"', collapse = ", "), " reused in pattern ", format(pattern), ".")
    }
}

# Test for whether a singleton object matches the given requirements
token_match = function(obj, tm, parent_match, env)
{
    req_class = tm[, 4];
    req_regexp = if (!is.na(tm[, 6]) && tm[, 6] == "/") tm[, 7] else NA;
    req_test = if (!is.na(tm[, 6]) && tm[, 6] == "|") tm[, 7] else NA;
    match = TRUE;
    if (!is.na(req_class))  match = match && req_class %in% class(obj);
    if (!is.na(req_regexp)) match = match && as.character(obj) %like% req_regexp;
    if (!is.na(req_test))   match = match && eval(str2lang(req_test), c(setNames(list(obj), tm[, 2]), parent_match), env);
    return (match);
}

build_match = function(match, loc, captures, capture_names)
{
    setNames(c(list(match, loc), captures), c("match", "loc", capture_names))
}

# Workhorse for match_expr
match_sub = function(expr, pattern, n, into, dotnames, loc, parent_match, env)
{
    if (n <= 0) {
        return (NULL)
    }

    # Define subfunction to perform matching
    tpat = "^\\.([a-zA-Z_][a-zA-Z0-9._]*)(:([^/|]+))?((/|\\|)(.*))?$"; # token pattern
    xpat = "^\\.\\.([a-zA-Z_][a-zA-Z0-9._]*)$"; # expression pattern
    apat = "^\\.\\.\\.([a-zA-Z_][a-zA-Z0-9._]*)$"; # args pattern
    name = function(pre, nm) if (dotnames) paste0(pre, nm) else nm;

    do_match = function(pattern)
    {
        check_pattern(pattern);

        # How this is done depends on the format of the pattern (A-F):
        if (is.call(pattern) && length(pattern) == 2 && is.name(pattern[[2]]) &&
                !is.na((am <- stringr::str_match(as.character(pattern[[2]]), apat))[1, 1])) {
            # A. Pattern is f(...A) or .A(...B)
            if (is.call(expr)) {
                if (!is.na((tm <- stringr::str_match(as.character(pattern[[1]]), tpat))[1, 1])) {
                    # Pattern's function name seeks token match e.g. .A(...X), .A:name(...X), etc
                    if (token_match(expr[[1]], tm, parent_match, env)) {
                        # Sought token match succeeds
                        match = build_match(expr, loc,
                            list(expr[[1]], as.list(expr[-1])),
                            c(name(".", tm[, 2]), name("...", am[, 2])));
                    }
                } else if (pattern[[1]] == expr[[1]]) {
                    # Pattern's function name seeks exact match and succeeds
                    match = build_match(expr, loc,
                        list(as.list(expr[-1])),
                        name("...", am[, 2]));
                }
            }
        } else if (length(pattern) > 1) {
            # B. Pattern has multiple parts
            if (length(expr) == length(pattern) && identical(class(expr), class(pattern))) {
                match1 = list(match = expr, loc = loc);
                match2 = NULL;
                for (i in seq_along(expr)) {
                    if (!identical(rlang::names2(expr)[i], rlang::names2(pattern)[i])) {
                        match1 = NULL;
                        match2 = NULL;
                        break;
                    }
                    m = match_sub(expr[[i]], pattern[[i]], 1, FALSE, dotnames, c(loc, i),
                        c(if (is.null(parent_match)) match1 else parent_match, match2), env);
                    if (is.null(m)) {
                        # no match for part i: match has failed
                        match1 = NULL;
                        match2 = NULL;
                        break;
                    } else if (is.list(m)) {
                        # match for part i: match is succeeding so far
                        match2 = c(match2, m[[1]][-(1:2)]);
                    }
                }
                match = c(match1, match2);
            }
        } else if (is.name(pattern) && !is.na((m <- stringr::str_match(as.character(pattern), xpat))[1, 1])) {
            # C. Pattern is ..A -- match any expression
            match = build_match(expr, loc, list(expr), name("..", m[, 2]));
        } else if (is.name(pattern) && !is.na((tm <- stringr::str_match(as.character(pattern), tpat))[1, 1])) {
            # D. Pattern is .A -- match token, provided it is not named (this requires ...A)
            if (is.null(names(expr)) && (is.atomic(expr) || is.name(expr)) && token_match(expr, tm, parent_match, env)) {
                match = build_match(expr, loc, list(expr), name(".", tm[, 2]));
            }
        } else if (is.call(pattern) && !is.na((tm <- stringr::str_match(as.character(pattern), tpat))[1, 1])) {
            # E. Pattern is .A() -- match no-arg call
            if (is.call(expr) && token_match(expr, tm, parent_match, env)) {
                match = build_match(expr, loc, list(expr[[1]]), name(".", tm[, 2]));
            }
        } else {
            # F. Any other single-token pattern (i.e. a literal)
            if (identical(pattern, expr)) {
                match = list(match = expr, loc = loc);
            }
        }
        return (match)
    }

    # 1. First, attempt to match pattern to entire expr.
    match = NULL;

    # Do matching
    if (is(pattern, "expr_alt")) {
        # Alternative expressions: try each, get first matching one.
        for (p in seq_along(pattern)) {
            # Skip alternatives marked as not for matching (see 2. below)
            if (!is.na(into[p])) {
                match = do_match(pattern[[p]]);
                if (!is.null(match)) {
                    match = c(alt = p, match)
                    break;
                }
            }
        }
    } else {
        match = do_match(pattern);
    }

    # Package "top-level" match, if there is one, in a "list of all matches"
    if (!is.null(match)) {
        match = list(match)
        n = n - 1
        if (n <= 0) {
            return (match)
        }
    }

    # 2. Now attempt to match to subcomponents of expr -- i.e. if still looking
    # for matches, and can go into expression, then do so
    if (is(pattern, "expr_alt")) {
        # For those alternatives which we should not use for recursion, set
        # "into" to NA to mark that alternative as something we should not try
        # to match to.
        into[!into] <- NA
        attr(pattern, "into") <- into
    }
    if (any(into, na.rm = TRUE) && is.call(expr)) {
        for (i in seq_along(expr)) {
            m = match_sub(expr[[i]], pattern, n, into, dotnames, c(loc, i), NULL, env);
            if (!is.null(m)) {
                n = n - 1
                match = c(match, m);
                if (n <= 0) {
                    break;
                }
            }
        }
    }

    return (match)
}

#' @keywords internal
#' @export
print.expr_match = function(x, ...)
{
    fmt = function(x) {
        sapply(x, function(x) {
            if (is.integer(x)) { # so deparse doesn't turn e.g. c(2L,1L) into 2:1
                paste0("c(", paste0(x, "L", collapse = ", "), ")")
            } else {
                deparse(x, backtick = TRUE, control = "all")
            }
        })
    }

    if (is.null(x)) {
        cat("NULL")
    } else {
        str = sapply(seq_along(x),
            function(i) {
                paste0(names(x[[i]]), " = ", fmt(x[[i]]), collapse = ", ")
            }
        )
        str = paste0("  list(", str, ")", ifelse(seq_along(x) < length(x), ",", ""), collapse = "\n")

        cat("list(\n", str, "\n)\n", sep = "")
    }
}
