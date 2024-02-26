#' Make a list of expressions
#'
#' Constructs a list of expressions, with support for Elixir's special
#' [expression] syntax (expression literals with `{}`, alternatives with `?`,
#' and recursion with `~`).
#'
#' Be aware that using the `[[` indexing operator on an object of class
#' `expr_list` discards information about whether that element of the list is
#' marked for recursion. In other words, if `xl <- expr_list({.A}, ~{.A})`,
#' then `xl[[1]]` and `xl[[2]]` are both equal to the "bare" symbol `.A`, so
#' the information that the second element of the list should recurse has been
#' lost. Consequently, in e.g. `expr_match(expr, xl[[2]])`, it will be as
#' though the tilde isn't there, and no recursion into `expr` will be done. Use
#' the `[` operator instead, which retains recursion information;
#' `expr_match(expr, xl[2])` will work as expected.
#'
#' Note that when you replace part of an `expr_list` with another `expr_list`,
#' the recursion information from the "replacement" `expr_list` is copied over,
#' while replacing part of an `expr_list` with an expression or a "plain" list
#' of expressions retains the existing recursion information.
#'
#' @param ... Expressions to include in the list. If the arguments are named,
#' these will be passed on to the returned list.
#' @param env Environment for injections in `...` (see [expression]).
#' @param xl An `expr_list`.
#' @param i Index for subsetting the `expr_list`; an integer, numeric, logical,
#' or character vector (for named `expr_list`s) interpreted in the usual R way.
#' @param value Replacement; an `expr_list`, an expression, or a list of
#' expressions.
#' @return A list of expressions, of class `expr_list`.
#' @examples
#' expr_list(
#'    { 1 + 1 = 2 } ? { 2 + 2 = 4 },
#'    { y = a * x + b },
#'    ~{ .A }
#' )
#'
#' # There is support for rlang's injection operators.
#' var = as.name("myvar")
#' expr_list({ 1 }, { !!var }, { (!!var)^2 })
#' @order 1
#' @export
expr_list = function(..., env = parent.frame())
{
    # Special case for 0 length list
    if (...length() == 0) {
        return (structure(list(), class = "expr_list", into = logical(0)))
    }

    # Get expressions from ...
    args = substitute(a(...));
    expr = list();
    for (i in seq_len(...length())) {
        expr[[i]] = do.call(do_parse, list(args[[i + 1]], env));
    }

    names(expr) = ...names()
    into = c(unlist(sapply(expr, attr, "into")))
    expr = structure(unlist(expr, recursive = FALSE), class = "expr_list", into = into)

    return (expr)
}

#' @rdname expr_list
#' @order 2
#' @export
`[.expr_list` = function(xl, i)
{
    if (missing(i)) {
        return (xl)
    }
    structure(`[.simple.list`(xl, i), class = "expr_list", into = attr(xl, "into")[i])
}

#' @rdname expr_list
#' @order 3
#' @export
`[<-.expr_list` = function(xl, i, value)
{
    if (is.null(value)) {
        # value is NULL: omit elements in i
        return (structure(`[.simple.list`(xl, -i), class = "expr_list", into = attr(xl, "into")[-i]))
    }

    l = unclass(xl); # To avoid recursing into this function below
    into = attr(xl, "into");
    nreplace = length(into[i]);

    if (is(value, "expr_list")) {
        l[i] = rep_len(value, nreplace);
        into[i] = rep_len(attr(value, "into"), nreplace);
    } else if (is.list(value)) {
        if (any(!sapply(value, function(x) { rlang::is_expression(x) || class(x) == "expr_alt" }))) {
            stop("When replacing part of an expr_list with a list, each element of that list must be a valid expr_list element.")
        }
        l[i] = rep_len(value, nreplace);
    } else if (rlang::is_expression(value)) {
        l[i] = rep_len(list(value), nreplace);
    } else {
        stop("Can only replace part of an expr_list with an expr_list, a list of expressions, or an expression.")
    }
    structure(l, class = "expr_list", into = into);
}

#' @keywords internal
#' @export
print.expr_list = function(x, ...)
{
    if (length(x) != length(attr(x, "into"))) {
        warning("Corrupted expr_list: length of list not equal to length of 'into' attribute.")
    }

    fmt_expr = function(expr, tilde) {
        paste(if (tilde) '~{' else '{', format(expr), '}')
    }

    str = mapply(
        function(y, i, n) {
            if (n != "") {
                n = paste0(n, " = ")
            }
            if (is(y, "expr_alt")) {
                return (paste0(n, paste0(mapply(fmt_expr, y, attr(y, "into")), collapse = " ? ")))
            } else {
                return (paste0(n, fmt_expr(y, i)))
            }
        },
        x, attr(x, "into"), rlang::names2(x))

    cat("expr_list of length ", length(x), ": ", paste0(str, collapse = ", "), sep = "")
}

#' This exists primarily so that `expr_apply` can be applied to an `expr_list`,
#' which may potentially contain elements of class `expr_apply`.
#'
#' @keywords internal
#' @export
`[<-.expr_alt` = function(xl, i, value)
{
    if (is.null(value)) {
        # value is NULL: omit elements in i
        return (structure(`[.simple.list`(xl, -i), class = "expr_alt", into = attr(xl, "into")[-i]))
    }

    l = unclass(xl); # To avoid recursing into this function below
    into = attr(xl, "into");
    nreplace = length(into[i]);

    if (is(value, "expr_list")) {
        l[i] = rep_len(value, nreplace);
        into[i] = rep_len(attr(value, "into"), nreplace);
    } else if (is.list(value)) {
        if (any(!sapply(value, rlang::is_expression))) {
            stop("When replacing part of an expr_alt with a list, each element of that list must be a valid expression.")
        }
        l[i] = rep_len(value, nreplace);
    } else if (rlang::is_expression(value)) {
        l[i] = rep_len(list(value), nreplace);
    } else {
        stop("Can only replace part of an expr_alt with an expr_list, a list of expressions, or an expression.")
    }
    structure(l, class = "expr_alt", into = into);
}

#' Get or set a subexpression
#'
#' These functions allow you to extract and/or modify a subexpression within an
#' expression.
#'
#' The Elixir functions [expr_match()] and [expr_locate()] find matching
#' "subexpressions" within expressions and return indices that allow accessing
#' these subexpressions. For example, the expression `1 + 2 + 3` contains all
#' the following subexpressions:
#'
#' | **index** | **subexpression** | **accessed with R code**              |
#' |:--------- |:-----------------:|:------------------------------------- |
#' | `NULL`    | `1+2+3`           | `expr`                                |
#' | `1`       | `+`               | `expr[[1]]`                           |
#' | `2`       | `1+2`             | `expr[[2]]`                           |
#' | `3`       | `3`               | `expr[[3]]`                           |
#' | `c(2,1)`  | `+`               | `expr[[2]][[1]]` or `expr[[c(2, 1)]]` |
#' | `c(2,2)`  | `1`               | `expr[[2]][[2]]` or `expr[[c(2, 2)]]` |
#' | `c(2,3)`  | `2`               | `expr[[2]][[3]]` or `expr[[c(2, 3)]]` |
#'
#' Any index returned by [expr_match()] or [expr_locate()] will either be
#' `NULL` (meaning the whole expression / expression list) or an integer vector
#' (e.g. `1` or `c(2,3)` in the table above).
#'
#' Suppose you have an index, `idx`. If `idx` is an integer vector, you can
#' just use `expr[[idx]]` to access the subexpression. But in the case where
#' `idx` is `NULL`, R will complain that you are trying to select less than one
#' element. The sole purpose of [expr_sub()] is to get around that issue and
#' allow you to pass either `NULL` or an integer vector as the index you want
#' for an expression or list of expressions.
#'
#' @param expr The expression to select from. Can also be a list of
#'     expressions, in which case the first element of `index` selects the
#'     expression from the list.
#' @param idx A valid index: `NULL` or an integer vector.
#' @param env Environment for any injections in `expr` (see [expression]).
#' @param value Replacement; an expression.
#' @return The subexpression, which can be modified.
#' @seealso [expr_match()], [expr_locate()] which return indices to
#' subexpressions.
#' @examples
#' expr = expr_list({ y = a * x + b })[[1]]
#' expr_sub(expr, NULL)
#' expr_sub(expr, 3)
#' expr_sub(expr, c(3, 3))
#'
#' expr_sub(expr, c(3, 3)) <- expr_list({ q })[[1]]
#' print(expr)
#' @export
expr_sub = function(expr, idx, env = parent.frame())
{
    expr = do.call(do_parse_simple, list(substitute(expr), env))
    if (is(expr, "expr_wrap")) {
        expr = expr[[1]]
    }

    if (length(idx) == 0) {
        return (expr)
    } else {
        return (expr[[idx]])
    }
}

#' @rdname expr_sub
#' @export
`expr_sub<-` = function(expr, idx, env = parent.frame(), value)
{
    expr = do.call(do_parse_simple, list(substitute(expr), env))
    if (is(expr, "expr_wrap")) {
        expr = expr[[1]]
    }

    if (length(idx) == 0) {
        return (value)
    } else {
        expr[[idx]] = value;
        return (expr)
    }
}

# Parses expressions passed to the expr_* functions with brace-wrapped
# expression literals, also including information on how to return the result
# in the "original" format.
do_parse_simple = function(expr, env = parent.frame())
{
    # Unevaluated expr as list
    lx = as.list(substitute(expr));

    # Special case to handle passing NULL
    if (length(lx) == 0) {
        return (structure(list(NULL), class = "expr_wrap", into = NA))
    }

    # Check for mistakes in specifying -- tilde or question mark
    if (length(lx) > 1 && (identical(lx[[1]], quote(`~`)) || identical(lx[[1]], quote(`?`)))) {
        stop("Do not use recursion operator (~) or alternatives operator (?) in first argument.", call. = FALSE)
    }

    # Convert env to an environment if it is a list
    if (is.list(env)) {
        env = list2env(env)
    }

    # Parse argument
    lx = debrace(lx, substitute(expr), env)
    if (is.list(lx)) {
        # Already list
        return (lx)
    }

    return (structure(list(lx), class = "expr_wrap", into = NA))
}

# Parses pattern/replacement expressions passed to the expr_* functions with a
# special syntax including brace-wrapped expression literals, ? for enumerating
# alternatives and ~ for specifying that the expression is a pattern that
# should be recursed into the parse tree being searched.
do_parse = function(expr, env = parent.frame())
{
    # Unevaluated expr as list
    lx = as.list(substitute(expr));

    # Special cases needed to handle passing NULL or ~NULL
    if (length(lx) == 0) {
        return (structure(list(NULL), class = "expr_list", into = FALSE))
    } else if (identical(lx, list(quote(`~`), NULL))) {
        return (structure(list(NULL), class = "expr_list", into = TRUE))
    }

    # Convert env to an environment if it is a list
    if (is.list(env)) {
        env = list2env(env)
    }

    # Parse argument
    # Handle alternatives (`?` notation)
    if (identical(lx[[1]], quote(`?`))) {
        if (length(lx) == 2 && !(is.call(lx[[2]]) && identical(lx[[2]][[1]], quote(`{`)))) {
            # Format: ?z or ?~z, where z is something evaluating to a list, but is NOT brace wrapped
            if (is.call(lx[[2]]) && identical(lx[[2]][[1]], quote(`~`)) && length(lx[[2]]) == 2) {
                alts = eval(lx[[2]][[2]], env)
                into = TRUE;
            } else {
                alts = eval(lx[[2]], env)
                into = FALSE;
            }
            if (!is.list(alts)) {
                stop("In ?x or ?~x, x must evaluate to a list.")
            }
            # This can be returned directly with no further processing
            return (structure(list(structure(alts, class = "expr_alt", into = rep(into, length(alts)))),
                class = "expr_list", into = NA))
        } else if (length(lx) == 3) {
            # Format: a ? b [? c ...]
            result = structure(dequestion(lx), class = "expr_alt")
            to_eval = dequestion(substitute(expr))

            result = list(detilde(result))
            to_eval = list(detilde(to_eval, TRUE))
            attr(result, "into") = NA
        } else {
            stop("Could not parse ", lang2str(substitute(expr)))
        }
    } else {
        # Single alternative (no ?) -- one-element list
        result = detilde(list(lx))
        to_eval = detilde(list(substitute(expr)), TRUE)
    }

    # Process the list
    for (i in seq_along(result)) {
        if (is(result[[i]], "expr_alt")) {
            for (j in seq_along(result[[i]])) {
                result[[i]][j] = list(debrace(result[[i]][[j]], to_eval[[i]][[j]], env))
            }
        } else {
            result[i] = list(debrace(result[[i]], to_eval[[i]], env))
            if (is(result[[i]], "expr_list")) {
                # Already expr_list
                return (result[[i]])
            } else if (is.list(result[[i]])) {
                # Evaluates to a list -- treat this as the result in itself
                result = structure(result[[i]], into = rep(attr(result, "into"), length(result[[i]])))
                break
            }
        }
    }

    # Set attributes and return; this particular resetting of both class and
    # into is to ensure order of these two attributes is the same, which is not
    # strictly required but may be good for consistency's sake.
    attributes(result) = list(class = "expr_list", into = attr(result, "into"))
    return (result)
}

# Removes tildes and stores their presence as TRUE in attribute "into"
detilde = function(x, to_eval = FALSE)
{
    into = rep(FALSE, length(x))
    for (i in seq_along(x)) {
        if (!is.name(x[[i]]) && identical(x[[i]][[1]], quote(`~`))) {
            into[i] = TRUE
            if (to_eval) {
                x[[i]] = x[[i]][[2]]
            } else {
                x[[i]] = as.list(x[[i]][[2]])
            }
        }
    }
    structure(x, into = into)
}

# Unwraps a ? b [ ? c [ ? d ... ]] into a list
dequestion = function(x)
{
    if (is.call(x[[2]]) && identical(x[[2]][[1]], quote(`?`))) {
        head = dequestion(x[[2]])
    } else {
        head = list(x[[2]])
    }
    return (c(head, list(x[[3]])))
}

# If x is brace-wrapped, remove braces and inject, otherwise return ev
debrace = function(x, ev, env)
{
    if ((is.list(x) || is.call(x)) && identical(x[[1]], quote(`{`))) {
        # If element is { }-wrapped, treat as quotation; debrace and inject
        if (length(x) != 2) {
            stop("Contents of { } must be a single expression.")
        } else {
            return (rlang::inject(rlang::expr(!!x[[2]]), env))
        }
    } else {
        # Otherwise provide the evaluation of expr in the parent frame
        return (eval(ev, env))
    }
}

#' Expressions in Elixir
#'
#' @description Elixir is primarily a package for working with what it calls
#' "expressions", in the sense of any R object for which
#' [rlang::is_expression()] returns `TRUE`. This includes calls, like the
#' results of evaluating `quote(f(x))` or `quote(a:b)`, symbols like
#' `quote(z)`, and syntactic literals like `2.5`, `"hello"`, `NULL`, `FALSE`,
#' and so on.
#'
#' This is not to be confused with the built-in type [base::expression], which
#' is essentially a special way of storing a vector of multiple "expressions".
#' Elixir does not use this type; see [expr_list()] instead.
#'
#' @section Usage:
#' ```
#' expr_list(number = { `.A:numeric` } ? { `.A:integer` },
#'     string = { `.A:character` }, symbol = { `.A:name` })
#' expr_match({ 1 * 2 }, { .A * .B })
#' expr_match({ 1 * 2 }, ~{ `.A:numeric` })
#' expr_replace({ y = a*x^3 + b*x^2 + c*x^1 + d*x^0 },
#'     { ..X ^ ..N }, { pow(..X, ..N) })
#' ```
#'
#' @section Specifying expressions in Elixir:
#'
#' The Elixir package functions starting with `expr_` work with expressions.
#' These functions all accept a special (optional) syntax for specifying
#' expressions which involves the symbols `{}`, `?`, and `~`, as well as the
#' rlang [injection operator, !!][rlang::injection-operator] and
#' [splice operator, !!!][rlang::splice-operator]).
#'
#' With base R, if you want to store an expression such as `x + y` in a
#' variable or pass it to a function, you need to use [base::quote()] or
#' [rlang::expr()], but any Elixir `expr_` function will also accept an
#' "expression literal" wrapped in braces, `{}`.
#'
#' So, for example, rather than
#'
#' `expr_translate(quote(x ^ y), "C++")`
#'
#' you can write
#'
#' `expr_translate({ x ^ y }, "C++")`.
#'
#' This only works if the braces are provided "directly"; that is, in
#'
#' `expr <- quote({ x ^ y }); expr_translate(expr, "C++")`,
#'
#' the braces are not interpreted in any special way.
#'
#' Anything between the braces essentially gets put through [rlang::expr()], so
#' you can use `!!` (i.e. [rlang::injection-operator]) and `!!!` (i.e.
#' [rlang::splice-operator]). There is an `env` parameter to all relevant
#' Elixir functions, defaulting to `parent.frame()`, in which these injection
#' operations are evaluated.
#'
#' @section Special syntax for patterns and replacements:
#'
#' Additionally, some functions ([expr_match()], [expr_count()],
#' [expr_detect()], [expr_extract()], [expr_locate()], and [expr_replace()])
#' take `pattern` and/or `replacement` arguments to specify patterns to match
#' to an expression and/or replacement expressions to replace those matches
#' with.
#'
#' For both `pattern` and `replacement` arguments, you can use the question
#' mark operator `?` to specify *alternatives*. For example, to match *either*
#' the token `cat` or `dog`, you can use
#'
#' `expr_match(expr, { cat } ? { dog })`.
#'
#' You can chain together as many alternatives as are needed. Alternatively,
#' if you have a list of expressions `z`, you can use a single question mark
#' before the name of the list, like so:
#'
#' `expr_match(expr, ?z)`
#'
#' and Elixir will treat the list as a set of alternatives. When using
#' [expr_replace()] with a set of alternatives as the pattern, the replacement
#' needs to be either a single expression, or a set of alternative expressions
#' which has the same number of alternatives as in the pattern.
#'
#' You can also use the tilde operator `~` to specify that a given pattern
#' should "recurse into" an expression. For example, in
#'
#' ```
#' exprs = expr_list(2, 5, {1 + 4})
#' expr_match(exprs, { `.A:numeric` })
#' ```
#'
#' only the numbers `2` and `5` will match. However, in
#'
#' ```
#' exprs = expr_list(2, 5, {1 + 4})
#' expr_match(exprs, ~{ `.A:numeric` })
#' ```
#'
#' all numbers `2`, `5`, `1` and `4` will match, because the `pattern` can
#' recurse into the third expression `1 + 4`.
#'
#' @name expression
NULL
