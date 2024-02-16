#' Apply a function over expressions
#'
#' Recursively apply a function over an [expression], or any [expression]
#' elements of a list, and optionally the subexpressions within any
#' expressions.
#'
#' The function `f` can take one to three arguments. The first argument is the
#' expression itself for `f` to apply to, and `f` should return some kind of
#' replacement for, or modified version of, this argument.
#'
#' The second argument is a list with information about the name of the
#' expression in the list `x` and of its parents. Specifically, the first
#' element of the list is the name of the expression, the second element of the
#' list is the name of the "parent" of the expression, and so on. If any
#' elements in this chain are unnamed, an integer is provided as the name. If
#' the expression is within another expression (which only happens with
#' `into = TRUE`), this is signalled as a `NULL` at the top of the list, one
#' for each level of recursion into the expression.
#'
#' The third argument is an integer vector, the index into `x` where
#' `f` is currently operating. This is suitable for use with [expr_sub()].
#'
#' @param x The R object; can an [expression], or a list of arbitrary
#' nestedness potentially containing [expression]s.
#' @param f Function to apply to all expressions within `x`; takes 1 to 3
#' arguments.
#' @param depth How many levels to recurse into lists; default is `Inf`.
#' @param into Whether to recurse into expressions. Can be `TRUE` to visit all
#' subexpressions, `FALSE` to not recurse, or `"leaves"` to recurse and only
#' apply `f` to terminal nodes of expressions (i.e. the symbols and syntactic
#' literals comprising the expressions).
#' @param order Whether a parent node is visited before ("pre") or after
#' ("post") its children (the terminology comes from pre-order and post-order
#' depth-first search). This only has an effect if `into == TRUE`.
#' @param how How to structure the result.
#' @param env Environment for injections in `x` (see [expression]).
#' @return If `how = "replace"` (the default), the original object `x` with `f`
#' applied to expressions within it. If `how = "unlist"`, the same but with
#' [unlist()] applied to it. If `how = "unique"`, first [unlist()] then
#' [unique()] are applied.
#' @examples
#' expr_apply(list(quote(a + b), quote(c)), function(x) all.vars(x), how = "unlist")
#' @export
expr_apply = function(x, f, depth = Inf, into = FALSE, order = c("pre", "post"),
    how = c("replace", "unlist", "unique"), env = parent.frame())
{
    # Parse x, although this use case seems unlikely
    x = do.call(do_parse_simple, list(substitute(x), env));
    if (is(x, "expr_wrap")) {
        x = x[[1]];
    }

    # Check form of f
    f_args = formals(f);
    if (length(f_args) == 1) {
        ff = function(x, name, idx) f(x);
    } else if (length(f_args) == 2) {
        ff = function(x, name, idx) f(x, name);
    } else if (length(f_args) == 3) {
        ff = f;
    } else {
        stop("f must take either one two, or three arguments.")
    }

    # Check arguments
    if (!into %in% list(TRUE, FALSE, "leaves")) {
        stop("into must be either TRUE, FALSE, or \"leaves\".");
    }
    order = match.arg(order);
    how = match.arg(how);

    # Do the apply
    if (how == "unlist") {
        return (unlist(apply_sub(x, ff, depth, into, order, list(), integer(0))))
    } else if (how == "unique") {
        return (unique(unlist(apply_sub(x, ff, depth, into, order, list(), integer(0)))))
    } else {
        return (apply_sub(x, ff, depth, into, order, list(), integer(0)))
    }
}

# Workhorse for expr_apply
apply_sub = function(x, f, depth, into, order, name, idx)
{
    if (depth > 0 && (is.list(x) || is.expression(x) || is.pairlist(x))) {
        # If x is listy, and we are permitted to enter it, do so
        for (i in seq_along(x)) {
            if (is.null(names(x)) || names(x)[i] == "") {
                nm = i;
            } else {
                nm = names(x)[i];
            }
            # If we did x[[i]] = apply_sub(...), then if apply_sub returns NULL, we
            # inadvertently delete x[[i]]. We don't want that, so do the below. This
            # works for list and (base-R) expression types; works also for pairlists,
            # though in that case it changes the result to a plain list. This also
            # works for expr_lists, thanks to our code for `[<-.expr_list` and
            # `[<-.expr_alt`.
            x[i] = list(apply_sub(x[[i]], f, depth - 1, into, order, c(list(nm), name), c(idx, i)));
        }
    } else if (rlang::is_expression(x)) {
        # For expressiony x, apply f, perhaps to all expression parts
        # Note that rlang::is_expression(x) is TRUE for syntactic literals too
        x = apply_sub_expr(x, f, into, order, name, idx);
    }

    return (x)
}

# Workhorse for expr_apply, once we are already in an "rlang" expression
apply_sub_expr = function(x, f, into, order, name, idx)
{
    # If leaves only: recurse into expression until encountering a leaf
    if (into == "leaves") {
        if (is.call(x)) {
            for (i in seq_along(x)) {
                x[i] = list(apply_sub_expr(x[[i]], f, into, order, c(list(NULL), name), c(idx, i)));
            }
            return (x)
        } else {
            return (f(x, name, idx))
        }
    }

    # If not leaves only: pre or post run f and (potentially) recurse into calls
    if (order == "pre") {
        x = f(x, name, idx);
    }

    if (into == TRUE && is.call(x)) {
        # If into == TRUE, recurse through all parts of call
        for (i in seq_along(x)) {
            # As above, we don't want to do x[[i]] = apply_sub_expr(...),
            # in case the result is NULL.
            x[i] = list(apply_sub_expr(x[[i]], f, into, order, c(list(NULL), name), c(idx, i)));
        }
    }

    if (order == "post") {
        x = f(x, name, idx);
    }

    return (x)
}
