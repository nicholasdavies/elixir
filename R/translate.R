#' Translate an R expression
#'
#' @description
#' Takes an R expression (in the sense of [rlang::is_expression()]) and
#' translates it into a character string giving the equivalent expression in
#' another programming language, according to the supplied
#' [rules][elixir-rules].
#'
#' This function is experimental.
#'
#' @details
#' The parameter `rules` can be a character string naming a "built-in"
#' [ruleset][elixir-rules]. Otherwise, `rules` should be a list with the
#' following elements:
#' * `ops`: an unnamed list of operator definitions, each of which should be a
#'   list with four elements:
#'   - `arity` the number of operands
#'   - `prec` the precedence of the operator (lower numbers equal higher
#'      precedence)
#'   - `assoc` the associativity of the operator, either `"LTR"`, `"RTL"`, or
#'      anything else for no associativity
#'   - `str` a [glue::glue()] format string with `{A[1]}`, `{A[2]}`, etc.,
#'      standing in for the first, second, etc. operands.
#'   - `nopar` a numeric vector with indices of arguments to the operator which
#'      should never be enclosed in parentheses. The default and usual value is
#'      integer(0), but (for example) it can be 2 for the `[` operator, as
#'      parentheses within the second argument (the content of the brackets)
#'      are redundant.
#'
#'    The function `elixir:::op` can help to assemble such lists.
#' * `paren` a [glue::glue()] format string with `{x}` standing in for the
#'    enclosed expression. Describes how parentheses are expressed in the
#'    target language. Example: `"({x})"` is correct for virtually all
#'    programming languages.
#' * `symbol`: a function which takes a symbol and returns a character string,
#'    representing the name of that symbol in the target language. This
#'    could just be equal to [base::as.character], but it can be changed to
#'    something else in case you want name mangling, or e.g. some processing
#'    to replace `.` in symbols with some other character (as `.` are often
#'    not allowed as part of symbols in popular languages).
#' * `literal`: a named list in which the name refers to the class of the
#'    operand to translate, and the value should be a function of a single
#'    argument (the operand) returning a character string.
#'
#' It may be helpful to inspect `elixir:::ruleset` to clarify the above
#' format.
#'
#' There are some important shortcomings to [translate()]. Here are some
#' potential pitfalls:
#' * Named arguments are not supported, because we cannot translate an R
#'   function call like `mean(x, na.rm = TRUE)` without knowing which
#'   parameter of `mean` matches to `na.rm`.
#' * Division: An R expression like `1/3` gets translated into `1./3.` in
#'   C/C++, as numeric literals are coerced to type `double`. So both of these
#'   evaluate to 0.333. However, the R expression `1L/3L` will get translated
#'   into `1/3` in C/C++, which evaluates to 0 (as it is integer division).
#' * Modulo: R uses "Knuth's modulo", where `a %% b` has the same sign as `b`.
#'   Lua also uses Knuth's modulo, but C/C++ use "truncated modulo", where
#'   `a % b` has the same sign as `a`. (see
#'   [Wikipedia](https://en.wikipedia.org/wiki/Modulo#Variants_of_the_definition)
#'   for details). So when converting a modulo expression from R to C/C++, a
#'   call to the function `kmod` is generated in the C/C++ expression. This is
#'   not a standard library function, so you will have to provide a definition
#'   yourself. A workable definition is:
#'   `double kmod(double x, double y) { double r = fmod(x, y); return r && r < 0 != y < 0 ? r + y : r; }`
#'   (R) or `a % b` (Lua)
#' * Types: In R, the type of `a %% b` and of `a %/% b` depends on the type of
#'   `a` and `b` (if both are integers, the result is an integer; if at least
#'   one is numeric, the result is numeric).
#' * Chained assignment does not work in Lua.
#'
#' @param expr [Expression][elixir-expression] or list of
#'     [expressions][elixir-expression] to be translated.
#' @param rules Which [rules][elixir-rules] to follow. You can pass a string
#'     from among `"C"`, `"C++"`, `"Lua"`, or `"R"`, or a list with translation
#'     rules (see Details).
#' @param env Environment for injections in `expr` (see
#'     [expression][elixir-expression]).
#' @return The translated expression as a single character string.
#' @examples
#' translate({x ^ y}, "C++")
#' @export
translate = function(expr, rules, env = parent.frame())
{
    # Get rules
    rules = check_rules(rules,
        ops = list("list", NA, 'is.list(rule[[i]]) && identical(names(rule[[i]]), c("arity", "prec", "assoc", "str", "nopar"))'),
        paren = list("character", 'length(rule) == 1'),
        symbol = list("function"),
        literal = list("list", NA, 'class(rule[[i]]) == "function"')
    )

    # Parse expr
    expr = do.call(do_parse_simple, list(substitute(expr), env));

    # Do translating
    result = lapply(expr, function(x) translate_sub(x, rules)[[1]]);

    # Return result
    if (inherits(expr, "expr_wrap")) {
        return (result[[1]])
    } else {
        return (result)
    }
}

translate_sub = function(x, rules)
{
    if (is.call(x)) {
        # Calls
        f = as.character(x[[1]]) # get function name
        n = length(x) - 1        # get number of arguments

        # Process arguments
        args = lapply(x[-1], translate_sub, rules)
        A = sapply(args, `[[`, 1) # each argument as a string
        P = sapply(args, `[[`, 2) # precedence of each argument's top level call, i.e. "lower-level precedence"

        # TODO Do something with named elements! This could happen with
        # e.g. [ or [[ operator (named args), function calls (named args)
        if (any(rlang::have_name(args))) {
            stop("No support for named args. f is ", f, ", names are ", paste(names(x), collapse = ", "))
        }

        # Special treatment for "("
        # This is needed because we can't assign ( a high precedence (e.g. 0th)
        # in an ops table, as then e.g. (1+1) would come out ((1+1)) owing to
        # the lower precedence of 1. We can't assign it a low precedence (e.g.
        # 99th) because then e.g. (1) + (1), which is `+`(`(`(1), `(`(2)) comes
        # out as ((1)) + ((1)).
        if (f == "(" && n == 1) {
            return (list(glue::glue_data(list(x = A[1]), rules$paren), 0))
        }

        # Look for call in ops table
        call_idx = which(names(rules$ops) == f & sapply(rules$ops, `[[`, "arity") == n)
        if (length(call_idx) == 0 && is_identifier(x[[1]])) { # TODO make "is_identifier" customizable.
            # No ops table hit: parse as function
            return (list(paste0(f, "(", paste0(A, collapse = ", "), ")"), 0))
        } else if (length(call_idx) != 1) {
            stop("No unique match for call ", f, " with arity ", n)
        }
        call_def = rules$ops[[call_idx]]

        # Add parentheses as needed:
        # (1) Parentheses needed if lower-level precedence is lower (=higher
        # number) than top level precedence.
        needed = P > call_def$prec;
        # (2) Parentheses needed for a binary op if lower-level precedence is
        # equal to top level precedence, with parentheses on right operand for
        # LTR associativity and parentheses on left operand for RTL.
        if (n == 2) {
            needed = needed | (P == call_def$prec &
                ((call_def$assoc == "LTR" & c(FALSE, TRUE)) |
                 (call_def$assoc == "RTL" & c(TRUE, FALSE)))
            )
        }

        # Suppress parentheses where needed (e.g. second operand of [)
        needed = needed & !seq_along(P) %in% call_def$nopar;

        A[needed] =
            glue::glue_data(list(x = A[needed]), rules$paren)

        # Return result
        return (list(
            glue::glue_data(list(A = A), call_def$str),
            call_def$prec[1]
        ))
    } else if (is.name(x)) {
        # Symbol: use symbol translator
        return (list(rules$symbol(x), 0))
    } else {
        # Literal: use appropriate literal translator
        literal = rules$literal[[class(x)]];
        if (is.null(literal)) {
            stop("Cannot translate object of class ", class(x))
        }
        return (list(literal(x), 0))
    }
}

#' Convert an expression into a string
#'
#' The opposite of [str2lang()], [lang2str()] converts an
#' [expression][elixir-expression] into a character string. Note that
#' [lang2str()] does not support the normal expression syntax for `elixir`, so
#' just expects an already-parsed expression.
#'
#' This function is essentially identical to [deparse1()], which has been
#' available since R 4.0.0, except with `collapse = ""` instead of
#' `collapse = " "`.
#'
#' @param x Expression to convert to a string.
#' @return A character string suitable for printing.
#' @examples
#' lang2str(quote(a + b + c))
#' @export
lang2str = function(x)
{
    if (!rlang::is_expression(x)) {
        stop("lang2str only works with expressions.")
    }
    paste0(deparse(x, width.cutoff = 500), collapse = "")
}

# Is x identifier-like?
is_identifier = function(x)
{
    is.name(x) && as.character(x) %like% "^[[:alpha:].][[:alnum:]._]*"
}
