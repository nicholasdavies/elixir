# R keywords:
# Handled: if else repeat while for in next break TRUE FALSE
# Not handled: function NULL Inf NaN NA NA_integer_ NA_real_ NA_complex_ NA_character_
# R operators:
# Handled completely: unary - + !; binary ^ * / %% %//% + - < > <= >= == != && || <- -> = := %&% %|% %^% %<<% %>>% compl(.)
# Handled incompletely 1: : [seq not impl] & [vectorised] | [vectorised] <<- ->> [global assignment] [ [.at() handler and _missing not defined, no drop or exact]
# Handled incompletely 2: = [for function args] %user_special% [doesn't escape unallowed characters] %*% %o% %x% %in% [handlers not defined]
# Not handled: :: ::: $ @ [[ ~ ? (unary or binary)
# Assign-equals operators:
# %+=% %-=% %*=% %/=% %//=% %%=% (others?)
interpret_call = function(x, indent = 0, statement = FALSE, noindent = FALSE, index1 = "{x}.at({i})", mangle = "{x}", mode = "C++")
{
    cpp = NULL;
    f = "";
    tab = "    ";
    interp = function(x, statement = FALSE) interpret_call(x, indent, statement, FALSE, index1, mangle, mode);

    # singletons: symbols and atomics
    if (is.symbol(x)) {
        cpp = stringr::str_glue(mangle, x = x);
        if (cpp == "") { cpp = "_missing" }; # handle missing arguments, e.g. to the subscript operator
    } else if (is.atomic(x)) {
        if (is.na(x) || is.null(x)) {
            stop("No NAs or NULLs in C++...");
        } else if (is.integer(x) || is.numeric(x)) {
            cpp = as.character(x);
        } else if (is.logical(x)) {
            cpp = if (x) "true" else "false";
        } else if (is.character(x)) {
            cpp = paste0('"', x, '"');
        }
    }

    # everything else: functions
    if (is.null(cpp)) {
        f = as.character(x[[1]])    # function name
        n = length(x) - 1           # number of arguments
        a = x[2:(n + 1)]            # arguments

        # behaviour depends on function being called and on language...
        if (mode == "C++") {
            unary_ops_a = c("+", "-", "!", "compl");
            unary_ops_A = c("+", "-", "!", "~");
            binary_ops_a = c("+", "-", "*", "==", "!=", "<", ">", "<=", ">=",  "&", "&&",  "|", "||", "%&%", "%|%", "%^%", "%<<%", "%>>%");
            binary_ops_A = c("+", "-", "*", "==", "!=", "<", ">", "<=", ">=", "&&", "&&", "||", "||",   "&",   "|",   "^",   "<<",   ">>");
            binary_ops_b = c("/", "^", "%%", "%//%", "%*%");
            binary_ops_B = c("divf", "pow", "mod", "divi", "mulm");
            assign_ops = c("=", ":=", "<-", "<<-")
        } else if (mode == "Lua") { # TODO not implemented: &, |    # TODO eliminate := and <<-; maybe no assignment at all?
            unary_ops_a = c("+", "-", "!");
            unary_ops_A = c( "", "-", "~");
            binary_ops_a = c("+", "-", "*", "==", "!=", "<", ">", "<=", ">=",  "&&", "||", "/", "^", "%%");
            binary_ops_A = c("+", "-", "*", "==", "~=", "<", ">", "<=", ">=", "and", "or", "/", "^",  "%");
            binary_ops_b = c(     "%&%",     "%|%",      "%^%",       "%<<%",       "%>>%", "%//%", "%*%");
            binary_ops_B = c("bit.band", "bit.bor", "bit.bxor", "bit.lshift", "bit.rshift", "divi", "mulm");
            assign_ops = c("=", ":=", "<-", "<<-")
        }

        if (f == "{" && n == 0) {
            # {}, empty braces
            cpp = "{}";
        } else if (f == "{" && n > 0) {
            # {, group of calls
            cpp = paste0("\n", stringr::str_dup(tab, indent), "{\n",
                paste0(sapply(a, function(x) interpret_call(x, indent + 1, TRUE, FALSE, index1, mangle, mode)), collapse = "\n"),
                "\n", stringr::str_dup(tab, indent), "}");
        } else if (f == "(") {
            # (, in parentheses
            cpp = paste0(sapply(a, interp), collapse = ", ");
        } else if (f == "[") {
            # Check whether argument is missing
            if (n == 2 && identical(a[[2]], alist(m = )$m)) {
                i = "";
            } else {
                i = paste0(sapply(a[2:length(a)], interp), collapse = ", ");
            }
            cpp = stringr::str_glue(index1, x = interp(a[[1]]), i = i);
        } else if (f %in% unary_ops_a && n == 1) {
            cpp = paste0(unary_ops_A[match(f, unary_ops_a)], interp(a[[1]]));
        } else if (f %in% binary_ops_a && n == 2) {
            cpp = paste0("(", interp(a[[1]]), " ", binary_ops_A[match(f, binary_ops_a)], " ", interp(a[[2]]), ")");
        } else if (f %in% binary_ops_b && n == 2) {
            cpp = paste0(binary_ops_B[match(f, binary_ops_b)], "(", interp(a[[1]]), ", ", interp(a[[2]]), ")");
        } else if (f %in% assign_ops && n == 2) {
            cpp = paste0(interp(a[[1]]), " = ", interp(a[[2]]));
        } else if (grepl("^%[^%]+%$", f)) {
            cpp = paste0("special_", stringr::str_sub(f, 2, -2), "(", interp(a[[1]]), ", ", interp(a[[2]]), ")");
        } else if (f == "function") {
            cpp = "A FUNCTION!!!";
        } else if (f == "if" && n == 2) {
            cpp = paste0("if ", interp(a[[1]]), " ", interpret_call(a[[2]], indent, TRUE, TRUE, index1, mangle, mode));
        } else if (f == "if" && n == 3) {
            cpp = paste0("if ", interp(a[[1]]), " ", interpret_call(a[[2]], indent, TRUE, TRUE, index1, mangle, mode), "\n",
                stringr::str_dup(tab, indent), "else ", interpret_call(a[[3]], indent, TRUE, TRUE, index1, mangle, mode));
        } else if (f == "repeat") {
            cpp = paste0("while (true) ", interp(a[[1]], TRUE));
        } else if (f == "while") {
            cpp = paste0("while ", interp(a[[1]]), " ", interp(a[[2]], TRUE));
        } else if (f == "for") {
            cpp = paste0("for (auto ", interp(a[[1]]), " : ", interp(a[[2]]), ") ", interp(a[[3]], TRUE));
        } else if (f == ":") {
            cpp = paste0("seq(", interp(a[[1]]), ", ", interp(a[[2]]), ")");
        } else if (f == "next") {
            cpp = "continue";
        } else if (f == "break") {
            cpp = "break";
        } else {
            # Otherwise, assume true function call
            if (n == 0) {
                cpp = paste0(f, "()")
            } else {
                cpp = paste0(f, "(", paste(sapply(a, interp), collapse = ", "), ")")
            }
        }
    }

    if (statement && f != "{") {
        whitespace = ifelse(noindent, "", stringr::str_dup(tab, indent))
        if (f != "if") {
            return (paste0(whitespace, cpp, ";"))
        } else {
            return (paste0(whitespace, cpp))
        }
    } else {
        return (cpp)
    }
}

#' Translate R commands into C++.
#'
#' @export
r_to_cpp = function(x, index1 = "{x}.at({i})", mangle = "{x}")
{
    interpret_call(substitute(x), index1 = index1, mangle = mangle, mode = "C++");
}

#' Translate R commands into Lua.
#'
#' Currently this is quite hacky.
#'
#' @export
r_to_lua = function(x, index1 = "{x}[{i}]", mangle = "{x}")
{
    interpret_call(substitute(x), index1 = index1, mangle = mangle, mode = "Lua");
}

#' Translate an R function into C++.
#'
#' @export
func_to_cpp = function(f, name = as.character(substitute(f)), index1 = "{x}.at({i})", mangle = "{x}")
{
    r_args = names(formals(f));
    tree = as.list(f)[[length(r_args) + 1]];

    # Get correct level for tree (differentiate between curly-braced, non-curly-braced, and empty functions)
    if (class(tree) != "{") {
        # Non-curly-braced function: start at beginning
        tree = list(tree);
        i_0 = 1;
    } else if (length(tree) > 1) {
        # Curly-braced function: start at element after brace
        i_0 = 2;
    } else {
        # Empty function: return empty flow
        stop("Empty function.")
    }

    # Find and remove enforce statement
    enforce_line = 0;
    for (i in i_0:length(tree)) {
        code = tree[[i]];
        if (is.call(code) && as.character(code[[1]]) == "enforce") {
            if (enforce_line > 0) {
                stop("Multiple enforce statements.");
            }
            enforce_line = i;
            arg_names = names(code)[2:length(code)];
            arg_types = as.character(code[2:length(code)]);
        }
    }

    if (enforce_line == 0) {
        stop("No enforce statement found.")
    } else {
        tree[[enforce_line]] <- NULL;
    }

    # Create function
    r_types = c("numeric", "integer", "character", "logical");
    cpp_types = c("double", "int", "string", "bool");
    #cpp_types = c("std::vector<double>", "std::vector<int>", "std::vector<string>", "std::vector<bool>");
    if (!all(arg_types %in% r_types)) {
        stop("Some arg types not supported.");
    }
    cpp_args = paste(cpp_types[match(arg_types, r_types)], arg_names);
    signature = paste0("auto ", name, "(", paste0(cpp_args, collapse = ", "), ")");

    # Interpret rest of function
    if (i_0 == 1) {
        # If there is only one line in the function, and we have gotten to here,
        # then the only line is the enforce statement, so the function is empty.
        return (paste0(signature, " { }\n"))
    } else {
        return (paste0(signature, interpret_call(tree, index1 = index1, mangle = mangle, mode = "C++"), "\n"))
    }
}


#' Enforce types of arguments for a quicksilver function.
#'
#' @examples
#' example <- function(a, b, c = 0)
#' {
#'     enforce(a = numeric, b = numeric, c = numeric)
#'     return (a + b + c)
#' }
#'
#' @export
enforce = function(..., .env = parent.frame(), .coerce = TRUE, .silent = FALSE)
{
    args = list(...);
    calling_func = deparse(sys.calls()[[sys.nframe()-1]]);
    allowed_classes = c("numeric", "integer", "character", "logical");

    # Check for invalid arguments to this function
    parent_args = formals(sys.function(-1));
    for (i in seq_along(args)) {
        name = names(args)[[i]];
        if (!name %in% names(parent_args)) {
            stop("In ", calling_func, " : enforced argument \"", name, "\" does not exist.", call. = FALSE);
        }
        if (!is.function(args[[i]])) {
            stop("In ", calling_func, " : class of enforced argument \"", name, "\" not specified correctly.", call. = FALSE);
        }
        if (!class(args[[i]]()) %in% allowed_classes) {
            stop("In ", calling_func, " : class \"", class(args[[i]]()), "\" of enforced argument \"", name, "\" not supported.", call. = FALSE);
        }
    }

    # Check type / availability of each argument
    for (i in seq_along(args)) {
        name = names(args)[[i]];
        desired_class = class(args[[i]]());
        value = try(get(name, envir = .env), silent = TRUE);

        if (class(value) == "try-error") {
            # Argument is missing
            stop("In ", calling_func, " : argument \"", name, "\" is missing, with no default.", call. = FALSE);
        } else if (class(value) != desired_class) {
            # Argument is of wrong class
            if (.coerce == FALSE) {
                # Coercion not desired: stop
                stop("In ", calling_func, " : argument \"", name, "\" not of required class ", desired_class, ".", call. = FALSE);
            } else if (canCoerce(value, desired_class)) {
                # Coercion desired and possible: coerce
                assign(name, as(value, desired_class), envir = .env);
                if (!.silent) {
                    warning("In ", calling_func, " : argument \"", name, "\" coerced to ", desired_class, ".", call. = FALSE);
                }
            } else {
                # Coercion desired but not possible: stop
                stop("In ", calling_func, " : argument \"", name, "\" could not be coerced to ", desired_class, ".", call. = FALSE);
            }
        }
    }
}




#' @rdname expr_list-etc
#' @export
`[[.expr_list` = function(el, i)
{
    if (length(i) == 0) {
        stop("Invalid index for expr_list.")
    } else if (length(i) == 1) {
        # get expr_wrap or expr_alt
        if (class(unclass(el)[[i]]) == "expr_alt") {
            return (unclass(el)[[i]])
        } else {
            structure(list(unclass(el)[[i]]), class = "expr_wrap", into = attr(el, "into")[i])
        }
    } else {
        # get element
        unclass(el)[[i]]
    }
}

#' @rdname expr_list-etc
#' @export
`[[<-.expr_list` = function(el, i, value)
{
    if (length(i) == 0) {
        stop("Invalid index for expr_list.")
    } else if (length(i) == 1) {
        # replace element of list
        if (is.null(value)) {
            # value is NULL: omit element i
            return (el[-1])
        } else if (class(value) == "expr_alt") {
            l = unclass(el);
            l[[i]] = value;
            into = attr(el, "into");
            into[[i]] = NA;
            return (structure(l, class = "expr_list", into = into));
        } else if (class(value) == "expr_wrap") {
            l = unclass(el);
            l[[i]] = value[[1]];
            into = attr(el, "into");
            into[[i]] = attr(value, into);
            return (structure(l, class = "expr_list", into = into));
        } else {
            stop("Cannot assign ", class(value), " within expr_list.")
        }
    } else {
        if (class(value) == "expr_wrap") {
            el[[i[1]]][[i[-1]]] = value[[1]];
        } else {
            el[[i[1]]][[i[-1]]] = value;
        }
    }
}

#' @rdname expr_list-etc
#' @export
c.expr_list = function(el, ...)
{
    args = list(...);
    for (a in args) {
        if (class(a) == "expr_list") {
            el = structure(c(unclass(el), a), class = "expr_list", into = c(attr(el, "into"), attr(a, "into")));
        } else if (class(a) == "expr_alt") {
            el = structure(c(unclass(el), list(a)), class = "expr_list", into = c(attr(el, "into"), NA));
        } else if (class(a) == "expr_wrap") {
            el = structure(c(unclass(el), list(a)), class = "expr_list", into = c(attr(el, "into"), attr(a, "into")));
        } else {
            stop("Cannot concatenate class ", class(a), " to expr_list.")
        }
    }
    return (el)
}

#' Do things with expr_wrap
#'
#' @rdname expr_wrap-etc
#' @export
`[.expr_wrap` = function(ew, i)
{
    if (missing(i)) {
        return (unclass(ew)[[1]][])
    } else {
        return (unclass(ew)[[1]][i])
    }
}

#' Do things with expr_wrap
#'
#' @rdname expr_wrap-etc
#' @export
`[[.expr_wrap` = function(ew, i)
{
    if (length(i) == 0) {
        return (unclass(ew)[[1]])
    } else {
        return (unclass(ew)[[1]][[i]])
    }
}

#' @keywords internal
#' @export
print.expr_alt = function(x)
{
    if (length(x) != length(attr(x, "into"))) {
        warning("Corrupted expr_alt: number of alternatives not equal to length of 'into' attribute.")
    }

    fmt_expr = function(expr, tilde) {
        paste(if (tilde) '~{' else '{', format(expr), '}')
    }

    str = paste0(paste0(mapply(fmt_expr, x, attr(x, "into")), collapse = " ? "))

    cat("expr_alt:", str)
}

#' @keywords internal
#' @export
print.expr_wrap = function(x)
{
    if (length(x) != 1 || length(attr(x, "into")) != 1) {
        warning("Corrupted expr_wrap: length not equal to 1.")
    }

    fmt_expr = function(expr, tilde) {
        paste(if (tilde) '~{' else '{', format(expr), '}')
    }

    cat("expr_wrap: ", fmt_expr(x[[1]], attr(x, "into")[1]), sep = "")
}
