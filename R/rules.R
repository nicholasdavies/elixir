# Simple helper to assemble a list with the needed components for rules$ops
# as an argument to translate.
op = function(arity, precedence, associativity, str, nopar = integer(0))
{
    if (is.null(arity)) {
        NULL
    } else {
        list(arity = arity, prec = precedence, assoc = associativity,
            str = str, nopar = nopar)
    }
}

# Note: kmod is
# double kmod(double x, double y)
# {
#     double r = fmod(x, y);
#     return r && r < 0 != y < 0 ? r + y : r;
# }
# i.e. knuth's mod operator

# Built-in rules
ruleset = list(
    "C" = list(
        indent_more = c("{", "("),
        indent_less = c("}", ")"),
        indent_both = c("case", "default:"),
        comment = c("/*", "*/"),
        ignore = list(
            c("//"),
            c("/*", "*/"),
            c('"', '"', '\\"', '\\\\"', '\\\\\\"', '\\\\\\\\"'),
            c("'", "'", "\\'", "\\\\'")
        ),

        ops = list(
            "["   = op(2,  2, "LTR", "{A[1]}[{A[2]}]", 2),
            "+"   = op(1,  3, "RTL", "+{A[1]}"),
            "-"   = op(1,  3, "RTL", "-{A[1]}"),
            "!"   = op(1,  3, "RTL", "!{A[1]}"),
            "*"   = op(2,  5, "LTR", "{A[1]} * {A[2]}"),
            "/"   = op(2,  5, "LTR", "{A[1]} / {A[2]}"), # TODO This needs to force double division: in R, / ALWAYS returns numeric
            "%%"  = op(2,  5, "LTR", "kmod({A[1]}, {A[2]})"), # TODO In R, `%%` type depends on type of operands
            "%/%" = op(2,  5, "LTR", "floor({A[1]} / {A[2]})"), # TODO In R, `%/%` type depends on type of operands
            "+"   = op(2,  6, "LTR", "{A[1]} + {A[2]}"),
            "-"   = op(2,  6, "LTR", "{A[1]} - {A[2]}"),
            "<"   = op(2,  9, "LTR", "{A[1]} < {A[2]}"),
            "<="  = op(2,  9, "LTR", "{A[1]} <= {A[2]}"),
            ">"   = op(2,  9, "LTR", "{A[1]} > {A[2]}"),
            ">="  = op(2,  9, "LTR", "{A[1]} >= {A[2]}"),
            "=="  = op(2, 10, "LTR", "{A[1]} == {A[2]}"),
            "!="  = op(2, 10, "LTR", "{A[1]} != {A[2]}"),
            "&&"  = op(2, 14, "LTR", "{A[1]} && {A[2]}"),
            "||"  = op(2, 15, "LTR", "{A[1]} || {A[2]}"),
            "^"   = op(2, 98, "---", "pow({A[1]}, {A[2]})"),
            "<-"  = op(2, 99, "RTL", "{A[1]} = {A[2]}"),
            "="   = op(2, 99, "RTL", "{A[1]} = {A[2]}")
        ),
        paren = "({x})",
        literal = list(
            "NULL" = function(x) "R_NilValue",
            numeric = function(x) if (is.na(x)) "NA_REAL" else if (!x %like% "\\.") paste0(x, ".") else format(x, digits = 17), # TODO locale
            integer = function(x) if (is.na(x)) "NA_INTEGER" else as.character(x),
            logical = function(x) if (is.na(x)) "NA_LOGICAL" else if (x) "true" else "false",
            character = function(x) if (is.na(x)) "NA_CHARACTER" else paste0('"', stringi::stri_escape_unicode(x), '"')
        ),
        symbol = as.character # TODO not enough; need to exclude non-C viable names e.g. with dots
    ),

    "C++" = list(
        indent_more = c("{", "("),
        indent_less = c("}", ")"),
        indent_both = c("public:", "private:", "protected:", "case", "default:"),
        comment = "//",
        ignore = list(
            c("//"),
            c("/*", "*/"),
            c('"', '"', '\\"', '\\\\"', '\\\\\\"', '\\\\\\\\"'),
            c("'", "'", "\\'", "\\\\'")
        ),

        ops = list(
            "["   = op(2,  2, "LTR", "{A[1]}[{A[2]}]", 2),
            "+"   = op(1,  3, "RTL", "+{A[1]}"),
            "-"   = op(1,  3, "RTL", "-{A[1]}"),
            "!"   = op(1,  3, "RTL", "!{A[1]}"),
            "*"   = op(2,  5, "LTR", "{A[1]} * {A[2]}"),
            "/"   = op(2,  5, "LTR", "{A[1]} / {A[2]}"), # TODO This needs to force double division: in R, / ALWAYS returns numeric
            "%%"  = op(2,  5, "LTR", "kmod({A[1]}, {A[2]})"), # TODO In R, `%%` type depends on type of operands
            "%/%" = op(2,  5, "LTR", "floor({A[1]} / {A[2]})"), # TODO In R, `%/%` type depends on type of operands
            "+"   = op(2,  6, "LTR", "{A[1]} + {A[2]}"),
            "-"   = op(2,  6, "LTR", "{A[1]} - {A[2]}"),
            "<"   = op(2,  9, "LTR", "{A[1]} < {A[2]}"),
            "<="  = op(2,  9, "LTR", "{A[1]} <= {A[2]}"),
            ">"   = op(2,  9, "LTR", "{A[1]} > {A[2]}"),
            ">="  = op(2,  9, "LTR", "{A[1]} >= {A[2]}"),
            "=="  = op(2, 10, "LTR", "{A[1]} == {A[2]}"),
            "!="  = op(2, 10, "LTR", "{A[1]} != {A[2]}"),
            "&&"  = op(2, 14, "LTR", "{A[1]} && {A[2]}"),
            "||"  = op(2, 15, "LTR", "{A[1]} || {A[2]}"),
            "^"   = op(2, 98, "---", "pow({A[1]}, {A[2]})"),
            "<-"  = op(2, 99, "RTL", "{A[1]} = {A[2]}"),
            "="   = op(2, 99, "RTL", "{A[1]} = {A[2]}")
        ),
        paren = "({x})",
        literal = list(
            "NULL" = function(x) "R_NilValue",
            numeric = function(x) if (is.na(x)) "NA_REAL" else if (!x %like% "\\.") paste0(x, ".") else format(x, digits = 17), # TODO locale
            integer = function(x) if (is.na(x)) "NA_INTEGER" else as.character(x),
            logical = function(x) if (is.na(x)) "NA_LOGICAL" else if (x) "true" else "false",
            character = function(x) if (is.na(x)) "NA_CHARACTER" else paste0('"', stringi::stri_escape_unicode(x), '"')
        ),
        symbol = as.character # TODO not enough; need to exclude non-C++ viable names e.g. with dots
    ),

    "Lua" = list(
        indent_more = c("{", "(", "function", "if", "do", "repeat"), # for and while covered by 'do'
        indent_less = c("}", ")", "end", "until"),
        indent_both = c("else", "elseif"),
        comment = "--",
        ignore = list(
            c("--"),
            c("--[[", "]]"),
            c("--[=[", "]=]"),
            c("--[==[", "]==]"),
            c('"', '"', '\\"', '\\\\"', '\\\\\\"', '\\\\\\\\"'),
            c("'", "'", "\\'", "\\\\'", "\\\\\\'", "\\\\\\\\'")
        ),

        ops = list(
            "["   = op(2,  0, "LTR", "{A[1]}[{A[2]}]", 2),
            "^"   = op(2,  1, "RTL", "{A[1]} ^ {A[2]}"),
            "!"   = op(1,  2, "LTR", "not {A[1]}"),
            "-"   = op(1,  2, "LTR", "-{A[1]}"),
            "+"   = op(1,  2, "LTR", "{A[1]}"),
            "*"   = op(2,  3, "LTR", "{A[1]} * {A[2]}"),
            "/"   = op(2,  3, "LTR", "{A[1]} / {A[2]}"),
            "%%"  = op(2,  3, "LTR", "kmod({A[1]}, {A[2]})"), # TODO In R, `%%` type depends on type of operands
            "+"   = op(2,  4, "LTR", "{A[1]} + {A[2]}"),
            "-"   = op(2,  4, "LTR", "{A[1]} - {A[2]}"),
            "<"   = op(2,  6, "LTR", "{A[1]} < {A[2]}"),
            ">"   = op(2,  6, "LTR", "{A[1]} > {A[2]}"),
            "<="  = op(2,  6, "LTR", "{A[1]} <= {A[2]}"),
            ">="  = op(2,  6, "LTR", "{A[1]} >= {A[2]}"),
            "!="  = op(2,  6, "LTR", "{A[1]} ~= {A[2]}"),
            "=="  = op(2,  6, "LTR", "{A[1]} == {A[2]}"),
            "&&"  = op(2,  7, "LTR", "{A[1]} and {A[2]}"),
            "||"  = op(2,  8, "LTR", "{A[1]} or {A[2]}"),
            "%/%" = op(2, 98, "LTR", "math.floor({A[1]} / {A[2]})"), # TODO In R, `%/%` type depends on type of operands
            "="   = op(2, 99, "RTL", "{A[1]} = {A[2]}"), # TODO You can't chain assignments in Lua. This needs
            "<-"  = op(2, 99, "RTL", "{A[1]} = {A[2]}")  # to be handled specially therefore.
        ),
        paren = "({x})",
        literal = list(
            "NULL" = function(x) "luajr.NULL",
            numeric = function(x) if (is.na(x)) "luajr.NA_real_" else format(x, digits = 17), # TODO locale
            integer = function(x) if (is.na(x)) "luajr.NA_integer_" else as.character(x),
            logical = function(x) if (is.na(x)) "luajr.NA_logical_" else if (x) "true" else "false",
            character = function(x) if (is.na(x)) "luajr.NA_character_" else paste0('"', stringi::stri_escape_unicode(x), '"')
        ),
        symbol = as.character # TODO not enough; need to exclude non-Lua viable names e.g. with dots
    ),

    "R" = list(
        indent_more = c("{", "("),
        indent_less = c("}", ")"),
        indent_both = character(0),
        comment = "#",
        ignore = list(
            c("#"),
            c('"', '"', '\\"', '\\\\"', '\\\\\\"', '\\\\\\\\"'),
            c("'", "'", "\\'", "\\\\'", "\\\\\\'", "\\\\\\\\'"),
            c("`", "`", "\\`", "\\\\`", "\\\\\\`", "\\\\\\\\`")
        )
    )
)

# Attempt to find ruleset named `rules` if rules is character string, otherwise
# check that the list `rules` contains the required elements as specified in
# `...`, lists, e.g.:
# name = list("list", "length(rule) > 0", "names(rule[[i]]) == c("chr", "esc")")
check_rules = function(rules, ...)
{
    if (is.character(rules) && length(rules) == 1) {
        index = match(rules, names(ruleset));
        if (is.na(index)) {
            stop('No matching rule set "', rules, '". Use one of ',
                paste0('"', names(ruleset), '"', collapse = ", "));
        }
        rules = ruleset[[index]]
    }

    # Also check built-in rulesets...
    if (is.list(rules)) {
        spec = list(...);
        fails = character(0);
        for (s in seq_along(spec)) {
            nm = names(spec)[[s]]; # name of required element
            cl = spec[[s]][[1]];   # class(es) of required element
            rule = rules[[nm]];

            # Required: rule exists and has correct class
            pass = !is.null(rule) && any(cl %in% class(rule));

            # 1st level predicate must be true when applied to rule
            if (length(spec[[s]]) >= 2 && !is.na(spec[[s]][[2]])) {
                pred1 = str2lang(spec[[s]][[2]]);
                pass = pass && eval(pred1);
            }

            # 2st level predicate must be true when applied to each element of rule
            if (length(spec[[s]]) >= 3) {
                pred2 = str2lang(spec[[s]][[3]]);
                for (i in seq_along(rule)) {
                    pass = pass && eval(pred2);
                }
            }

            if (!pass) {
                fails = c(fails,
                    paste0("Must supply a rule \"", nm, "\" of type ",
                        paste0(spec[[s]][[1]], collapse = " OR "),
                        ifelse(length(spec[[s]]) >= 2, paste0(" with ", paste0(na.omit(unlist(spec[[s]][-1])), collapse = ", ")), "")));
            }
        }

        if (length(fails) > 0) {
            stop(paste(fails, collapse = "\n"))
        }
        return (rules)
    } else {
        stop("Rules must be character or list.")
    }
}

#' Rules for understanding languages
#'
#' Several Elixir functions -- namely those starting with `expr_` -- take an
#' argument `rules` which assists those functions in interpreting their
#' arguments.
#'
#' In all cases, `rules` can either be a character string identifying a set of
#' built-in rules for a specific language or purpose -- currently, Elixir
#' accepts `"C"`, `"C++"`, `"Lua"`, or `"R"` -- or a list with elements
#' required for interpretation.
#'
#' `elixir:::ruleset` contains the built-in rules. Passing an empty `list()` as
#' the `rules` argument to an Elixir function will cause it to complain about
#' the missing components, which is one way of discerning what is needed for
#' a given function, but usually these error messages do not quite cover all
#' details of what is needed.
#'
#' @name rules
NULL
