#' Reindent some lines of code
#'
#' @description
#' Using some fairly unsophisticated metrics, [reindent()] will take some lines
#' of code and, according to its understanding of the rules for that language,
#' reindent those lines. This is intended to help prettify automatically
#' generated code.
#'
#' This function is experimental.
#'
#' @details
#' Conceptually, the function first ignores any comments or string literals.
#' Then, line by line, `reindent` looks for tokens that signal either an
#' increase in the indent level, a decrease in the indent level, or both at
#' the same time. For example, in this Lua code:
#'
#' ```
#' if x == 1 then
#'     print 'one'
#' else
#'     print 'not one'
#' end
#' ```
#'
#' the `else` keyword both decreases and increases the indent level.
#'
#' [reindent()] supports "raw strings" in R, C, C++, and Lua code but only in
#' limited cases. In R, when using [raw character constants][Quotes] you must
#' use an uppercase `R`, the double quote symbol and zero to two hyphens. In
#' C/C++, when using
#' [raw string literals](https://en.cppreference.com/w/cpp/language/string_literal.html)
#' you must use the prefix `R`, and zero to two hyphens as the delimiter char
#' sequence (plus parentheses). In Lua, you can use
#' [long brackets](https://www.lua.org/manual/5.2/manual.html#3.1) with zero
#' to two equals signs. Any other attempt to use raw strings will probably
#' break [reindent()].
#'
#' Other unusual character sequences may also break [reindent()].
#'
#' @param lines Character vector with lines of text; can have internal
#'     newlines.
#' @param rules Which [rules][elixir-rules] to follow. You can pass a string
#'     from among `"C"`, `"C++"`, `"Lua"`, or `"R"`, or a list with elements:
#' * `indent_more` Character vector of tokens which increase the indent level.
#' * `indent_less` Character vector of tokens which decrease the indent level.
#' * `indent_both` Character vector of tokens which decrease, then increase the
#' indent level (see Details).
#' * `ignore` Comment and string literal delimiters (see Details).
#' @param tab Character string; what to use as an indent.
#' @param start Indent level to start at.
#' @return Reindented lines as a character vector.
#' @examples
#' reindent(
#'     c(
#'         "if x == 1 then",
#'         "print 'one'",
#'         "else",
#'         "print 'not one'",
#'         "end"
#'     ),
#'     rules = "Lua")
#' @export
reindent = function(lines, rules, tab = "    ", start = 0L)
{
    # Get rules
    rules = check_rules(rules,
        indent_more = list("character"),
        indent_less = list("character"),
        indent_both = list("character"),
        ignore = list("list", NA, 'is.character(rule[[i]]) && length(rule[[i]]) >= 1')
    )

    # Remove any internal newlines from lines
    lines = unlist(stringr::str_split(lines, "\n"));

    # Process rule components from character vectors into lists
    tokenize = function(x) stringr::str_split(x, stringr::boundary("word", skip_word_none = FALSE));
    indent_more = tokenize(rules$indent_more);
    indent_less = tokenize(rules$indent_less);
    indent_both = tokenize(rules$indent_both);
    ignore = lapply(rules$ignore, tokenize);

    # Calculate indent level for each line
    indent_level = as.integer(start);
    indent_levels = integer(length(lines));
    in_ignore = 0;

    for (l in seq_along(lines)) {
        # Split line into tokens on word boundary
        tokens = tokenize(lines[l])[[1]];

        if (length(tokens) > 0) {
            # If we are in an ignore, look for end ignore symbol
            if (in_ignore > 0) {
                # Look for sought-for end token
                locations_end = locate_end_ignore(tokens, ignore[[in_ignore]]);
                if (any(locations_end)) {
                    # Closing symbol in this line: cut out and do not indent this line
                    cut_end = which(locations_end)[1];
                    tokens = tokens[-(1:cut_end)];
                    in_ignore = 0;
                    indent_levels[l] = NA;
                    next;
                } else {
                    # If not here, preserve indent level from original and process next line
                    indent_levels[l] = NA;
                    next;
                }
            }

            # Cut out ignores
            while (length(tokens) > 0) {
                # Find starting positions of all ignore types
                # Need to use lapply because we need to know which element of ignore
                # occurs where, rather than just getting "any" element of ignore's position
                locations = lapply(ignore, function(ign) locate(tokens, ign[1]));

                # Select earliest-occurring ignore type; if tie, longest opening ignore token among these,
                # since that would have to be how the different ignore types work (otherwise the underlying
                # language couldn't support multiple ignore types with the same starting token(s))
                earliest = sapply(locations, function(x) which(x)[1]);
                if (all(is.na(earliest))) break;
                earliest = which(earliest == min(earliest, na.rm = TRUE));
                itype = earliest[which.max(sapply(ignore[earliest], function(x) length(x[[1]])))];
                ign = ignore[[itype]];

                # Now remove ignored tokens.
                ilevel = cumsum(locations[[itype]]);
                if (length(ign) == 1) {
                    # If ignore is to end of line, cut out and break from loop
                    tokens = tokens[ilevel == 0];
                    break;
                } else {
                    # If comment is to closing symbol, try to find in this line
                    locations_end = ilevel != 0 & locate_end_ignore(tokens, ign);
                    if (any(locations_end)) {
                        # Closing symbol in this line: cut out and keep looking for ignores
                        cut_start = which(locations[[itype]])[1];
                        cut_end = which(locations_end & seq_along(locations_end) > cut_start)[1];
                        if (is.na(cut_end)) {
                            cut_end = length(tokens);
                        }
                        tokens[cut_start:cut_end] = "";
                        # We replace the cut-out tokens with "" rather than removing them
                        # so that removing an ignore from a line doesn't create new
                        # "valid" token sequences.
                        next;
                    } else {
                        # No closing symbol in this line: cut out to end of line and carry on
                        in_ignore = itype;
                        tokens = tokens[ilevel == 0];
                        break;
                    }
                }
            }
        }

        # Do indenting. "itrace" records the indent changes, token by token.
        itrace = integer(length(tokens));
        itrace[locate(tokens, indent_more)] = +1L;
        itrace[locate(tokens, indent_less)] = -1L;

        # "both" indenters need to be marked as c(-1, +1)
        itrace = as.list(itrace);
        itrace[locate(tokens, indent_both)] = list(c(-1L, +1L));
        itrace = unlist(itrace);

        # Now get running indent level within line, and set line's overall indent level using it
        itrace = cumsum(itrace);
        indent_levels[l] = indent_level + min(c(0L, itrace));
        indent_level = indent_level + tail(c(0L, itrace), 1L);
    }

    # Actually do indentation.
    # Generate leading whitespace
    tabs = stringr::str_dup(tab, indent_levels);
    # Attach to all lines
    ind_lines = stringr::str_c(tabs, stringr::str_trim(lines, "left"));
    # Use indented lines only when non-NA level
    lines = ifelse(is.na(indent_levels),
        lines,
        ind_lines
    );

    return (lines)
}

# Find elements of 'what' -- a list -- sequentially in tokens
locate = function(tokens, what)
{
    pos = rep(FALSE, length(tokens));
    for (k in what) {
        start = rep(TRUE, length(tokens));
        for (j in seq_along(k)) {
            start = start & data.table::shift(tokens == k[j], n = 1L - j, fill = FALSE)
        }
        pos = pos | start;
    }
    return (pos);
}

# Special version of locate for end of specific ignore sequence
locate_end_ignore = function(tokens, ign)
{
    loc = locate(tokens, ign[2]);
    if (length(ign) == 3) {
        neg = list(c(ign[[3]], ign[[2]]))
        pos = list(c(ign[[3]], neg[[1]]))
        while (TRUE) {
            ln = locate(tokens, neg)
            if (any(ln)) {
                lp = locate(tokens, pos)
                loc_neg = data.table::shift(ln, n = length(neg) - 1L, fill = FALSE);
                loc_pos = data.table::shift(lp, n = length(pos) - 1L, fill = FALSE);
                loc = loc & !(loc_neg & !loc_pos);
                neg = list(c(ign[[3]], pos[[1]]))
                pos = list(c(ign[[3]], neg[[1]]))
            } else {
                break;
            }
        }
    }
    return (loc)
}
