#' Code generation from template file
#'
#' @description
#' `meld` reads a specially-formatted file from filename `file` or
#' as lines of text passed via unnamed arguments and returns these lines of
#' text after performing substitutions of R code.
#'
#' This function is experimental.
#'
#' @details
#' As `meld` works through each line of the text, any blocks of text starting
#' with the delimiter `/***R` and ending with `*/` are run as R code.
#'
#' Outside these blocks, any substrings in the text delimited by
#' `` `backticks` `` are interpreted as R expressions to be substituted into
#' the line. If any of the backticked expressions are length 0, the line is
#' commented out (with the message "\[skipped\]" appended) using the `comment`
#' element of `rules`. If any of the backticked expressions are length L > 1,
#' the entire interpreted line is repeated L times, separated by newlines and
#' with elements of the expression in sequence.
#'
#' There are some special sequences:
#' - `` `^expr` `` subs in `expr` only on the first line of a multi-line expansion
#' - `` `!^expr` `` subs in `expr` on all but the first line of a multi-line expansion
#' - `` `$expr` ``subs in `expr` only on the last line of a multi-line expansion
#' - `` `!$expr` `` subs in `expr` on all but the last line of a multi-line expansion
#' - `` `#include file` `` interprets `file` as an R expression resolving to
#'   a filename, runs that file through `meld`, and pastes in the result
#'
#' The `#include` command must appear by itself on a line, and searches for
#' files in the path `ipath`.
#'
#' The function tries to guess `rules` from the file extension if that is
#' possible. If the file extension is .c, then `"C"` is guessed; for .h, .hpp,
#' or .cpp, `"C++"` is guessed; for .R, `"R"` is guessed; for .lua, `"Lua"` is
#' guessed. Case is ignored for file extensions.
#'
#' R blocks are evaluated immediately prior to the next-occurring backticked
#' line, so variables modified in an R block are available to any backticked
#' expression following the R block. Any remaining R blocks are run after
#' remaining lines are interpreted.
#'
#' If any line from the text ends with a single backslash \code{\\}, the next
#' line is concatenated to it. If any line from the text ends with a double
#' backslash \code{\\\\}, the next line is concatenated to it with a newline as
#' a separator. This allows backticked expressions to apply over multiple
#' lines.
#'
#' @param ... Lines to be interpreted as the text. If there are any embedded
#'     newlines in a line, the line is split into multiple lines.
#' @param file File to be read in as the text.
#' @param rules Which [rules][elixir-rules] to follow. You can pass a string
#' from among `"C"`, `"C++"`, `"Lua"`, or `"R"`, or a list with elements:
#' * `comment` Character vector for comments (used when backticked lines are
#' skipped); either NA for no comments, one string for end-of-line comments or
#' two strings for delimited comments.
#' * `indent_more` Character vector of tokens which increase the indent level.
#' * `indent_less` Character vector of tokens which decrease the indent level.
#' * `indent_both` Character vector of tokens which decrease, then increase the
#' indent level (see [reindent()]).
#' * `ignore` Comment and string literal delimiters (see [reindent()]).
#'
#' If `NULL`, the default, either guess rules from the file extension, or if
#' that is not possible, do not put in 'skipped' comments and do not reindent
#' the result. `NA` to not try to guess.
#' @param reindent If `TRUE`, the default, reindent according to `rules`. If
#' `FALSE`, do not reindent.
#' @param ipath Path to search for `#include`d files
#' @param env Environment in which to evaluate R expressions. The default is
#' `rlang::env_clone(parent.frame())`, and it is best to clone the environment
#' so that new declarations do not pollute the environment in question.
#' @return The interpreted text as a single character string.
#' @examples
#' meld(
#'     "/***R",
#'     "names = c('a', 'b', 'c');",
#'     "dontdothis = NULL;",
#'     "*/",
#'     "double foo()",
#'     "{",
#'     "    double `names` = `1:3`;",
#'     "    double `dontdothis` = this_doesnt_matter;",
#'     "    return `paste(names, sep = ' + ')`;",
#'     "}")
#' @export
meld = function(..., file = NULL, rules = NULL, reindent = TRUE, ipath = ".", env = rlang::env_clone(parent.frame()))
{
    # Get rules
    if (is.null(rules)) {
        rules = NA;
        if (!is.null(file)) {
            ext = stringr::str_to_lower(stringr::str_extract(file, "\\.([[:alnum:]]+)$", 1));
            if (!is.na(ext)) {
                if (ext == "c") {
                    rules = "C";
                } else if (ext %in% c("h", "cpp", "hpp")) {
                    rules = "C++";
                } else if (ext == "r") {
                    rules = "R";
                } else if (ext == "lua") {
                    rules = "Lua";
                }
            }
        }
    }

    if (!all(is.na(rules))) {
        rules = check_rules(rules,
            comment = list(c("character", "logical"), 'length(rule) %in% 1:2'),
            indent_more = list("character"),
            indent_less = list("character"),
            indent_both = list("character"),
            ignore = list("list", NA, 'is.character(rule[[i]]) && length(rule[[i]]) >= 1')
        )
    } else {
        reindent = FALSE;
        rules = list(comment = NA);
    }

    # Load lines
    if (!is.null(file)) {
        lines = readLines(file, warn = FALSE)
        filename = file;
    } else {
        lines = as.character(c(...));
        lines = unlist(lapply(lines, stringr::str_split_1, "\n"));
        filename = "... argument";
    }

    # Put multi-line expressions into one line
    # 0. Warn about potentially malformed multi-lines
    ml_malform = which(lines %like% "\\\\\\s+$")
    if (length(ml_malform) > 0) {
        warning("On line(s) ", paste(ml_malform, collapse = ", "),
            ", terminal backslash plus whitespace. Not interpreted as a multi-line expression.")
    }

    # 1. Find all lines ending with one or two backslashes
    ml_any = which(lines %like% "\\\\$");
    ml_double = which(lines %like% "\\\\\\\\$");
    ml_single = setdiff(ml_any, ml_double);

    # 2. Concatenate lines
    for (l in rev(ml_any))
    {
        if (l %in% ml_single) {
            lines[l] = paste0(stringr::str_sub(lines[l], 1L, -2L), lines[l + 1]);
        } else {
            lines[l] = paste0(stringr::str_sub(lines[l], 1L, -3L), "\n", lines[l + 1]);
        }
        lines = lines[-(l + 1)];
    }

    # Extract R blocks to run
    # 1. Find all pairs of /***R and */ -- the logic here is different from
    # above since we do not want to capture all */ lines as ends of R blocks,
    # only the first one after each /***R.
    R_block_open = which(lines %like% "^\\s*/\\*\\*\\*R\\s*$");
    R_block_close = integer(length(R_block_open));
    for (bl in seq_along(R_block_open)) {
        R_block_close[bl] = which(lines %like% "^\\s*\\*/\\s*$" & seq_along(lines) > R_block_open[bl])[1];
        if (is.na(R_block_close[bl]) || (bl < length(R_block_open) && R_block_close[bl] >= R_block_open[bl + 1])) {
            stop("Unterminated R block found. Ensure R block ends with */ on its own line.")
        }
    }

    # 2. Set aside each R block to run later, replacing with NAs in lines
    R_blocks = list();
    for (rb in seq_along(R_block_open))
    {
        rb_lines = rlang::seq2(R_block_open[rb] + 1, R_block_close[rb] - 1);
        R_blocks[[rb]] = lines[rb_lines];
        lines[R_block_open[rb]:R_block_close[rb]] = NA_character_;
    }
    # Note we are keeping those NAs in lines[] for now.

    # Replace lines containing backticks, running R code blocks along the way
    for (l in which(lines %like% "`"))
    {
        # Execute any R blocks prior to this line
        blocks = which(R_block_open <= l);
        for (b in blocks) {
            eval(parse(text = R_blocks[[b]]), envir = env);
        }
        if (length(blocks) > 0) {
            R_blocks = R_blocks[-blocks];
            R_block_open = R_block_open[-blocks];
        }

        # Execute #include command if present
        match_include2 = regmatches(lines[l],
            regexec("^\\s*`\\s*#\\s*include\\s+([^`]+)`\\s*$", lines[l]));
        if (length(match_include2[[1]]) == 2) {
            filename = eval(str2lang(match_include2[[1]][2]), envir = env);
            inc_file = file.path(ipath, filename);
            if (!file.exists(inc_file)) {
                stop("Cannot find #included filename ", inc_file, ".");
            }
            lines[l] = meld(file = inc_file, rules = rules, reindent = reindent, ipath = ipath, env = env);
            next;
        }

        # Find backtick-escaped expressions within the line
        m = gregexpr("`[^`]*`", lines[l]);

        # Separate line into expression (sE) and literal (sL) components;
        # sE are the components in backticks, sL are the rest
        sE = regmatches(lines[l], m)[[1]];
        sE = substring(sE, 2, nchar(sE) - 1);
        sL = regmatches(lines[l], m, invert = TRUE)[[1]];

        # Expression components within backticks can optionally start with
        # special codes which alter their interpretation, called flavours
        fl_first_line = sE %like% "^\\^";
        fl_not_first_line = sE %like% "^!\\^";
        fl_last_line = sE %like% "^\\$";
        fl_not_last_line = sE %like% "^!\\$";

        # Remove flavours
        sE = stringr::str_remove(sE, "^!?[$^]");

        # Evaluate expressions
        sE = lapply(sE, function(x) if (nchar(x) > 0) eval(str2lang(x), envir = env) else NULL);

        # If any expressions have length 0, skip this line
        if (any(lengths(sE) == 0)) {
            spaces = as.integer(regexpr("[^ ]", lines[l]) - 1);
            preview = substring(lines[l], spaces + 1, nchar(lines[l]));
            spaces = substring(lines[l], 1, spaces);
            preview = stringr::str_replace(preview, "\n(?s).*$", "...");
            if (!is.na(rules$comment[1])) {
                lines[l] = paste0(spaces, rules$comment[1], " ", preview, " [skipped]", if (!is.na(rules$comment[2])) paste0(" ", rules$comment[2]) else "");
            } else {
                lines[l] = NA;
            }
            next;
        }

        # Make sure all expressions are either length 1 or length max(length)
        longest = max(lengths(sE));
        if (any(!lengths(sE) %in% c(1, longest))) {
            stop("Backticked expressions on the same line must all have the same length or length 1.\n",
                "(i.e., meld does not auto-recycle backticked expressions to the longest element length.)\n",
                "Error occurred on line ", l, " of ", filename, ":\n",
                lines[l], "\n",
                "Lengths of backticked expressions: ", paste(lengths(sE), collapse = ", "))
        }

        # Do recycling
        sE = lapply(sE, rep_len, length.out = longest);

        # Apply flavours
        sE[fl_first_line]     = lapply(sE[fl_first_line],     function(x) ifelse(seq_along(x) == 1, x, ""));
        sE[fl_not_first_line] = lapply(sE[fl_not_first_line], function(x) ifelse(seq_along(x) != 1, x, ""));
        sE[fl_last_line]      = lapply(sE[fl_last_line],      function(x) ifelse(seq_along(x) == length(x), x, ""));
        sE[fl_not_last_line]  = lapply(sE[fl_not_last_line],  function(x) ifelse(seq_along(x) != length(x), x, ""));

        # Special case: if this line contains only spaces/tabs and one backticked expression evaluating to character
        # with newlines within, indent non-first lines within character according to spaces preceding backtick.
        # This is so lines like this
        #     `lines_of_code`
        # include the spacing before `lines_of_code` for each line, even if
        # `lines_of_code` is just a length-1 string (with embedded newlines).
        if (length(sE) == 1 && sL[1] %like% "^[ \t]+$" && is.character(sE[[1]]) && any(sE[[1]] %like% "\n"))
        {
            sE[[1]] = stringr::str_replace_all(sE[[1]], "\n", paste0("\n", sL[1]));
        }

        # Interleave expression and literal components
        sI = c(as.list(sL), sE)[order(c(2 * seq_along(sL) - 1, 2 * seq_along(sE)))];

        # Replace line
        lines[l] = do.call(paste0, c(sI, collapse = "\n"));
    }

    # Execute any remaining R blocks, i.e. if they appear at the end of the file
    for (b in seq_along(R_blocks)) {
        eval(parse(text = R_blocks[[b]]), envir = env);
    }
    # Remove any NA lines
    lines = lines[!is.na(lines)];

    # Reindent
    if (reindent) {
        lines = reindent(lines, rules = rules);
    }

    return (paste0(lines, collapse = "\n"))
}
