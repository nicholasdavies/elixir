test_that("expr_apply works", {
    # expr_apply should work for expressions, lists, and expr_lists.
    # Works with all types for x:
    x1 = quote(y <- a*x + b)
    x2 = list(quote(a), quote(b + c), quote(d + e + f))
    x3 = expr_list({to + be} ? {!(to + be)}, { f(x) })

    expect_identical(expr_apply(x1, function(x) as.name(stringr::str_to_upper(as.character(x))), into = "leaves"),
        quote(Y <- A*X + B))
    expect_identical(expr_apply(x2, function(x) as.name(stringr::str_to_upper(as.character(x))), into = "leaves"),
        list(quote(A), quote(B + C), quote(D + E + F)))
    expect_identical(expr_apply(x3, function(x) as.name(stringr::str_to_upper(as.character(x))), into = "leaves"),
        expr_list({TO + BE} ? {!(TO + BE)}, { F(X) }))

    # Depth limiting works
    expect_identical(expr_apply(x1, function(x) as.name(stringr::str_to_upper(as.character(x))), depth = 0, into = "leaves"),
        quote(Y <- A*X + B))
    expect_identical(expr_apply(x2, function(x) as.name(stringr::str_to_upper(as.character(x))), depth = 0, into = "leaves"),
        list(quote(a), quote(b + c), quote(d + e + f)))
    expect_identical(expr_apply(x3, function(x) as.name(stringr::str_to_upper(as.character(x))), depth = 1, into = "leaves"),
        expr_list({to + be} ? {!(to + be)}, { F(X) }))

    # Different modes of `into`, `order`.
    string = ""
    expr_apply({a + b}, function(x) { string <<- paste(string, paste0(all.names(x), collapse = "")); x }, into = TRUE, order = "post")
    expect_identical(string, " + a b +ab")

    string = ""
    expr_apply({a + b}, function(x) { string <<- paste(string, paste0(all.names(x), collapse = "")); x }, into = TRUE, order = "pre")
    expect_identical(string, " +ab + a b")

    string = ""
    expr_apply({a + b}, function(x) { string <<- paste(string, paste0(all.names(x), collapse = "")); x }, into = FALSE)
    expect_identical(string, " +ab")

    string = ""
    expr_apply({a + b}, function(x) { string <<- paste(string, paste0(all.names(x), collapse = "")); x }, into = "leaves")
    expect_identical(string, " + a b")

    # Different modes of `how`.
    x = list(quote(a), list(quote(b + c), list(quote(d + e))))
    expect_identical(expr_apply(x, function(x) { if (all(as.character(x) %in% letters)) quote(.) else x }, into = TRUE, how = "replace"),
        list(quote(.), list(quote(. + .), list(quote(. + .)))))
    expect_identical(expr_apply(x, function(x) { if (all(as.character(x) %in% letters)) quote(.) else x }, into = TRUE, how = "unlist"),
        list(quote(.), quote(. + .), quote(. + .)))
    expect_identical(expr_apply(x, function(x) { if (all(as.character(x) %in% letters)) quote(.) else x }, into = TRUE, how = "unique"),
        list(quote(.), quote(. + .)))

    # Use of second and third params of x.
    expect_identical(expr_apply(x,
        function(x, name, idx) { if (is.null(name[[1]])) quote(x) else quote(y) },
        into = "leaves", order = "post", how = "unlist"),
        list(quote(y), quote(x(x, x)), quote(x(x, x))))

    expect_identical(expr_apply(x, function(x, name, idx) { idx }, into = FALSE),
        list(1L, list(c(2L,1L), list(c(2L,2L,1L)))))
})
