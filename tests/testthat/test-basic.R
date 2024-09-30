test_that("do_parse_simple works", {
    # NULL
    b = NULL
    expect_identical(do_parse_simple(NULL),  structure(list(NULL), class = "expr_wrap", into = NA))
    expect_identical(do_parse_simple(b),     structure(list(NULL), class = "expr_wrap", into = NA))

    # "Expression literals"
    expect_identical(do_parse_simple({ .A }), structure(list(quote(.A)), class = "expr_wrap", into = NA))

    # Literals
    expect_identical(do_parse_simple(NA), structure(list(NA), class = "expr_wrap", into = NA))

    # With expressions stored in variables
    x = quote(letters[1])
    expect_identical(do_parse_simple(x), structure(list(quote(letters[1])), class = "expr_wrap", into = NA))

    # With z a list of expressions
    z = list(quote(x), quote(y))
    expect_identical(do_parse_simple(z), list(quote(x), quote(y)))

    # With z an expr_list
    z = expr_list({x}, {y})
    expect_identical(do_parse_simple(z), structure(list(quote(x), quote(y)), class = "expr_list", into = c(TRUE, TRUE)))

    # Names
    z = list(a = quote(x), b = quote(y))
    expect_identical(names(do_parse_simple(z)), c("a", "b"))
})

test_that("expr_list works", {
    # This is also a test of the expressions syntax

    # NULL
    b = NULL
    expect_identical(expr_list(NULL),  structure(list(NULL), class = "expr_list", into = TRUE))
    expect_identical(expr_list(~NULL), structure(list(NULL), class = "expr_list", into = FALSE))
    expect_identical(expr_list(b),     structure(list(NULL), class = "expr_list", into = TRUE))
    expect_identical(expr_list(~b),    structure(list(NULL), class = "expr_list", into = FALSE))

    # "Expression literals"
    expect_identical(expr_list({ .A }),  structure(list(quote(.A)), class = "expr_list", into = TRUE))
    expect_identical(expr_list(~{ .A }), structure(list(quote(.A)), class = "expr_list", into = FALSE))
    expect_identical(expr_list({ .A } ? ~{ .B }),
        structure(list(
            structure(list(quote(.A), quote(.B)), class = "expr_alt", into = c(TRUE, FALSE))),
        class = "expr_list", into = NA))

    # Literals
    expect_identical(expr_list(2),  structure(list(2), class = "expr_list", into = TRUE))
    expect_identical(expr_list(~2), structure(list(2), class = "expr_list", into = FALSE))
    expect_identical(expr_list(~2 ? 4),
        structure(list(
            structure(list(2, 4), class = "expr_alt", into = c(FALSE, TRUE))),
        class = "expr_list", into = NA))

    # With expressions stored in variables
    x = quote(letters[1])
    y = quote(letters[2*2])
    expect_identical(expr_list(x),  structure(list(quote(letters[1])), class = "expr_list", into = TRUE))
    expect_identical(expr_list(~x), structure(list(quote(letters[1])), class = "expr_list", into = FALSE))
    expect_identical(expr_list(x ? y),
        structure(list(
            structure(list(quote(letters[1]), quote(letters[2*2])), class = "expr_alt", into = c(TRUE, TRUE))),
        class = "expr_list", into = NA))

    # With z a list of expressions
    z = list(quote(x), quote(y))
    expect_identical(expr_list(z),   structure(list(quote(x), quote(y)), class = "expr_list", into = c(TRUE, TRUE)))
    expect_identical(expr_list(~z),  structure(list(quote(x), quote(y)), class = "expr_list", into = c(FALSE, FALSE)))
    expect_identical(expr_list(?z),
        structure(list(
            structure(list(quote(x), quote(y)), class = "expr_alt", into = c(TRUE, TRUE))),
        class = "expr_list", into = NA))
    expect_identical(expr_list(?~z),
        structure(list(
            structure(list(quote(x), quote(y)), class = "expr_alt", into = c(FALSE, FALSE))),
        class = "expr_list", into = NA))

    # Names
    expect_identical(names(expr_list(a = {.A}, b = {.B}, expr_list(y = 1, yy = 2))), c("a", "b", "y", "yy"))
    expect_identical(names(expr_list(a = {.A}, b = {.B}, ?expr_list(y = 1, yy = 2))), c("a", "b", ""))
    expect_identical(names(expr_list(a = {.A}, b = {.B}, q = ?expr_list(y = 1, yy = 2))), c("a", "b", "q"))
    expect_identical(names(expr_list(a = {.A}, b = {.B}, q = expr_list(y = 1, yy = 2))), c("a", "b", "q.y", "q.yy"))
    expect_identical(names(expr_list(a = {.A}, b = {.B}, q = expr_list(1, 2))), c("a", "b", "q1", "q2"))

    # Empty list
    expect_identical(expr_list(), structure(list(), class = "expr_list", into = logical(0)))

    # Interestingly, the below fails if the definition of val is put directly
    # into the call to expect_identical. Not clear why.
    val = expr_list({a}, {b}, {!!c}, env = list(c = 3))
    expect_identical(val, structure(list(quote(a), quote(b), 3), class = "expr_list", into = c(TRUE, TRUE, TRUE)))
})

test_that("expr_sub works", {
    expr = expr_list({1 + 2 + 3 + 4})
    expect_identical(expr_sub(expr, NULL), expr_list({1 + 2 + 3 + 4}))
    expect_identical(expr_sub(expr, 1), quote(1 + 2 + 3 + 4))
    expect_identical(expr_sub(expr, c(1,2)), quote(1 + 2 + 3))
    expect_identical(expr_sub(expr, c(1,2,2)), quote(1 + 2))
    expect_identical(expr_sub(expr, c(1,2,2,2)), quote(1))
})
