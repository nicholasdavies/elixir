test_that("match convenience wrappers work", {
    exprs = expr_list(
        { y = a*x^2 + b*x + c },
        { c^2 = a^2 + b^2 },
        { x1 = (-b + sqrt(b^2 - 4*a*c)) / (2*a) }
    )

    expect_identical(expr_count(exprs, { .A * .B }), c(1L, 0L, 2L))

    expect_identical(expr_detect(exprs, { .A * .B }), c(TRUE, FALSE, TRUE))

    expect_identical(expr_extract(exprs, { .A * .B }),
        list(list(quote(b * x)), NULL, list(quote(4 * a), quote(2 * a))))
    expect_identical(expr_extract(exprs, { .A * .B }, "match"),
        list(list(quote(b * x)), NULL, list(quote(4 * a), quote(2 * a))))
    expect_identical(expr_extract(exprs, { .A * .B }, "A"),
        list(list(quote(b)), NULL, list(quote(4), quote(2))))
    expect_identical(expr_extract(exprs, { .A * .B }, ".A", dotnames = TRUE),
        list(list(quote(b)), NULL, list(quote(4), quote(2))))
    expect_identical(expr_locate(exprs, { .A * .B }),
        list(list(c(1L, 3L, 2L, 3L)), NULL, list(c(3L, 3L, 2L, 2L, 3L, 2L, 3L, 2L), c(3L, 3L, 3L, 2L))))

    expect_identical(expr_extract(exprs, { .A * .B }, "match", gather = TRUE),
        list(quote(b * x), quote(4 * a), quote(2 * a)))
    expect_identical(expr_extract(exprs, { .A * .B }, "A", gather = TRUE),
        list(quote(b), quote(4), quote(2)))
    expect_identical(expr_locate(exprs, { .A * .B }, gather = TRUE),
        list(c(1L, 3L, 2L, 3L), c(3L, 3L, 2L, 2L, 3L, 2L, 3L, 2L), c(3L, 3L, 3L, 2L)))
})
