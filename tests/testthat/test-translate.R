test_that("translate works", {
    exprs = expr_list(
        { (1 / 3) },
        { a != n }
    )

    expect_identical(translate(exprs, "C"),   list("(1. / 3.)", "a != n"))
    expect_identical(translate(exprs, "Lua"), list("(1 / 3)",   "a ~= n"))
    expect_identical(translate({1/3}, "C++"), "1. / 3.")
    expect_identical(translate({f(NULL)}, "Lua"), "f(luajr.NULL)")
    expect_error(translate({ `1`(0) }, "C"))
    expect_error(translate({ f(a = 0, b = 1) }, "C"))
    expect_error(translate({ 1 + !!as.raw("123") }, "C"))
})

test_that("lang2str works", {
    # NULL/NA cases
    expect_identical(lang2str(NULL), "NULL")
    expect_identical(lang2str(NA), "NA")

    # Easy cases
    expect_identical(lang2str(1:1), "1L")
    expect_identical(lang2str(2.0), "2")

    # Trickier cases
    expect_identical(lang2str("hi"), '"hi"')
    expect_error(lang2str(list(a = 1,2,3)))
    expect_error(lang2str(1:3))
    expect_error(lang2str(c(1,2,3)))
    expect_identical(lang2str(quote(stroopwaffel)), "stroopwaffel")
})


