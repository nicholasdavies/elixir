test_that("helper functions work", {
    x = c(TRUE, TRUE)
    expect_identical(lshift(x, 0), c(TRUE, TRUE))
    expect_identical(lshift(x, 1), c(TRUE, FALSE))
    expect_identical(lshift(x, 2), c(FALSE, FALSE))
    expect_identical(lshift(x, 3), c(FALSE, FALSE))
    expect_identical(rshift(x, 0), c(TRUE, TRUE))
    expect_identical(rshift(x, 1), c(FALSE, TRUE))
    expect_identical(rshift(x, 2), c(FALSE, FALSE))
    expect_identical(rshift(x, 3), c(FALSE, FALSE))

    expect_identical("ham" %like% "[a-z]m$", TRUE)
})
