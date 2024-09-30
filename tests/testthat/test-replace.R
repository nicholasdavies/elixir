test_that("expr_replace works", {
    # Basics
    expect_identical(expr_replace({ a },     ~{ a }, { b }, n = 1), quote(b))
    expect_identical(expr_replace({ a + x }, ~{ a }, { b }, n = 1), quote(a + x))
    expect_identical(expr_replace({ a + x },  { a }, { b }, n = 1), quote(b + x))
    expect_identical(expr_replace({ a + x }, ~{ .a }, { b }, n = 1), quote(a + x))
    expect_identical(expr_replace({ a + x },  { .a }, { b }, n = 1), quote(b(a, x)))
    expect_identical(expr_replace({ a + x },  { .a }, { b }), quote(b(b, b)))
    expect_identical(expr_replace({ a + x }, ~{ .X + .Y }, { b }, n = 1), quote(b))
    expect_identical(expr_replace({ a + x }, ~{ .X + .Y }, { .Y + 2*.X^.X }, n = 1), quote(x + 2*a^a))

    # Replacing in an expr_list; replacing alternatives
    exprs = expr_list(quote(a + x), quote(a + y))
    expect_identical(expr_replace(exprs, {a}, {b}, {x}, {y}), expr_list({b + y}, {b + y}))
    expect_identical(expr_replace(exprs, {a}, {b}, {x}, {y}, n = 1), expr_list({b + y}, {b + y}))
    expect_identical(expr_replace(exprs, {a} ? {x}, {b} ? {y}, n = 1), expr_list({b + x}, {b + y}))
    expect_identical(expr_replace(exprs, {a} ? {x}, {b} ? {y}), expr_list({b + y}, {b + y}))

    # References to temporary match entries
    expect_identical(expr_replace({ E = m * c^2 },
        { ._^2 }, { ._^3 },
        { `.A:name|A == "m"` }, { M },
        { `=` }, { `<-` },
        { `E` }, { `ee` }), quote(ee <- M * c^3))
})
