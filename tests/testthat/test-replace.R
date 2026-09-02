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

test_that("expr_replace works with function replacements", {
    # Simple function replacement
    expect_identical(
        expr_replace({ x^2 }, ~{ ..A ^ .N }, function(m) substitute(pow(A, N), m)),
        quote(pow(x, 2)))

    # Conditional function replacement
    expect_identical(
        expr_replace({ x^2 + y^2 + z^3 }, { ..A ^ .N },
            function(m) if (m$N == 2) substitute(square(A), m) else substitute(A^N, m)),
        quote(square(x) + square(y) + z^3))

    # Function replacement with access to full match object
    expect_identical(
        expr_replace({ x + y }, ~{ .A + .B },
            function(m) {
                if (identical(m$A, quote(x))) substitute(B - A, m) else m$match
            }),
        quote(y - x))

    # Mixing function and expression replacements in ...
    expect_identical(
        expr_replace({ x^2 + y },
            { ..A ^ .N }, function(m) substitute(pow(A, N), m),
            { y }, { z }),
        quote(pow(x, 2) + z))

    # Function replacement on expr_list input
    exprs = expr_list({x^2}, {y^3}, {z^2})
    expect_identical(
        expr_replace(exprs, ~{ ..A ^ .N },
            function(m) if (m$N == 2) substitute(square(A), m) else substitute(pow(A, N), m)),
        expr_list({square(x)}, {pow(y, 3)}, {square(z)}))
})
