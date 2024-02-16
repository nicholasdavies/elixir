test_that("expr_match works at top level", {
    M1 = function(match, ...) {
        structure(list(
            c(list(match = substitute(match), loc = NULL), list(...))
        ), class = "expr_match")
    }

    # Calls with captured arg list
    expect_identical(expr_match( { f(x) }, { .f(...a) } ), M1(f(x), f = quote(f), a = list(quote(x))))
    expect_identical(expr_match( { f() }, { .a(...a) } ), M1(f(), a = quote(f), a = list()))
    expect_identical(expr_match( { f(x=3,y,z) }, { .a(...b) } ), M1(f(x=3, y, z), a = quote(f), b = list(x = 3, quote(y), quote(z))))

    expect_identical(expr_match( { f(x) }, { `.f/g`(...a) } ), NULL)
    expect_identical(expr_match( { f(x) }, { `.f/f`(...a) } ), M1(f(x), f = quote(f), a = list(quote(x))))

    expect_identical(expr_match( { f(x) }, { f(...a) } ), M1(f(x), a = list(quote(x))))
    expect_identical(expr_match( { f() }, { f(...a) } ), M1(f(), a = list()))
    expect_identical(expr_match( { f(x,y,z) }, { f(...a) } ), M1(f(x,y,z), a = list(quote(x), quote(y), quote(z))))

    expect_identical(expr_match( { f(x) }, { q(...a) } ), NULL)

    # Multi-part pattern
    expect_identical(expr_match( { a + b }, { .A + 9 } ), NULL)
    expect_identical(expr_match( { a + b }, { .A + .B } ), M1(a + b, A = quote(a), B = quote(b)))
    expect_identical(expr_match( { a + b }, { .A + .B + .C } ), NULL)

    expect_identical(expr_match( { a + b + c }, { .A + ..B } ), NULL)
    expect_identical(expr_match( { a + b + c }, { ..A + .B } ), M1(a + b + c, A = quote(a + b), B = quote(c)))
    expect_identical(expr_match( { a + (b + c) }, { .A + ..B } ), M1(a + (b + c), A = quote(a), B = quote((b + c))))

    # Single-part pattern
    expect_identical(expr_match( { a }, { ..A } ), M1(a, A = quote(a)))
    expect_identical(expr_match( { a + b }, { ..A } ), M1(a + b, A = quote(a + b)))
    expect_identical(expr_match( { a + b + c }, { ..B } ), M1(a + b + c, B = quote(a + b + c)))

    expect_identical(expr_match( { a }, { .A } ), M1(a, A = quote(a)))
    expect_identical(expr_match( { a() }, { .A } ), NULL)
    expect_identical(expr_match( { a }, { .A() } ), NULL)
    expect_identical(expr_match( { a() }, { .A() } ), M1(a(), A = quote(a)))

    expect_identical(expr_match( { 2 }, { 2 } ), M1(2))
    expect_identical(expr_match( { 2 }, { 2.1 } ), NULL)
    expect_identical(expr_match( { 2L }, { 2.0 } ), NULL)

    expect_null(expr_match( { a + b }, { .A } ))
    expect_null(expr_match( { a + b }, { ...A } ))

    # More complex token matches
    expect_null(expr_match( { a }, { `.A/b` } ))
    expect_null(expr_match( { a() }, { `.A/b` } ))
    expect_null(expr_match( { a }, { `.A/b`() } ))
    expect_null(expr_match( { a() }, { `.A/b`() } ))

    expect_identical(expr_match( { 2 }, { `.A:numeric` } ), M1(2, A = 2))
    expect_identical(expr_match( { 2 }, { `.A:integer` } ), NULL)
    expect_identical(expr_match( { 2L }, { `.A:numeric` } ), NULL)
    expect_identical(expr_match( { 2L }, { `.A:integer` } ), M1(2L, A = 2L))

    expect_identical(expr_match( { 1+1 }, { `.A:call` } ), NULL)

    expect_identical(expr_match( { 2 }, { `.A|A > 1` } ), M1(2, A = 2))
    expect_identical(expr_match( { 2 }, { `.A|A > 2` } ), NULL)
    expect_identical(expr_match( { 2 }, { `.A:numeric|A > 1` } ), M1(2, A = 2))
    expect_identical(expr_match( { 2 }, { `.A:numeric|A > 2` } ), NULL)
    expect_identical(expr_match( { 2 }, { `.A:integer|A > 1` } ), NULL)
    expect_identical(expr_match( { 2 }, { `.A:integer|A > 2` } ), NULL)
    expect_identical(expr_match( { "hi" }, { `.A:character|A == "hi"` } ), M1("hi", A = "hi"))
})

test_that("expr_match works into and alternatively", {
    # Picking up all relevant tokens
    expect_identical(expr_match({ E = m * c^2 }, ~{ `.A:name/[[:alpha:]]` }),
        structure(list(list(match = quote(E), loc = 2L, A = quote(E)),
            list(match = quote(m), loc = 3:2, A = quote(m)),
            list(match = quote(c), loc = c(3L, 3L, 2L), A = quote(c))), class = "expr_match"))
    expect_identical(expr_match({ E = m * c^2 }, ~{ `.A:numeric` }),
        structure(list(list(match = 2, loc = c(3L, 3L, 3L), A = 2)), class = "expr_match"))
    expect_identical(expr_match({ a + b + c }, ~{ ..A + ..B }),
        structure(list(list(match = quote(a + b + c), loc = NULL, A = quote(a + b), B = quote(c)),
            list(match = quote(a + b), loc = 2L, A = quote(a), B = quote(b))), class = "expr_match"))

    # A very funky match, testing what is available in "tests" within token matches
    expect_identical(expr_match({0 + (1 + 2 + 3 + 4 + 5 + 6)},
        ~{ `.A|A%%2==1` + `.B|B==A+A` + `.C|A==B/2` + `.D|D==2*B` + `.E|match==quote(1+2+3+4+5+6)` + `.F|E==5 && identical(loc, c(3L,2L))` }),
        structure(list(list(match = quote(1 + 2 + 3 + 4 + 5 + 6), loc = 3:2,
            A = 1, B = 2, C = 3, D = 4, E = 5, F = 6)), class = "expr_match"))

    # Alternatives
    expect_identical(expr_match({ 1 + 2 * 3 }, ~{ `/` } ? ~{ `*` } ? ~{ `+` }),
        structure(list(list(alt = 2L, match = quote(`*`), loc = c(3L, 1L))), class = "expr_match"))
    expect_identical(expr_match({ 1 + 2 * 3 }, ~{ `*` } ? ~{ `+` } ? ~{ `/` }),
        structure(list(list(alt = 1L, match = quote(`*`), loc = c(3L, 1L))), class = "expr_match"))
    expect_identical(expr_match({ 1 + 2 * 3 }, ~{ `+` } ? ~{ `/` } ? ~{ `*` }),
        structure(list(list(alt = 1L, match = quote(`+`), loc = 1L)), class = "expr_match"))
})
