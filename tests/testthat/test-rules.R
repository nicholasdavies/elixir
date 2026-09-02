test_that("op works", {
    expect_identical(op(NULL), NULL)
    expect_identical(op(2, 10, "LTR", "{A[1]} + {A[2]}"),
        list(arity = 2, prec = 10, assoc = "LTR", str = "{A[1]} + {A[2]}", nopar = integer(0)))
})

test_that("check_rules works", {
    expect_error(check_rules("Java"))
    expect_error(check_rules(42))
    expect_identical(check_rules("C++",
            comment = list(c("character", "logical"), 'length(rule) %in% 1:2'),
            indent_more = list("character"),
            indent_less = list("character"),
            indent_both = list("character"),
            ignore = list("list", NA, 'is.character(rule[[i]]) && length(rule[[i]]) >= 1')
        ), ruleset[["C++"]])
    expect_error(check_rules(list(something = 1L),
        something = list("character")))
    expect_error(check_rules(list(something = 1L),
        something = list("integer", "rule == 2L")))
})
