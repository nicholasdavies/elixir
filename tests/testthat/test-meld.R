test_that("meld works", {
    names = c("a", "b");
    more_names = letters[24:26];

    expect_identical(meld(c(
        '/***R',
        'values = list(1, quote(a + 1));',
        '*/',
        'auto `names` = `values`;',
        'while (false) {',
        'auto `names[0]`;',
        '}',
        '`!^"else"` if (`more_names` `$"="``!$"!"`= `1:3`) {`^" // start of an if else block"`\\\\',
        'cout << "`more_names`" << "\\n";\\\\',
        '}'
    ),
    rules = "C++", reindent = TRUE),
'auto a = 1;
auto b = a + 1;
while (false) {
    // auto `names[0]`; [skipped]
}
if (x != 1) { // start of an if else block
    cout << "x" << "\\n";
}
else if (y != 2) {
    cout << "y" << "\\n";
}
else if (z == 3) {
    cout << "z" << "\\n";
}')

    expect_warning(meld(c("malformed \\ ", "backslash")), "terminal backslash plus whitespace")
    expect_identical(meld(c("OK \\", "backslash")), "OK backslash")
    expect_error(meld("`letters` `month.abb`"), "must all have the same length")
    expect_error(meld(file = test_path("meld.c"), ipath = test_path()), "Cannot find")
    expect_error(meld(file = test_path("meld.cpp")), "Unterminated R block")
    expect_identical(meld(file = test_path("meld.lua"), ipath = test_path()), "\n-- `list()` [skipped]")
    expect_identical(meld(file = test_path("meld.R")), "")
    expect_identical(meld('    `"one\\ntwo"`'), "    one\n    two")
    expect_identical(meld("/***R\nlist()\n*/", rules = NA), "")
    expect_identical(meld("`list()`", rules = NA), "")
})
