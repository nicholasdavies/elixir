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
})
