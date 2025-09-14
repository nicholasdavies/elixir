test_that("reindent works", {
    C_lines = c(
    "if /* check for {, ( */ (ch == '{' || ch == '(')",
    "{",
    "return 1;",
    "}")
    expect_identical(reindent(C_lines, "C"), c(
    "if /* check for {, ( */ (ch == '{' || ch == '(')",
    "{",
    "    return 1;",
    "}"))

    Cpp_lines = c(
    "class Dog",
    "{",
    "public:",
    "void Woof();",
    "private:",
    "std::string name;",
    "/*",
    "nothing here",
    "*/",
    "};")
    expect_identical(reindent(Cpp_lines, "C++"), c(
    "class Dog",
    "{",
    "public:",
    "    void Woof();",
    "private:",
    "    std::string name;",
    "    /*",
    "nothing here",
    "*/",
    "};"))

    Lua_lines = c(
    'concat = function(self, sep)',
    'sep = sep or ","',
    'local str = ""',
    'for i=1,#self do',
    'if sep ~= nil and i ~= #self then',
    'str = str .. tostring(self[i]) .. sep',
    'else',
    'str = str .. tostring(self[i])',
    'end',
    'end',
    'return str',
    'end'
    )
    expect_identical(reindent(Lua_lines, "Lua", tab = " ", start = 1L), c(
    ' concat = function(self, sep)',
    '  sep = sep or ","',
    '  local str = ""',
    '  for i=1,#self do',
    '   if sep ~= nil and i ~= #self then',
    '    str = str .. tostring(self[i]) .. sep',
    '   else',
    '    str = str .. tostring(self[i])',
    '   end',
    '  end',
    '  return str',
    ' end'))

    R_lines = c(
    'if ((is.list(x) || is.call(x)) && identical(x[[1]], quote(`{`))) {',
    '# If element is {-wrapped, treat as quotation; debrace and inject',
    'if (length(x) != 2) {',
    'stop("Contents of { } must be a single expression.")',
    '} else {\nreturn (rlang::inject(rlang::expr(!!x[[2]]), env))',
    '}\n}')

    expect_identical(reindent(R_lines, "R"), c(
    'if ((is.list(x) || is.call(x)) && identical(x[[1]], quote(`{`))) {',
    '    # If element is {-wrapped, treat as quotation; debrace and inject',
    '    if (length(x) != 2) {',
    '        stop(\"Contents of { } must be a single expression.\")',
    '    } else {',
    '        return (rlang::inject(rlang::expr(!!x[[2]]), env))',
    '    }',
    '}'))
})
