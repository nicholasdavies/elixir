# Helper to shift logical vectors left/right
lshift = function(x, n)
{
    if (n >= length(x)) {
        rep(FALSE, length(x))
    } else {
        c(x[(1 + n):length(x)], rep(FALSE, n))
    }
}

rshift = function(x, n)
{
    if (n >= length(x)) {
        rep(FALSE, length(x))
    } else {
        c(rep(FALSE, n), x[1:(length(x) - n)])
    }
}

# Replacement for data.table::`%like%`
`%like%` = function(vector, pattern)
{
    grepl(pattern, vector)
}
