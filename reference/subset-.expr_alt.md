# Assign to part of an `expr_alt`.

This exists primarily so that `expr_apply` can be applied to an
`expr_list`, which may potentially contain elements of class `expr_alt`.

## Usage

``` r
# S3 method for class 'expr_alt'
xl[i] <- value
```

## Value

The modified object of class `"expr_alt"`.
