# `elixir`: Transmutation of Languages

<!-- badges: start -->

[![R-CMD-check](https://github.com/nicholasdavies/elixir/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nicholasdavies/elixir/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/nicholasdavies/elixir/graph/badge.svg)](https://app.codecov.io/gh/nicholasdavies/elixir)
<!-- badges: end -->

Tools for transforming R expressions. 

Provides functions for finding, extracting, and replacing patterns in R'
language objects, similarly to how regular expressions can be used to find, 
extract, and replace patterns in text. Also provides functions for generating 
code using specially-formatted template files and for translating R 
expressions into similar expressions in other programming languages. The package 
may be helpful for advanced uses of R expressions, such as developing 
domain-specific languages.

## Installation

You can install the released version of `elixir` from
[CRAN](https://CRAN.R-project.org/package=elixir) with:

``` r
install.packages("elixir")
```

You can install the development version of `elixir` from
[GitHub](https://github.com/nicholasdavies/elixir) with:

``` r
# install.packages("devtools")
devtools::install_github("nicholasdavies/elixir")
```
