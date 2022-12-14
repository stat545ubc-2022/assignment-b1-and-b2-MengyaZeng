
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LinearModelSE

<!-- badges: start -->
<!-- badges: end -->

The goal of LinearModelSE is to take in two numeric vectors x and y,
either manually or get two vectors from a dataset, and return the
standard error of the linear model fitted for vector x, y. Since it’s
for fitting a linear model, the length of x and y should be the same.

## Installation

You can install the development version of LinearModelSE like so:

``` r
# install.packages("devtools")
devtools::install_github("stat545ubc-2022/assignment-b1-and-b2-MengyaZeng/LinearModelSE")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(LinearModelSE)
## you can either directly input two numeric vectors 
lm_std(c(1,2,3), c(4,5,7))
#> [1] 0.2886751
## or get two vectors from a dataset
lm_std(datateachr::cancer_sample$radius_mean, datateachr::cancer_sample$perimeter_mean)
#> [1] 0.01895491
```
