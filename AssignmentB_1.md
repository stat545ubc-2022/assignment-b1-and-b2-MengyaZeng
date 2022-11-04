Assignment B-1: Making a function
================

# Setup

Load data and needed package:

``` r
library(datateachr) # <- might contain the data you picked!
library(tidyverse)
library(arsenal)
library(broom)
library(testthat)
```

# Exercise 1: Make a Function (25 points)

Here I create a function that retrieves the standard error of a linear
model object fitted for two numeric vectors.

``` r
#' @description
#' This function will return the standard error of the fitted linear model for vector x, y. Since we are fitting a linear model, value that's not numeric will not be accepted, also we would expect the length of these two vectors to match. The input vector can have NA as long as we have enough numeric elements to fit our model.
#'
#' @title lm_std
#' @param x - corresponds to is the "X" vector that we want to fit our linear regression model for, 
#' @param y - corresponds to is the "Y" vector that we want to fit our linear regression model for
#' @param dataset - the dataset that contains the x, y variable, this dataset variable is set as an empty vector by default if the user wishes to input two vector x, y directly.
#' @return return the standard error of the fitted linear model for vector x, y.
#'
lm_std <- function( x, y, dataset=c() ) {
    stopifnot(is.numeric(x) && is.numeric(y))
    stopifnot(length(x) == length(y))
    my_lm <- lm(y ~ x, data = dataset)
    tidy_lm <- tidy(my_lm)
    tidy_lm$std.error[2]# 2 is for extracting the standard error row corresponding to our x variable
}
```

# Exercise 2: Document your Function (20 points)

In the same code chunk where you made your function, document the
function using roxygen2 tags.

Documentation added in code chunk accordingly.

# Exercise 3: Include examples (15 points)

Demonstrate the usage of your function with a few examples. Use one or
more new code chunks, describing what you’re doing.  

## Example 1:

Here I want to use this function to fit a linear model between variable
*perimeter_mean* and *radius_mean* in dataset *cancer_sample*.

``` r
cancer_lm <- lm(perimeter_mean ~ radius_mean, data = cancer_sample)
sum_cancer <- summary(cancer_lm)
sum_cancer$coefficients
```

    ##              Estimate Std. Error   t value    Pr(>|t|)
    ## (Intercept) -5.232389 0.27597302 -18.95978 1.87499e-62
    ## radius_mean  6.880400 0.01895491 362.98772 0.00000e+00

So I would expect the function to return 0.01895491.  

``` r
print(lm_std(cancer_sample$radius_mean, cancer_sample$perimeter_mean, cancer_sample))
```

    ## [1] 0.01895491

Which means this function works.

## Example 2:

Now let’s try using two numeric vectors.  

``` r
x1 <- c(1,2,3)
y1 <- c(4,5,7)
lm_1 <- lm(y1~x1)
summary(lm_1)$coefficients
```

    ##             Estimate Std. Error  t value  Pr(>|t|)
    ## (Intercept) 2.333333  0.6236096 3.741657 0.1662580
    ## x1          1.500000  0.2886751 5.196152 0.1210377

``` r
print(lm_std(x1,y1))
```

    ## [1] 0.2886751

Which means this function works for two vector input as well.

## Example 3:

Now let’s try using two vectors with different length, we should expect
an error of length not matching in this one.

``` r
x2 <- c(1,2,3,4)
y2 <- c(4,5,7)
print(lm_std(x2, y2))
```

    ## Error in lm_std(x2, y2): length(x) == length(y) is not TRUE

Which is as expected.

# Exercise 4: Test the Function (25 points)

Write formal tests for your function. You should use at least three
non-redundant uses of an expect\_() function from the testthat package,
and they should be contained in a test_that() function (or more than
one). They should all pass.

## Test1:

Get input vectors from dataset with no NA’s  

``` r
test1_lm <- lm(area_mean ~perimeter_mean, cancer_sample)
result1 <- tidy(test1_lm)$std.error[2]

test_that("Test 1:", {
  expect_equal(result1, lm_std(cancer_sample$perimeter_mean ,cancer_sample$area_mean, cancer_sample))
})
```

    ## Test passed 😸

## Test2:

Directly input numeric vectors with matching length and no NA’s  

``` r
test2_lm <- lm(c(2,4,6) ~c(5,7,10))
result2 <- tidy(test2_lm)$std.error[2]

test_that("Test 2:", {
  expect_equal(result2, lm_std(c(5,7,10), c(2,4,6)))
})
```

    ## Test passed 🌈

## Test3:

Directly input numeric vectors with matching length that has NA.

``` r
test3_lm <- lm(c(2,4,6,8) ~c(5,7,10,NA))
result3 <- tidy(test2_lm)$std.error[2]

test_that("Test 3:", {
  expect_equal(result3, lm_std(c(5,7,10,NA),c(2,4,6,8)))
})
```

    ## Test passed 🌈

## Test4:

Directly input numeric vectors with matching length that has special
character.  

``` r
test_that("Test 3:", {
  expect_error(lm_std(c(5,7,10), c(2,4,"s")), 'is.numeric')
})
```

    ## Test passed 😸

## Test5:

Directly input numeric vectors with different length.

``` r
test_that("Test 3:", {
  expect_error(lm_std(c(5,7,10), c(2,4)))
})
```

    ## Test passed 😸
