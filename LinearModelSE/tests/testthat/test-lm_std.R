test1_lm <- lm(area_mean ~perimeter_mean, datateachr::cancer_sample)
result1 <- broom::tidy(test1_lm)$std.error[2]

test_that("Test 1:", {
  expect_equal(result1, lm_std(datateachr::cancer_sample$perimeter_mean ,datateachr::cancer_sample$area_mean, datateachr::cancer_sample))
})

test2_lm <- lm(c(2,4,6) ~c(5,7,10))
result2 <- broom::tidy(test2_lm)$std.error[2]

test_that("Test 2:", {
  expect_equal(result2, lm_std(c(5,7,10), c(2,4,6)))
})

test3_lm <- lm(c(2,4,6,8) ~c(5,7,10,NA))
result3 <- broom::tidy(test2_lm)$std.error[2]

test_that("Test 3:", {
  expect_equal(result3, lm_std(c(5,7,10,NA),c(2,4,6,8)))
})

test_that("Test 4:", {
  expect_error(lm_std(c(5,7,9,11), c(2,4,6,"s")), 'is.numeric')
})

test_that("Test 5:", {
  expect_error(lm_std(c(5,7,9,11,13), c(2,4,5,8)))
})

# clean up objects
rm(list = ls())
