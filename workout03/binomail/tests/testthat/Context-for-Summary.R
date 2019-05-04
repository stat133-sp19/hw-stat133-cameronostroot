context("Check Summary Measures")

#aux_mean
test_that("aux_mean is a summary measure function that works", {
  expect_equivalent(aux_mean(10,.4), 4)
  expect_length(aux_mean(10,.4), 1)
  expect_equal(aux_mean(10,.3), 3)
})

#aux_variance
test_that("aux_variance is a summary measure function that works", {
  expect_equivalent(aux_variance(10,.4), 2.4)
  expect_length(aux_variance(10,.4), 1)
  expect_equal(aux_variance(10,.3), 2.1)
})

#aux_mode
test_that("aux_mode is a summary measure function that works", {
  expect_equivalent(aux_mode(10,.4), 4)
  expect_length(aux_mode(10,.4), 1)
  expect_equal(aux_mode(10,.3), 3)
})

#aux_skewness
test_that("aux_skewness is a summary measure function that works", {
  expect_lt(aux_skewness(10,.4), .13)
  expect_length(aux_skewness(10,.4), 1)
  expect_gt(aux_skewness(10,.3), .27)
})

#aux_kurtosis
test_that("aux_kurtosis is a summary measure function that works", {
  expect_lt(aux_kurtosis(10,.4), -.18)
  expect_length(aux_kurtosis(10,.4), 1)
  expect_gt(aux_kurtosis(10,.3), -0.13)
})

