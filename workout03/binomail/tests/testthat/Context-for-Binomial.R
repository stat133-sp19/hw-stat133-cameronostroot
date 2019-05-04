context("Check Binomial Main Functions")

#bin_choose
test_that("bin_choose is a main function that works", {
  expect_error(bin_choose(n = 2, k = 3))
  expect_length(bin_choose(n = 5, k = 0), 1)
  expect_equal(bin_choose(n = 5, k = 2), 10)
})

#bin_probability
test_that("bin_probability is a main function that works", {
  expect_error(bin_probability(success = 2, trials = 3, prob = 3))
  expect_error(bin_probability(success = 2, trials = 1, prob = .5))
  expect_error(bin_probability(success = -1, trials = 1, prob = .5))
  expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5), 0.3125)
})

#bin_distribution
test_that("bin_distribution is a main function that works", {
  expect_error(bin_distribution(trials = 3, prob = 3))
  expect_error(bin_distribution(trials = -1, prob = .5))
  expect_is(bin_distribution(trials = 5, prob = 0.5), c("bindis", "data.frame"))
})

#bin_cumulative
test_that("bin_cumulative is a main function that works", {
  expect_error(bin_cumulative(trials = 3, prob = 3))
  expect_error(bin_cumulative(trials = -1, prob = .5))
  expect_is(bin_cumulative(trials = 5, prob = 0.5), c("bincum", "data.frame"))
})

#bin_variable
test_that("bin_cumulative is a main function that works", {
  expect_error(bin_variable(trials = 3, prob = 3))
  expect_error(bin_variable(trials = -1, prob = .5))
  expect_is(bin_variable(trials = 3, prob = .5), c("binvar"))
})

#bin_mean
test_that("bin_mean is a main function that works", {
  expect_error(bin_mean(trials = 3, prob = 3))
  expect_error(bin_mean(trials = -1, prob = .5))
  expect_equal(bin_mean(trials = 10, prob = 0.3), 3)
})

#bin_variance
test_that("bin_variance is a main function that works", {
  expect_error(bin_variance(trials = 3, prob = 3))
  expect_error(bin_variance(trials = -1, prob = .5))
  expect_equal(bin_variance(trials = 10, prob = 0.3), 2.1)
})

#bin_mode
test_that("bin_mode is a main function that works", {
  expect_error(bin_mode(trials = 3, prob = 3))
  expect_error(bin_mode(trials = -1, prob = .5))
  expect_equal(bin_mode(trials = 10, prob = 0.3), 3)
})

#bin_skewness
test_that("bin_skewness is a main function that works", {
  expect_error(bin_skewness(trials = 3, prob = 3))
  expect_error(bin_skewness(trials = -1, prob = .5))
  expect_length(bin_skewness(trials = 10, prob = 0.3), 1)
})

#bin_kurtosis
test_that("bin_kurtosis is a main function that works", {
  expect_error(bin_kurtosis(trials = 3, prob = 3))
  expect_error(bin_kurtosis(trials = -1, prob = .5))
  expect_length(bin_kurtosis(trials = 10, prob = 0.3), 1)
})

