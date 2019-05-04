context("Check Checker Functions")

#check_prob
test_that("check_prob is a checker function that works", {
  expect_true(check_prob(0.5))
  expect_length(check_prob(1), 1)
  expect_error(check_prob(-1))
})



#check_trials
test_that("check_trials is a checker function that works", {
  expect_true(check_trials(2))
  expect_length(check_trials(1), 1)
  expect_error(check_trials(-1))
})


#check_success
test_that("check_trials is a checker function that works", {
  expect_true(check_success(2, 3))
  expect_true(check_success(3, 3))
  expect_true(check_success(2, 3))
  expect_true(check_success(1, 3))
  expect_error(check_success(-1, 0))
  expect_error(check_success(2, 1))
})

