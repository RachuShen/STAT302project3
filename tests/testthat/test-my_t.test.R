# within test-my_t.test.R
sample <- rnorm(20, mean = 0, sd = 1)

test_that("unacceptable `alternative` throws error", {
  expect_error(my_t.test(x, alternative = "abc", mu = 0))
})

test_that("non-numeric input throws warning and errors", {
  expect_error(my_t.test("x", alternative = "two.sided", mu = 0))
})

test_that("my_t.test works mathematically", {
  expect_type(my_t.test(sample, "two.sided", 0), "list")
  expect_type(my_t.test(sample, "greater", 0), "list")
  expect_type(my_t.test(sample, "less", 0), "list")
})
