# within test-my_rf_cv.R
test_that("non_numeric k throws error", {
  expect_error(my_rf_cv("5"))
})

test_that("my_t.test works mathematically", {
  expect_type(my_rf_cv(5), "double")
})
