# within test-my_lm.R
data0 <- my_penguins %>%
  dplyr::select(body_mass_g, bill_length_mm) %>%
  tidyr::drop_na()
data1 <- my_penguins %>%
  dplyr::select( bill_length_mm) %>%
  tidyr::drop_na()

test_that("my_lm works mathematically", {
  expect_type(my_lm(body_mass_g ~ bill_length_mm, data = data0), "double")
  expect_is(my_lm(body_mass_g ~ bill_length_mm, data = data0), "matrix")
})
test_that("variables not included in data throws error", {
  expect_error(my_lm(body_mass_g ~ bill_length_mm, data = data1))
})
test_that("data must be dataframe", {
  expect_error(my_lm(body_mass_g ~ bill_length_mm, data = as.matrix(data0)))
})

