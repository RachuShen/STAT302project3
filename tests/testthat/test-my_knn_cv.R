# within test-my_knn_cv.R
train <- my_penguins %>%
  tidyr::drop_na() %>%
  dplyr::select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
cl <- my_penguins %>%
  tidyr::drop_na() %>%
  dplyr::select(species)

test_that("my_t.test works mathematically", {
  expect_type(my_knn_cv(train, cl, 5, 5), "list")
})

test_that("non_numeric k_nn and k_cv throws error", {
  expect_error(my_knn_cv(train, cl, "5", 5))
  expect_error(my_knn_cv(train, cl, 5, "5"))
})

test_that("train should be dataframe, otherwise throws error", {
  expect_error(my_knn_cv(as.matrix(train), cl, 5, 5))
})




