#' Random Forest Cross-Validation
#'
#' This function performs cross validation on random forest regression to predict
#'  output class \code{body_mass_g} using covariates \code{bill_length_mm},
#'  \code{bill_depth_mm}, \code{flipper_length_mm} within \code{\link[palmerpenguins]{penguins}}
#'  dataset. More information about random forest regression are available at
#'  \code{\link[randomForest]{randomForest}}.
#'
#' @param k an integer representing the number of folds.
#' @keywords prediction
#'
#' @return Numeric representing the cross-validation error, which is the average
#'  MSE across all \code{k} folds.
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k){
  # Clean data
  data <- my_penguins %>%
    dplyr::select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm) %>%
    tidyr::drop_na()
  # Assign observations to folds
  fold <- sample(rep(1:k, length = nrow(data)))
  # Empty matrix to store predictions & MSE
  mse <- matrix(NA, k, 1)

  for (i in 1:k) {
    train <- data %>%
      dplyr::mutate(fold = fold) %>%
      dplyr::filter(fold != i)
    test <- data %>%
      dplyr::mutate(fold = fold) %>%
      dplyr::filter(fold == i) %>%
      subset(select = -fold)
    # Train our models
    rd_forest <- randomForest::randomForest(body_mass_g ~ bill_length_mm +
                                            bill_depth_mm + flipper_length_mm,
                                            data = train, ntree = 100)
    # Record predictions
    mse[i, 1] <- mean((stats::predict(rd_forest, test[, -1]) - test$body_mass_g)^2)
  }
  # Compute average MSE to get CV error
  return(mean(mse))
}
