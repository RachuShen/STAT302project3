#' k-Nearest Neighbors Cross-Validation
#'
#' This function performs cross validation on k-Nearest Neighbors classification.
#'   More information about k-Nearest Neighbors classification are available at
#'   \code{\link[class]{knn}}.
#'
#' @param train a data frame of training set cases.
#' @param cl a matrix or data frame of test set cases.
#' @param k_nn an integer representing the number of neighbors considered.
#' @param k_cv an integer representing the number of folds.
#' @keywords prediction
#'
#' @return A list containing the following component:
#' \describe{
#'   \item{\code{class}}{Factor of classifications of test set.}
#'   \item{\code{cv_err}}{cross validation error}
#'   }
#'
#' @examples
#' train <- data.frame(rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3]))
#' cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
#' my_knn_cv(train, cl, k_nn = 5, k_cv = 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv){
  # Assign observations to folds
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  # Empty matrix to store predictions and misclassification rate
  pred <- matrix(NA, nrow(train), 1)
  misclas <- matrix(NA, 1, k_cv)

  for (i in 1:k_cv) {
    x_train <- train %>%
      dplyr::mutate(fold = fold) %>%
      dplyr::filter(fold != i) %>%
      subset(select = -fold)
    y_train <- as.matrix(cl)[fold != i]
    x_test <- train %>%
      dplyr::mutate(fold = fold) %>%
      dplyr::filter(fold == i) %>%
      subset(select = -fold)
    y_test <- as.matrix(cl)[fold == i]
    # Train our models
    knn_cv <- class::knn(x_train, x_test, y_train, k_nn, FALSE)
    # Record predictions
    pred[fold == i] <- as.matrix(knn_cv)
    misclas[i] <- mean(knn_cv != y_test)
  }

  return(list(class = pred, cv_err = mean(misclas)))
}
