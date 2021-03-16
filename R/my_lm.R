#' Fitting Linear Models
#'
#' This function fits a linear model.
#'
#' @param formula an object of class "formula" that gives a symbolic description
#'  of the model to be fitted.
#' @param data a data frame containing the variables in the model.
#' @keywords inference
#' @keywords prediction
#'
#' @return A table containing the following componenrmt:
#' \describe{
#'   \item{\code{Estimate}}{the linear regression coefficients of each variables.}
#'   \item{\code{Std. Error}}{the standard error for coefficients.}
#'   \item{\code{t value}}{the value of the t_statistic for coefficients}
#'   \item{\code{Pr(>|t|)}}{P value for tests for coefficients}
#'   }
#'
#' @examples
#' my_lm(mpg ~ wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  x <- stats::model.matrix(formula, data)
  y <- stats::model.response(stats::model.frame(formula, data))
  # compute coefficents
  coefs <- solve(t(x) %*% x, t(x) %*% y)
  # Create matrix and add coefficients
  result <- matrix(NA, nrow = nrow(coefs), ncol = 4)
  result[, 1] <- coefs
  # Add Std. Error
  df <- nrow(data) - nrow(coefs)
  variance <- sum((y - x %*% coefs) ^ 2 / df)
  se <- sqrt(abs(variance * solve(t(x) %*% x))) %>%
    diag()
  result[, 2] <- se
  # Add t value
  result[, 3] <- (coefs - 0) / se
  # Add Pr(>|t|)
  result[, 4] <- ifelse(result[, 3] > 0, 2 * stats::pt(result[, 3], df, lower.tail = F),
                        2 * stats::pt(result[, 3], df, lower.tail = T))
  rownames(result) <- colnames(x)
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(result)
}
