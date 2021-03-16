#' Student's t-test
#'
#' Performs one sample t-test on vectors of data.
#'
#' @param x a non-empty numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis.
#'   This only accept \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' @param mu a number indicating the null hypothesis value of the mean.
#' @keywords inference
#'
#' @return A list containing the following component:
#' \describe{
#'   \item{\code{test_stat}}{the value of the t_statistic.}
#'   \item{\code{df}}{the degree of freedom for the t-statistic.}
#'   \item{\code{alternative}}{a character string describing the alternative hypothesis.}
#'   \item{\code{p_val}}{the p-value for the test.}
#'   }
#'
#' @examples
#' x <- rnorm(20, mean = 0, sd = 1)
#' my_t.test(x, mu = 0)
#' my_t.test(x, alternative = "less", mu = 0)
#'
#' @export
my_t.test <- function(x, alternative = "two.sided", mu) {
  # Throw error
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop("no applicable method for Argument ", alternative)
  }
  # Define
  if (!is.numeric(x)){
    stop("x should be numeric")
  }
  est <- mean(x)
  df <- length(x) - 1
  se <- stats::sd(x) / sqrt(length(x))
  t_obs <- (est - mu) / se
  # Calculate p value
  if (alternative == "less") {
    p_value <- stats::pt(t_obs, df, lower.tail = T)
  } else if (alternative == "greater") {
    p_value <- stats::pt(t_obs, df, lower.tail = F)
  } else {
    if ((stats::pt(t_obs, df, lower.tail = T)) > 0.5) {
      p_value <- 2 * stats::pt(t_obs, df, lower.tail = F)
    } else {
      p_value <- 2 * stats::pt(t_obs, df, lower.tail = T)
    }
  }
  # Generate and return result
  result <- list("test_stat" = t_obs,
                 "df" = df,
                 "alternative" = alternative,
                 "p_val" = p_value)
  return(result)
}
