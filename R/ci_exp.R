#' Confidence Interval of Exponential Distribution
#'
#' @param x a (non-empty) numeric vector.
#' @param conf_level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param type a character string specifying the type of confidence interval. Must be one of "two.sided" (default), "right" or "less".
#' @param na.rm a logical value indicating whether ‘NA’ values should be stripped before the computation proceeds.
#'
#' @import stats
#'
#' @details "lower_ci" and "upper_ci" are computed using pivotal quantity, as explained by Montgomery and Runger <<ISBN: 978-1-119-74635-5>.
#'
#' @return A 1 x 3 tibble with 'lower_ci', 'upper_ci' and 'conf_level' columns. Values correspond to lower, upper bounds of the confidence interval and confidence level, respectively.
#'
#' @examples
#' x <- rexp(1000)
#' ci_exp(x)
#'
#' @export
ci_exp <- function(x, conf_level = 0.95, type = "two.sided", na.rm = F) {
  if (!(type %in% c("two.sided", "left", "right"))) {
    stop("'type' must be one of 'two.sided', 'left' or 'right'.")
  }

  if (conf_level < 0 | conf_level > 1) {
    stop("'conf_level' must be a number between 0 and 1.")
  }

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  n <- length(x)
  if (type == "two.sided") {
    lower_ci <- 2 * n * mean(x) / qchisq((1 + conf_level) / 2, df = 2 * n)
    upper_ci <- 2 * n * mean(x) / qchisq((1 - conf_level) / 2, df = 2 * n)
  } else if (type == "left") {
    lower_ci <- 2 * n * mean(x) / qchisq(conf_level, df = 2 * n)
    upper_ci <- Inf
  } else if (type == "right") {
    lower_ci <- 0
    upper_ci <- 2 * n * mean(x) / qchisq(1 - conf_level, df = 2 * n)
  }

  tibble::tibble(lower_ci, upper_ci, conf_level)
}
