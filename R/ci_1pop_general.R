#' Confidence interval for a population mean (general case)
#'
#' @param x a (non-empty) numeric vector.
#' @param conf_level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param type a character string specifying the type of confidence interval. Must be one of "two.sided" (default), "right" or "left".
#' @param na.rm a logical value indicating whether \code{NA} values should be removed before the computation proceeds.
#'
#' @import stats
#'
#' @details "lower_ci" and "upper_ci" are computed using the `t.test` function.
#'
#' @return A 1 x 3 tibble with 'lower_ci', 'upper_ci', and 'conf_level' columns. Values correspond to the lower and upper bounds of the confidence interval, and the confidence level, respectively.
#'
#' @examples
#' x <- rpois(1000, lambda = 10)
#' ci_1pop_general(x)
#'
#' @export
ci_1pop_general <- function(x, conf_level = 0.95, type = "two.sided", na.rm = F) {
  if (!(type %in% c("two.sided", "left", "right"))) {
    stop("'type' must be one of 'two.sided', 'left' or 'right'.")
  }

  if (conf_level < 0 | conf_level > 1) {
    stop("'conf_level' must be a number between 0 and 1.")
  }

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (type == "two.sided") {
    lower_ci <- stats::t.test(x, alternative = 'two.sided', conf.level = conf_level)$conf.int[1]
    upper_ci <- stats::t.test(x, alternative = 'two.sided', conf.level = conf_level)$conf.int[2]
  } else if (type == "left") {
    lower_ci <- stats::t.test(x, alternative = 'greater', conf.level = conf_level)$conf.int[1]
    upper_ci <- stats::t.test(x, alternative = 'greater', conf.level = conf_level)$conf.int[2]
  } else if (type == "right") {
    lower_ci <- stats::t.test(x, alternative = 'less', conf.level = conf_level)$conf.int[1]
    upper_ci <- stats::t.test(x, alternative = 'less', conf.level = conf_level)$conf.int[2]
  }

  tibble::tibble(lower_ci, upper_ci, conf_level)
}
