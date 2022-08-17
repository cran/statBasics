#' Confidence interval for a population proportion
#'
#' \code{ci_1pop_bern} can be used for obtaining the confidence intervalo for a proportion for a group.
#'
#' @param x a vector of counts of sucesses.
#' @param n a vector of counts of trials.
#' @param conf_level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param type a character string specifying the type of confidence interval. Must be one of "two.sided" (default), "right" or "left".
#' @param na.rm a logical value indicating whether \code{NA} values should be removed before the computation proceeds.
#'
#' @import stats
#'
#' @details \code{type} specifies the type of confidence interval. If \code{type} is "two.sided",  the returned confidence interval is \code{(lower_ci, upper_ci)}. If \code{type} is "left", the returned confidence interval is \code{(lower_ci, 1)}. And, finally, if \code{type} is "right", the returned confidence interval is \code{(0, upper_ci))}.
#'
#' @return A 1 x 3 tibble with 'lower_ci', 'upper_ci', and 'conf_level' columns. Values correspond to the lower and upper bounds of the confidence interval, and to the confidence level, respectively.
#'
#' @examples
#' heads <- rbinom(1, size = 100, prob = .5)
#' ci_1pop_bern(heads)
#'
#' @export
ci_1pop_bern <- function(x, n = NULL, conf_level = 0.95, type = "two.sided", na.rm = F) {
  if (!(type %in% c("two.sided", "left", "right"))) {
    stop("'type' must be one of 'two.sided', 'left' or 'right'.")
  }

  if (conf_level < 0 | conf_level > 1) {
    stop("'conf_level' must be a number between 0 and 1.")
  }

  if (!is.null(n)) {
    if (length(x) != length(n)) {
      stop("'x' and 'n' must have the same length.")
    } else if (any(x > n, na.rm = T)) {
      stop("Number of sucesses must be equal or smaller than the number of trials.")
    } else if (any(x < 0, na.rm = T)) {
      stop("Number of sucesses must be non-negative.")
    }
  } else {
    n <- rep(1, length(x))
  }

  if (na.rm == TRUE) {
    if (!is.null(n)) {
      logical_pos <- !(is.na(x) | is.na(n))
      x <- x[logical_pos]
      n <- n[logical_pos]
    } else {
      logical_pos <- !is.na(x)
      x <- x[logical_pos]
    }
  }

  p_hat <- sum(x) / sum(n)

  if (type == "two.sided") {
    lower_ci <- max(qnorm((1 - conf_level) / 2) / (2 * sqrt(sum(n))) + p_hat, 0)
    upper_ci <- min(qnorm((1 + conf_level) / 2) / (2 * sqrt(sum(n))) + p_hat, 1)
  } else if (type == "left") {
    lower_ci <- max(qnorm(1 - conf_level) / (2 * sqrt(sum(n))) + p_hat, 0)
    upper_ci <- 1
  } else if (type == "right") {
    lower_ci <- 0
    upper_ci <- min(qnorm(conf_level) / (2 * sqrt(sum(n))) + p_hat, 1)
  }

  tibble::tibble(lower_ci, upper_ci, conf_level)
}
