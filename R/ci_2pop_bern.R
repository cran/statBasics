#' Confidence interval for the difference in two population proportions
#'
#' Computes the interval for different in two proportions from two distinct and independent population.
#'
#' @param x a (non-empty) numeric vector of 0 and 1 or a non-negative number representing number of successes.
#' @param y a (non-empty) numeric vector of 0 and 1 or a non-negative number representing number of successes.
#' @param n_x non-negative number of cases.
#' @param n_y non-negative number of cases.
#' @param conf_level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param type a character string specifying the type of confidence interval. Must be one of "two.sided" (default), "right" or "left".
#' @param na.rm a logical value indicating whether \code{NA} values should be removed before the computation proceeds.
#'
#' @import stats
#'
#' @details \code{type} specifies the type of confidence interval. If \code{type} is "two.sided",  the returned confidence interval is \code{(lower_ci, upper_ci)}. If \code{type} is "left", the returned confidence interval is \code{(lower_ci, Inf)}. And, finally, is \code{type} is "right", the returned confidence interval is \code{(-Inf, upper_ci))}.
#'
#' @details If \code{is.null(n_x) == T} and \code{is.null(n_y) == T}, then \code{x} and \code{y} must be a numeric value of 0 and 1 and the proportions are computed using \code{x} and \code{y}. If \code{is.null(n_x) == F} and \code{is.null(n_y) == F}, then  \code{x}, \code{y}, \code{n_x} and \code{n_y} must be non-negative integer scalar and \code{x <= n_x} and \code{y <= n_y}.
#'
#' @return A 1 x 3 tibble with 'lower_ci', 'upper_ci', and 'conf_level' columns. Values correspond to the lower and upper bounds of the confidence interval, and to the confidence level, respectively.
#'
#' @examples
#' x <- 3
#' n_x <- 100
#' y <- 50
#' n_y <- 333
#' ci_2pop_bern(x, y, n_x, n_y)
#'
#' x <- rbinom(100, 1, 0.75)
#' y <- rbinom(500, 1, 0.75)
#' ci_2pop_bern(x, y)
#'
#' @export
ci_2pop_bern <- function(x, y, n_x = NULL, n_y = NULL, conf_level = 0.95, type = "two.sided", na.rm = F) {
  if (!(type %in% c("two.sided", "left", "right"))) {
    stop("'type' must be one of 'two.sided', 'left' or 'right'.")
  }

  if (conf_level < 0 || conf_level > 1) {
    stop("'conf_level' must be a number between 0 and 1.")
  }

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }

  if ((is.null(n_x) && !is.null(n_y)) || (!is.null(n_x) && is.null(n_y))) {
    stop("Either 'n_x' and 'n_y' are both not null or 'n_x' and 'n_y' are both null")
  }

  if (!is.null(n_x) && !is.null(n_y)) {
    if (length(x) != 1 || length(y) != 1 || length(n_x) != 1 || length(n_y) != 1) {
      stop("If 'n_x' and 'n_y' are not NULL, then 'x', 'y', 'n_x' and 'n_y' must be all  nonnegative scalar values.")
    }

    if (is.na(x) || is.na(y) || is.na(n_x) || is.na(n_y)) {
      stop("If 'n_x' and 'n_y' are not NULL, then 'x', 'y', 'n_x' and 'n_y' must be all  non missing  values.")
    }

    if (x > n_x) {
      stop("'x' must be less than or equal to 'n_x'.")
    }

    if (y > n_y) {
      stop("'y' must be less than or equal to 'n_y'")
    }
  }

# 0-1 vectors -------------------------------------------------------------

  if (!is.null(n_x) && !is.null(n_x)) {
    if (type == "right") {
      lower_ci <- -1
      upper_ci <- min(
        1,
        qnorm(conf_level) * sqrt(1 / n_x + 1 / n_y) / 2 + x / n_x - y / n_y
      )
    } else if (type == "left") {
      lower_ci <- max(
        -1,
        qnorm(1 - conf_level) * sqrt(1 / n_x + 1 / n_y) / 2 + x / n_x - y / n_y
      )
      upper_ci <- 1
    } else {
      lower_ci <- max(
        -1,
        qnorm((1 - conf_level) / 2) * sqrt(1 / n_x + 1 / n_y) / 2 + x / n_x - y / n_y
      )
      upper_ci <- min(
        1,
        qnorm((1 + conf_level) / 2) * sqrt(1 / n_x + 1 / n_y) / 2 + x / n_x - y / n_y
      )
    }
  }


# scalar values -----------------------------------------------------------

  if (is.null(n_x) && is.null(n_x)) {
    if (type == "right") {
      lower_ci <- -1
      upper_ci <- min(
        1,
        qnorm(conf_level) * sqrt(1 / length(x) + 1 / length(y)) / 2 + mean(x) - mean(y)
      )
    } else if (type == "left") {
      lower_ci <- max(
        -1,
        qnorm(1 - conf_level) * sqrt(1 / length(x) + 1 / length(y)) / 2 + mean(x) - mean(y)
      )
      upper_ci <- 1
    } else {
      lower_ci <- max(
        -1,
        qnorm((1 - conf_level) / 2) * sqrt(1 / length(x) + 1 / length(y)) / 2 + mean(x) - mean(y)
      )
      upper_ci <- min(
        1,
        qnorm((1 + conf_level) / 2) * sqrt(1 / length(x) + 1 / length(y)) / 2 + mean(x) - mean(y)
      )
    }
  }

  return(tibble::tibble(lower_ci = lower_ci, upper_ci = upper_ci, conf_level = conf_level))
}
