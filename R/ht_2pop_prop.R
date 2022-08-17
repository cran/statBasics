#' Hypothesis testing for two population porportions
#' 
#' Comparing proportions in two populations
#'
#' \code{ht_2pop_prop} can be used for testing the null hipothesis that proportions (probabilities of success) in two groups are the same.
#'
#' @param x a vector of 0 and 1, or a scalar of count of sucesses in the first group.
#' @param y a vector of 0 and 1, or a scalar of count of sucesses in the first group.
#' @param n_x a scalar of number of trials in the first group.
#' @param n_y a scalar of number of trials in the second group.
#' @param delta a scalar value indicating the difference in proportions (\eqn{\Delta_0}). Default value is 0.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param conf_level a number indicating the confidence level to compute the confidence interval. If \code{conf_level = NULL}, then the confidence interval is not included in the output. Default value is \code{NULL}.
#' @param sig_level a number indicating the significance level to use in the General Procedure for Hypothesis Testing.
#' @param na_rm a logical value indicating whether \code{NA} values should be removed before the computation proceeds. Default value is \code{FALSE}.
#'
#' @import stats stringr tibble
#'
#'@details If \code{is.null(n_x) == T} and \code{is.null(n_y) == T}, then \code{x} and \code{y} must be a numeric value of 0 and 1 and the proportions are computed using \code{x} and \code{y}. If \code{is.null(n_x) == F} and \code{is.null(n_y) == F}, then  \code{x}, \code{y}, \code{n_x} and \code{n_y} must be non-negative integer scalars and \code{x <= n_x} and \code{y <= n_y}.
#'
#' @return a \code{tibble} with the following columns:
#' \describe{
#' \item{statistic}{the value of the test statistic.}
#' \item{p_value}{the p-value for the test.}
#' \item{critical_value}{critical value in the General Procedure for Hypothesis Testing.}
#' \item{critical_region}{critical region in the General Procedure for Hypothesis Testing.}
#' \item{delta}{a scalar value indicating the value of \code{delta}.}
#' \item{alternative}{character string giving the direction of the alternative hypothesis.}
#' \item{lower_ci}{lower bound of the confidence interval. It is presented only if \code{!is.null(conf_level)}.}
#' \item{upper_ci}{upper bound of the confidence interval. It is presented only if \code{!is.null(conf_level)}.}
#' }
#'
#' @export
#' @examples
#' x <- 3
#' n_x <- 100
#' y <- 50
#' n_y <- 333
#' ht_2pop_prop(x, y, n_x, n_y)
#'
#' x <- rbinom(100, 1, 0.75)
#' y <- rbinom(500, 1, 0.75)
#' ht_2pop_prop(x, y)
ht_2pop_prop <- function(x, y, n_x = NULL, n_y = NULL, delta = 0, alternative = "two.sided", conf_level = NULL, sig_level = 0.05, na_rm = FALSE) {
  if (!(alternative %in% c("two.sided", "greater", "less"))) {
    stop("'alternative' must be one of 'two.sided', 'greater' or 'less'.")
  }

  if (sig_level < 0 | sig_level > 1) {
    stop("'sig_level' must be a number between 0 and 1.")
  }

  if (na_rm == TRUE) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }

  print(!is.null(conf_level))
  if (!is.null(conf_level)) {
    if (conf_level < 0 || conf_level > 1) {
      stop("'conf_level' must be a number between 0 and 1.")
    }
  }

  if ((is.null(n_x) && !is.null(n_y)) || (!is.null(n_x) && is.null(n_y))) {
    stop("Either 'n_x' and 'n_y' are both not null or 'n_x' and 'n_y' are both null")
  }

  if (!is.null(n_x) && !is.null(n_y)) {
    if (length(x) != 1 || length(y) != 1 || length(n_x) != 1 || length(n_y) != 1) {
      stop("If 'n_x' and 'n_y' are not NULL, then 'x', 'y', 'n_x' and 'n_y' must be all nonnegative scalar values.")
    }

    if (is.na(x) || is.na(y) || is.na(n_x) || is.na(n_y)) {
      stop("If 'n_x' and 'n_y' are not NULL, then 'x', 'y', 'n_x' and 'n_y' must be all non missing  values.")
    }

    if (x > n_x) {
      print(stringr::str_interp("x = ${x} and n_x = ${n_x}"))
      stop("'x' must be less than or equal to 'n_x'.")
    }

    if (y > n_y) {
      stop("'y' must be less than or equal to 'n_y'")
    }
  }

# 0-1 vectors -------------------------------------------------------------

  if (is.null(n_x) & is.null(n_y)) {
    p1 <- mean(x)
    p2 <- mean(y)
    n1 <- length(x)
    n2 <- length(y)
    p <- (sum(x) + sum(y)) / (n1 + n2)
    statistic <- (p1 - p2 - delta) / sqrt(p * (1 - p) * (1 / n1 + 1 / n2))
    if (alternative == 'two.sided') {
      p_value <- 2 * (1 - pnorm(abs(statistic)))
      critical_value <- qnorm(1 - sig_level / 2)
      critical_region <- stringr::str_interp("(-Inf,$[.3f]{-critical_value})U($[.3f]{critical_value},Inf)")

    } else if (alternative == 'less') {
      p_value <-  pnorm(statistic)
      critical_value <- qnorm(sig_level)
      critical_region <- stringr::str_interp("(-Inf,$[.3f]{critical_value})")
    } else if (alternative == 'greater') {
      p_value <-  1 - pnorm(statistic)
      critical_value <- qnorm(1 - sig_level)
      critical_region <- stringr::str_interp("($[.3f]{critical_value}, Inf)")
    }
  }


# scalar values -----------------------------------------------------------

  if (!is.null(n_x) & !is.null(n_y)) {
    p_x <- x / n_x
    p_y <- y / n_y
    p <- (x + y) / (n_x + n_y)
    statistic <- (p_x - p_y - delta) / sqrt(p * (1 - p) * (1 / n_x + 1 / n_y))
    if (alternative == 'two.sided') {
      p_value <- 2 * (1 - pnorm(abs(statistic)))
      critical_value <- qnorm(1 - sig_level / 2)
      critical_region <- stringr::str_interp("(-Inf,$[.3f]{-critical_value})U($[.3f]{critical_value},Inf)")
    } else if (alternative == 'less') {
      p_value <-  pnorm(statistic)
      critical_value <- qnorm(sig_level)
      critical_region <- stringr::str_interp("(-Inf,$[.3f]{critical_value})")
    } else if (alternative == 'greater') {
      p_value <-  1 - pnorm(statistic)
      critical_value <- qnorm(1 - sig_level)
      critical_region <- stringr::str_interp("($[.3f]{critical_value}, Inf)")
    }
  }

  if (alternative == "two.sided") {
    type = "two.sided"
  } else if (alternative == "greater") {
    type = "right"
  } else {
    type = "left"
  }

  if (is.null(conf_level)) {
    output <- tibble::tibble(
      statistic = statistic,
      p_value = p_value,
      critical_value = critical_value,
      critical_region = critical_region,
      delta = delta,
      alternative = alternative
    )
  } else {
    ci <- ci_2pop_bern(x, y, n_x, n_y, na.rm = T, type = type, conf_level = conf_level)
    output <- tibble::tibble(
      statistic = statistic,
      p_value = p_value,
      critical_value = critical_value,
      critical_region = critical_region,
      delta = delta,
      alternative = alternative,
      lower_ci = ci$lower_ci,
      upper_ci = ci$upper_ci
    )
  }

  output
}
