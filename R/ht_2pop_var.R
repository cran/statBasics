#' F Test to compare two variances
#'
#' Performs a F test to compare the variances of two normal populations.
#'
#' @param x a (non-empty) numeric vector.
#' @param y a (non-empty) numeric vector.
#' @param ratio the hypothesized ratio of the population variances of \code{x} and \code{y}. Default value is 1.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param conf_level a number indicating the confidence level to compute the confidence interval. If \code{conf_level = NULL}, then the confidence interval is not included in the output. Default value is \code{NULL}.
#' @param sig_level a number indicating the significance level to use in the General Procedure for Hypothesis Testing.
#' @param na_rm a logical value indicating whether \code{NA} values should be removed before the computation proceeds. Default value is \code{FALSE}.
#'
#' @import stats stringr tibble
#'
#' @details We have wrapped the \code{var.test} in a function as explained in the book of Montgomery and Runger (2010) <ISBN: 978-1-119-74635-5>.
#'
#' @return a \code{tibble} with the following columns:
#' \describe{
#' \item{statistic}{the value of the test statistic.}
#' \item{p_value}{the p-value for the test.}
#' \item{critical_value}{critical value in the General Procedure for Hypothesis Testing.}
#' \item{critical_region}{critical region in the General Procedure for Hypothesis Testing.}
#' \item{ratio}{a scalar value indicating the value of \code{ratio}.}
#' \item{alternative}{character string giving the direction of the alternative hypothesis.}
#' \item{lower_ci}{lower bound of the confidence interval. It is presented only if \code{!is.null(conf_level)}.}
#' \item{upper_ci}{upper bound of the confidence interval. It is presented only if \code{!is.null(conf_level)}.}
#' }
#'
#' @export
#' @examples
#' x <- rnorm(100, sd = 2)
#' y <- rnorm(1000, sd = 10)
#' ht_2pop_var(x, y)
ht_2pop_var <- function(x, y, ratio = 1, alternative = "two.sided", conf_level = NULL, sig_level = 0.05, na_rm = FALSE) {
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

  if (!is.null(conf_level)) {
    if (conf_level < 0 || conf_level > 1) {
      stop("'conf_level' must be a number between 0 e 1.")
    }
  }

  n_x <- length(x)
  n_y <- length(y)
  statistic <- var(x) / var(y)

  if (alternative == "two.sided") {
    p_value <- 2 * min(pf(statistic, df1 = n_x - 1, df2 = n_y - 1), 1 - pf(statistic, df1 = n_x - 1, df2 = n_y - 1))
    critical_value <- c(qf(sig_level / 2, df1 = n_x - 1, df2 = n_y - 1), qf(1 - sig_level / 2, df1 = n_x - 1, df2 = n_y - 1))
    critical_region <- stringr::str_interp("(0,$[2.3f]{critical_value[[1]]})U($[2.3f]{critical_value[[2]]}, Inf)")
  } else if (alternative == "greater") {
    p_value <- 1 - pf(statistic, df1 = n_x - 1, df2 = n_y - 1)
    critical_value <- qf(1 - sig_level, df1 = n_x - 1, df2 = n_y - 1)
    critical_region <- stringr::str_interp("($[2.3f]{critical_value}, Inf)")
  } else {
    p_value <- pf(statistic, df1 = n_x - 1, df2 = n_y - 1)
    critical_value <- qf(sig_level, df1 = n_x - 1, df2 = n_y - 1)
    critical_region <- stringr::str_interp("(0, $[2.f]{critical_value})")
  }

  if (alternative == "two.sided") {
    type = "two.sided"
  } else if (alternative == "greater") {
    type = "right"
  } else {
    type = "left"
  }

  if (!is.null(conf_level)) {
    ci <- ci_2pop_norm(x, y, parameter = "variance", type = type, conf_level = conf_level)
    output <- tibble::tibble(
      statistic = statistic,
      p_value = p_value,
      critical_vale = critical_value,
      critical_region = critical_region,
      alternative = alternative,
      ratio = ratio,
      sig_level = sig_level,
      lower_ci = ci$lower_ci,
      upper_ci = ci$upper_ci,
      conf_level = conf_level
    )
  } else {
    output <- tibble::tibble(
      statistic = statistic,
      p_value = p_value,
      critical_vale = critical_value,
      critical_region = critical_region,
      alternative = alternative,
      ratio = ratio,
      sig_level = sig_level
    )
  }

  output
}
