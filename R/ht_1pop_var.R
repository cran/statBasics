#' Hypothesis testing for the population variance
#'
#' One-Sample chi-squared test on variance.
#'
#' @param x a (non-empty) numeric vector.
#' @param sigma a number indicating the true value of the standard deviation in the null hypothesis. Default value is 1.
#' @param alternative a character string specifying the alternative hypothesis, must be one of ‘"two.sided"’ (default), ‘"greater"’ or ‘"less"’. You can specify just the initial letter.
#' @param conf_level a number indicating the confidence level to compute the confidence interval. If \code{conf_level = NULL}, then the confidence interval is not included in the output. Default value is \code{NULL}.
#' @param sig_level a number indicating the significance level to use in the General Procedure for Hypotheiss Testing.
#' @param na.rm a logical value indicating whether \code{NA} values should be remove before the computation proceeds.
#'
#' @import stats stringr tibble
#'
#' @details We have wrapped the \code{EnvStats::varTest} in a function as explained in the book of Montgomery and Runger (2010) <ISBN: 978-1-119-74635-5>.
#'
#' @return a \code{tibble} with the following columns:
#' \describe{
#' \item{statistic}{the value of the test statistic.}
#' \item{p_value}{the p-value for the test.}
#' \item{critical_value}{critical value in the General Procedure for Hypothesis Testing.}
#' \item{critical_region}{critical region in the General Procedure for Hypothesis Testing.}
#' \item{sigma}{a number indicating the true value of sigma.}
#' \item{alternative}{character string giving the direction of the alternative hypothesis.}
#' \item{lower_ci}{lower bound of the confidence interval. It is presented only if \code{!is.null(con_level)}.}
#' \item{upper_ci}{upper bound of the confidence interval. It is presented only if \code{!is.null(con_level)}.}
#' }
#'
#' @export
#'
#' @examples
#' sample <- rnorm(1000, mean = 10, sd = 2)
#' ht_1pop_var(sample, sigma = 1) # H0: sigma = 1
ht_1pop_var <- function(x, sigma = 1, alternative = "two.sided", conf_level = NULL, sig_level = 0.05, na.rm = TRUE) {
  if (!(alternative %in% c("two.sided", "greater", "less"))) {
    stop("'alternative' must be one of 'two.sided', 'greater' or 'less'.")
  }

  if (sig_level < 0 | sig_level > 1) {
    stop("'sig_level' must be a number between 0 and 1.")
  }

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  n <- length(x)
  statistic <- var(x) * (n - 1) / sigma ^ 2
  if (alternative == "two.sided") {
    p_value <- 2 * min(1 - pchisq(statistic, df = n - 1), pchisq(statistic, df = n - 1))
    critical_value <- c(qchisq(sig_level / 2, df = n - 1), qchisq(1 - sig_level / 2, df = n - 1))
    critical_region <- stringr::str_interp("(0,$[2.3f]{critical_value[[1]]})U($[2.3f]{critical_value[[2]]}, Inf)")
  } else if (alternative == "less") {
    p_value <- pchisq(statistic, df = n - 1)
    critical_value <- qchisq(sig_level, df = n - 1)
    critical_region <- stringr::str_interp("(0, $[2.f]{critical_value})")
  } else {
    p_value <- 1 - pchisq(statistic, df = n - 1)
    critical_value <- qchisq(1 - sig_level, df = n - 1)
    critical_region <- stringr::str_interp("($[2.3f]{critical_value}, Inf)")
  }

  if (!is.null(conf_level)) {
    ci <- ci_1pop_norm(x, conf_level = conf_level, parameter = 'variance')
    output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, sigma, sig_level, lower_ci = ci$lower_ci, upper_ci = ci$upper_ci, conf_level = ci$conf_level)
  } else {
    output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, sigma, sig_level)
  }

  output
}
