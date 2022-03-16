#' Hypothesis Mean for Normal Distribution
#'
#' @param x a (non-empty) numeric vector.
#' @param mu a number indicating the true value of the mean. Default value is 0.
#' @param sd_pop a number specifying the known standard deviation of the population. If \code{sd_pop == NULL}, we use the t-ttest. If \code{!is.null(sd_pop)}, we use the z-test. Default value is \code{NULL}.
#' @param alternative a character string specifying the alternative hypothesis, must be one of ‘"two.sided"’ (default), ‘"greater"’ or ‘"less"’.  You can specify just the initial letter.
#' @param conf_level a number indicating the confidence level to compute the confidence interval. If \code{conf_level = NULL}, then confidence interval is not included in the output. Default value is \code{NULL}.
#' @param sig_level a number indicating the significance level to use in the General Procedure for Hypothesis Testing.
#' @param na.rm a logical value indicating whether ‘NA’ values should be stripped before the computation proceeds.
#'
#' @import stats stringr tibble
#'
#' @details I have wrapped the \code{t.test} and the \code{BSDA::z.test} in a function as explained in the book of Montgomery and Runger (2010) <ISBN: 978-1-119-74635-5>.
#'
#' @return a \code{tibble} with the following columns:
#' \describe{
#' \item{statistic}{the value of statistic.}
#' \item{p_value}{the p-value for the test.}
#' \item{critical_value}{critical value in the General Procedure for Hypothesis Testing.}
#' \item{critical_region}{critical region in the General Procedure for Hypothesis Testing.}
#' \item{mu}{a number indicating the true value of the mean.}
#' \item{alternative}{character string giving the direction of the alternative hypothesis.}
#' \item{lower_ci}{lower bound of the confidence interval. Is is present only if \code{!is.null(con_level)}.}
#' \item{upper_ci}{upper bound of the confidence interval. Is is present only if \code{!is.null(con_level)}.}
#' }
#'
#' @export
#'
#' @examples
#' sample <- rnorm(1000, mean = 10, sd = 2) #t-test
#' ht_1pop_mean(sample, mu = -1) # H0: mu == -1
#'
#' sample <- rnorm(1000, mean = 5, sd = 3) # z-test
#' ht_1pop_mean(sample, mu = 0, sd_pop = 3, alternative = 'less') # H0: mu >= 0
ht_1pop_mean <- function(x, mu = 0, sd_pop = NULL, alternative = 'two.sided', conf_level = NULL, sig_level = 0.05, na.rm = TRUE) {
  if (!(alternative %in% c("two.sided", "greater", "less"))) {
    stop("'alternative' must be one of 'two.sided', 'greater' or 'less'.")
  }

  if (sig_level < 0 | sig_level > 1) {
    stop("'sig_level' must be a number between 0 and 1.")
  }

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  n <- length(x) # sample size
  if (is.null(sd_pop)) {
    # t.test
    statistic <- (mean(x) - mu) * sqrt(n) / sd(x)
    if (alternative == 'two.sided') {
      p_value <- 2 * (1 - pt(abs(statistic), df = n - 1))
      critical_value <- qt(1 - sig_level / 2, df = n - 1)
      critical_region <- stringr::str_interp("(-Inf,-$[2.3f]{critical_value})U($[2.3f]{critical_value}, Inf)")
      if (!is.null(conf_level)) {
        ci <- ci_norm(x, conf_level = conf_level)
        output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, mu, sig_level, lower_ci = ci$lower_ci, upper_ci = ci$upper_ci, conf_level = ci$conf_level)
      } else {
        output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, mu, sig_level)
      }
    } else if (alternative == 'less') {
      p_value <- pt(statistic, df = n - 1)
      critical_value <- qt(sig_level, df = n - 1)
      critical_region <- stringr::str_interp("(-Inf, $[2.f]{critical_value})")
      if (!is.null(conf_level)) {
        ci <- ci_norm(x, conf_level = conf_level)
        output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, mu, sig_level, lower_ci = ci$lower_ci, upper_ci = ci$upper_ci, conf_level = ci$conf_level)
      } else {
        output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, mu, sig_level)
      }
    } else {
      p_value <- 1 - pt(statistic, df = n - 1)
      critical_value <- qt(1 - sig_level, df = n - 1)
      critical_region <- stringr::str_interp("($[2.3f]{critical_value}, Inf)")
      if (!is.null(conf_level)) {
        ci <- ci_norm(x, conf_level = conf_level)
        output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, mu, sig_level, lower_ci = ci$lower_ci, upper_ci = ci$upper_ci, conf_level = ci$conf_level)
      } else {
        output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, mu, sig_level)
      }
    }
  } else {
    # z.test
    statistic <- (mean(x) - mu) * sqrt(n) / sd_pop
    if (alternative == 'two.sided') {
      p_value <- 2 * (1 - pnorm(abs(statistic)))
      critical_value <- qnorm(1 - sig_level / 2)
      critical_region <- stringr::str_interp("(-Inf,-$[2.3f]{critical_value})U($[2.3f]{critical_value}, Inf)")
      if (!is.null(conf_level)) {
        ci <- ci_norm(x, conf_level = conf_level, sd_pop = sd_pop)
        output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, mu, sig_level, lower_ci = ci$lower_ci, upper_ci = ci$upper_ci, conf_level = ci$conf_level)
      } else {
        output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, mu, sig_level)
      }
    } else if (alternative == 'less') {
      p_value <- pnorm(statistic)
      critical_value <- qnorm(sig_level)
      critical_region <- stringr::str_interp("(-Inf,-$[2.3f]{critical_value})")
      if (!is.null(conf_level)) {
        ci <- ci_norm(x, conf_level = conf_level, sd_pop = sd_pop)
        output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, mu, sig_level, lower_ci = ci$lower_ci, upper_ci = ci$upper_ci, conf_level = ci$conf_level)
      } else {
        output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, mu, sig_level)
      }
    } else {
      p_value <- 1 - pnorm(statistic)
      critical_value <- qnorm(1 - sig_level)
      critical_region <- stringr::str_interp("($[2.3f]{critical_value}, Inf)")
      if (!is.null(conf_level)) {
        ci <- ci_norm(x, conf_level = conf_level, sd_pop = sd_pop)
        output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, mu, sig_level, lower_ci = ci$lower_ci, upper_ci = ci$upper_ci, conf_level = ci$conf_level)
      } else {
        output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, mu, sig_level)
      }
    }
  }

  output
}
