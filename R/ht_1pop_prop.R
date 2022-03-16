#' Hypothesis Testing for Proportion
#' 
#' One-sample test to proportion. 
#' 
#' @param x a (non-empty) numeric vector indicating the number of success. It can also be a vector with number of success, and it can be vector of 0 and 1.
#' @param n a (non-empty) numeric vector indicating the number of trials. It can also be a vector with number of trials (if \code{x} is vector of success), and it can be \code{NULL} (if \code{x} is a vector of 0 e 1).
#' @param proportion a number between 0 e 1 indicating the value in the null hypothesis. Default value is 0.5.
#' @param alternative a character string specifying the alternative hypothesis, must be one of ‘"two.sided"’ (default), ‘"greater"’ or ‘"less"’.  You can specify just the initial letter.
#' @param conf_level a number indicating the confidence level to compute the confidence interval. If \code{conf_level = NULL}, then confidence interval is not included in the output. Default value is \code{NULL}.
#' @param sig_level a number indicating the significance level to use in the General Procedure for Hypotheiss Testing.
#' @param na.rm a logical value indicating whether ‘NA’ values should be stripped before the computation proceeds.
#' 
#' @import stats stringr tibble
#' 
#' @details I have wrapped the book of Millard and Neerchal (2001) <ISBN: 978-0-367-39814-9>.
#' 
#' @return a \code{tibble} with the following columns:
#' \describe{
#' \item{statistic}{the value of statistic.}
#' \item{p_value}{the p-value for the test.}
#' \item{critical_value}{critical value in the General Procedure for Hypothesis Testing.}
#' \item{critical_region}{critical region in the General Procedure for Hypothesis Testing.}
#' \item{proportion}{a number indicating the true value of the sigma.}
#' \item{alternative}{character string giving the direction of the alternative hypothesis.}
#' \item{lower_ci}{lower bound of the confidence interval. Is is present only if \code{!is.null(con_level)}.}
#' \item{upper_ci}{upper bound of the confidence interval. Is is present only if \code{!is.null(con_level)}.}
#' }
#' 
#' @import stats stringr tibble
#' 
#' @export
#' 
#' @examples 
#' amostra <- rbinom(1, size = 100, prob = 0.75)
#' ht_1pop_prop(amostra, proportion = 0.75, 100, conf_level = 0.99)
#' 
#' amostra <- c(rbinom(1, size = 10, prob = 0.75),
#' rbinom(1, size = 20, prob = 0.75),
#' rbinom(1, size = 30, prob = 0.75))
#' ht_1pop_prop(amostra, c(10, 20, 30), proportion = 0.99, conf_level = 0.90, alternative = 'less')
#' 
#' amostra <- rbinom(100, 1, prob = 0.75)
#' ht_1pop_prop(amostra, proportion = 0.01, conf_level = 0.95, alternative = 'greater')
ht_1pop_prop <- function(x, n = NULL, proportion = 0.5, alternative = "two.sided", conf_level = NULL, sig_level = 0.05, na.rm = TRUE) {
  if (!(alternative %in% c("two.sided", "greater", "less"))) {
    stop("'alternative' must be one of 'two.sided', 'greater' or 'less'.")
  }

  if (sig_level < 0 | sig_level > 1) {
    stop("'sig_level' must be a number between 0 and 1.")
  }

  if (is.null(n)) {
    if (!(min(x, na.rm = T) == 0 & max(x, na.rm = T) == 1)) {
      stop("'x' must be a vector of 0 and 1 when 'n == NULL'.")
    }
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

  if (!is.null(n)) {
    if (length(x) != length(n)) {
      stop("'x' and 'n' must have the same length.")
    } else if (any(x > n, na.rm = T)) {
      stop("Number of sucess must be equal or smaller than number of trials.")
    } else if (any(x < 0, na.rm = T)) {
      stop("Number of sucess must be non-negative.")
    }
  } else {
    n <- rep(1, length(x))
  }

  p_hat <- sum(x) / sum(n)
  statistic <- (p_hat - proportion) * sqrt(sum(n)) / sqrt(proportion * (1 - proportion))
  if (alternative == "two.sided") {
    statistic <- abs(statistic)
    p_value <- 2 * (1 - pnorm(statistic))
    critical_value <- qnorm(1 - sig_level / 2)
    critical_region <- stringr::str_interp("(-Inf,-$[2.3f]{critical_value})U($[2.3f]{critical_value}, Inf)")
  } else if (alternative == "less") {
    p_value <- pnorm(statistic)
    critical_value <- qnorm(sig_level)
    critical_region <- stringr::str_interp("(-Inf,$[2.3f]{critical_value})")
  } else {
    p_value <- 1 - pnorm(statistic)
    critical_value <- qnorm(1 - sig_level)
    critical_region <- stringr::str_interp("($[2.3f]{critical_value}, Inf)")
  }

  if (!is.null(conf_level)) {
    ci <- ci_bern(x, n, conf_level = conf_level)
    output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, proportion, sig_level, lower_ci = ci$lower_ci, upper_ci = ci$upper_ci, conf_level = ci$conf_level)
  } else {
    output <- tibble::tibble(statistic, p_value, critical_value, critical_region, alternative, proportion, sig_level)
  }

  output
}
