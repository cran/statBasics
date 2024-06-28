#' Hypothesis testing mean for two populations
#'
#' Performs a hypothesis testing for the difference in means of two populations.
#'
#' @param x a (non-empty) numeric vector.
#' @param y a (non-empty) numeric vector.
#' @param delta a scalar value indicating the difference in means (\eqn{\Delta_0}). Default value is 0.
#' @param sd_pop_1 a number specifying the known standard deviation of the first population. Default value is \code{NULL}.
#' @param sd_pop_2 a number specifying the known standard deviation of the second population. Default value is \code{NULL}.
#' @param var_equal a logical variable indicating whether to treat the two variances as being equal. If \code{TRUE} then the pooled variance is used to estimate the variance, otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used. Default value is \code{FALSE}.
#' @param alternative a character string specifying the alternative hypothesis, must be one of ‘"two.sided"’ (default), ‘"greater"’ or ‘"less"’.
#' @param conf_level a number indicating the confidence level to compute the confidence interval. If \code{conf_level = NULL}, then confidence interval is not included in the output. Default value is \code{NULL}.
#' @param sig_level a number indicating the significance level to use in the General Procedure for Hypothesis Testing.
#' @param na_rm a logical value indicating whether \code{NA} values should be removed before the computation proceeds.
#'
#' @import stats stringr tibble
#'
#' @details We have wrapped the \code{t.test} and the \code{BSDA::z.test} in a function as explained in the book of Montgomery and Runger (2010) <ISBN: 978-1-119-74635-5>.
#'
#' @return a \code{tibble} with the following columns:
#' \describe{
#' \item{statistic}{the value of the test statistic.}
#' \item{p_value}{the p-value for the test.}
#' \item{critical_value}{critical value in the General Procedure for Hypothesis Testing.}
#' \item{critical_region}{critical region in the General Procedure for Hypothesis Testing.}
#' \item{delta}{a scalar value indicating the value of \eqn{\Delta_0}.}
#' \item{alternative}{character string giving the direction of the alternative hypothesis.}
#' \item{lower_ci}{lower bound of the confidence interval. It is presented only if \code{!is.null(conf_level)}.}
#' \item{upper_ci}{upper bound of the confidence interval. It is presented only if \code{!is.null(conf_level)}.}
#' }
#'
#' @export
#' @examples
#' # t-test: var_equal == FALSE
#' x <- rnorm(1000, mean = 10, sd = 2)
#' y <- rnorm(500, mean = 5, sd = 1)
#' # H0: mu_1 - mu_2 == -1 versus H1: mu_1 - mu_2 != -1
#' ht_2pop_mean(x, y, delta = -1)
#' # t-test: var_equal == TRUE
#' x <- rnorm(1000, mean = 10, sd = 2)
#' y <- rnorm(500, mean = 5, sd = 2)
#' # H0: mu_1 - mu_2 == -1 versus H1: mu_1 - mu_2 != -1
#' ht_2pop_mean(x, y, delta = -1, var_equal = TRUE)
#'
#' # z-test
#' x <- rnorm(1000, mean = 10, sd = 3)
#' x <- rnorm(500, mean = 5, sd = 1)
#' # H0: mu_1 - mu_2 >= 0 versus H1: mu_1 - mu_2 < 0
#' ht_2pop_mean(x, y, delta = 0, sd_pop_1 = 3, sd_pop_2 = 1)
ht_2pop_mean <- function(x, y, delta = 0, sd_pop_1 = NULL, sd_pop_2 = NULL, var_equal = FALSE, alternative = 'two.sided', conf_level = NULL, sig_level = 0.05, na_rm = TRUE) {
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

  if ((!is.null(sd_pop_1) && is.null(sd_pop_2)) || (is.null(sd_pop_1) && !is.null(sd_pop_2))) {
    stop("'sd_pop_1' and 'sd_pop_2' must be either nonnegative scalar values or NULL.")
  }

  if ((!is.null(sd_pop_1) || !is.null(sd_pop_2)) && var_equal == T) {
    warning("'var_equal' should be given only if 'is.null(sd_pop_1) == T' and 'is.null(sd_pop_2) == T'.")
  }

  n_x <- length(x)
  n_y <- length(y)


# known variances ---------------------------------------------------------
  if (!is.null(sd_pop_1) && !is.null(sd_pop_2)) {
    statistic <- (mean(x) - mean(y) - delta) / sqrt(((sd_pop_1^2) / n_x) + ((sd_pop_2^2) / n_y))
    if (alternative == "two.sided") {
      p_value <- 2 * (1 - pnorm(abs(statistic)))
      critical_value <- qnorm(1 - sig_level / 2)
      critical_region <- stringr::str_interp("(-Inf,-$[2.3f]{critical_value})U($[2.3f]{critical_value}, Inf)")
    } else if (alternative == "greater") {
      p_value <- 1 - pnorm(statistic)
      critical_value <- qnorm(1 - sig_level)
      critical_region <- stringr::str_interp("($[2.3f]{critical_value}, Inf)")
    } else {
      p_value <- pnorm(statistic)
      critical_value <- qnorm(sig_level)
      critical_region <- stringr::str_interp("(-Inf, $[2.3f]{critical_value})")
    }
  }

# unknown equal variances  --------------------------------------------
  if (is.null(sd_pop_1) && is.null(sd_pop_2) && (var_equal == T)) {
    s_d <- ((n_x - 1) * sd(x)^2 + (n_y - 1) * sd(y)^2) / (n_x + n_y - 2)
    statistic <- (mean(x) - mean(y) - delta) / sqrt(s_d * (1 / n_x + 1 / n_y))
    gl <- n_x + n_y - 2
    if (alternative == "two.sided") {
      p_value <- 2 * (1 - pt(abs(statistic), df = gl))
      critical_value <- qt(1 - sig_level / 2, df = gl)
      critical_region <- stringr::str_interp("(-Inf,-$[2.3f]{critical_value})U($[2.3f]{critical_value}, Inf)")
    } else if (alternative == "greater") {
      p_value <- 1 - pt(statistic, df = gl)
      critical_value <- qt(1 - sig_level, df = gl)
      critical_region <- stringr::str_interp("($[2.3f]{critical_value}, Inf)")
    } else {
      p_value <- pt(statistic, df = gl)
      critical_value <- qt(sig_level, df = gl)
      critical_region <- stringr::str_interp("(-Inf, $[2.3f]{critical_value})")
    }
  }

# unknown unequal variances -----------------------------------------
  if (is.null(sd_pop_1) && is.null(sd_pop_2) && (var_equal == F)) {
    statistic <- (mean(x) - mean(y) - delta) / sqrt(var(x) / n_x + var(y) / n_y)
    gl <- (var(x) / n_x + var(y) / n_y)^2 / ((var(x) / n_x)^2 / (n_x - 1) + (var(y) / n_y)^2 / (n_y - 1))
    if (alternative == "two.sided") {
      p_value <- 2 * (1 - pt(abs(statistic), df = gl))
      critical_value <- qt(1 - sig_level / 2, df = gl)
      critical_region <- stringr::str_interp("(-Inf,-$[2.3f]{critical_value})U($[2.3f]{critical_value}, Inf)")
    } else if (alternative == "greater") {
      p_value <- 1 - pt(statistic, df = gl)
      critical_value <- qt(1 - sig_level, df = gl)
      critical_region <- stringr::str_interp("($[2.3f]{critical_value}, Inf)")
    } else {
      p_value <- pt(statistic, df = gl)
      critical_value <- qt(sig_level, df = gl)
      critical_region <- stringr::str_interp("(-Inf, $[2.3f]{critical_value})")
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
      alternative = alternative,
      sig_level = sig_level
    )
  } else {
    ci <- ci_2pop_norm(x, y, sd_pop_1, sd_pop_2, var_equal, na.rm = T, type = type, conf_level = conf_level)
    output <- tibble::tibble(
      statistic = statistic,
      p_value = p_value,
      critical_value = critical_value,
      critical_region = critical_region,
      delta = delta,
      alternative = alternative,
      sig_level = sig_level,
      lower_ci = ci$lower_ci,
      upper_ci = ci$upper_ci,
      conf_level = conf_level
    )
  }

  output
}
