#' Confidence Interval for Normal Distribution
#' 
#' @param x a (non-empty) numeric vector.
#' @param sd_pop a number specifying the known standard deviation of the population.
#' @param parameter a character string specifying the parameter in the normal distribution. Must be one of "mean" or "variance".
#' @param conf_level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param type a character string specifying the type of confidence interval. Must be one of "two.sided" (default), "right" or "less".
#' @param na.rm a logical value indicating whether ‘NA’ values should be stripped before the computation proceeds.
#' 
#' @import stats
#' 
#' @details \code{type} specifies the type of confidence interval. If \code{type} is "two.sided",  the returned confidence interval is \code{(lower_ci, upper_ci)} when \code{parameter} is "mean" or "variance". If \code{type} is "left", the returned confidence interval is \code{(lower_ci, Inf)} when \code{parameter} is "mean" or "variance". And, finally, is \code{type} is "right", the returned confidence interval is \code{(-Inf, upper_ci))} when \code{parameter} is "mean", and the returned confidence interval is \code{(0, upper_ci)} when \code{parameter} is "variance".
#' 
#' @return A 1 x 3 tibble with 'lower_ci', 'upper_ci' and 'conf_level' columns. Values correspond to lower, upper bounds of the confidence interval and confidence level, respectively.
#' 
#' @examples 
#' x <- rnorm(1000)
#' ci_norm(x) # unknown variance and confidence interval for mean
#' 
#' x <- rnorm(1000, sd = 2)
#' ci_norm(x, sd_pop = 2) # known variance and confidence interval for mean
#' 
#' x <- rnorm(1000, sd = 5)
#' ci_norm(x, parameter = "variance") # confidence interval for variance
#' 
#' @export
ci_norm <- function(x, sd_pop = NULL, parameter = "mean", conf_level = 0.95, type = "two.sided", na.rm = F) {
  if (!(type %in% c("two.sided", "left", "right"))) {
    stop("'type' must be one of 'two.sided', 'left' or 'right'.")
  }

  if (conf_level < 0 | conf_level > 1) {
    stop("'conf_level' must be a number between 0 and 1.")
  }

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (!(parameter %in% c("mean", "variance"))) {
    stop("'parameter' must be one of 'mean' or 'variance'.")
  }

  if (!is.null(sd_pop) & parameter %in% "variance") {
    warning("You should not build a confidence interval for variance if you know the populational standard deviation.")
  }

  n <- length(x)
  if (parameter == 'mean') {
    if (is.null(sd_pop)) {
      if (type == 'two.sided') {
        lower_ci <- qt((1 - conf_level) / 2, df = n - 1) * sd(x) / sqrt(n) + mean(x)
        upper_ci <- qt((1 + conf_level) / 2, df = n - 1) * sd(x) / sqrt(n) + mean(x)
      } else if (type == 'left') {
        lower_ci <- qt(1 - conf_level, df = n - 1) * sd(x) / sqrt(n) + mean(x)
        upper_ci <- Inf
      } else if (type == 'right') {
        lower_ci <- -Inf
        upper_ci <- qt(conf_level, df = n - 1) * sd(x) / sqrt(n) + mean(x)
      }
    } else {
      if (type == 'two.sided') {
        lower_ci <- qnorm((1 - conf_level) / 2) * sd_pop / sqrt(n) + mean(x)
        upper_ci <- qnorm((1 + conf_level) / 2) * sd_pop / sqrt(n) + mean(x)
      } else if (type == 'left') {
        lower_ci <- qnorm(1 - conf_level) * sd_pop / sqrt(n) + mean(x)
        upper_ci <- Inf
      } else if (type == 'right') {
        lower_ci <- -Inf
        upper_ci <- qnorm(conf_level) * sd_pop / sqrt(n) + mean(x)
      }

    }
  } else if (parameter == 'variance') {
    if (type == 'two.sided') {
      lower_ci <- max((n - 1) * sd(x) ^ 2 / qchisq((1 + conf_level) / 2, df = n - 1), 0)
      upper_ci <- (n - 1) * sd(x) ^ 2 / qchisq((1 - conf_level) / 2, df = n - 1)
    } else if (type == 'left') {
      lower_ci <- max((n - 1) * sd(x) ^ 2 / qchisq(conf_level, df = n - 1), 0)
      upper_ci <- Inf
    } else if (type == 'right') {
      lower_ci <- 0
      upper_ci <- (n - 1) * sd(x) ^ 2 / qchisq(1 - conf_level, df = n - 1)
    }
  }

  tibble::tibble(lower_ci, upper_ci, conf_level)
}