#' Confidence Interval for the normal distribution parameters - 2 populations
#'
#' Computes the confidence interval for the difference in two population means or
#' computes the confidence interval for the ratio of two population variances
#' according to the \code{parameter} argument.
#'
#' @param x a (non-empty) numeric vector.
#' @param y a (non-empty) numeric vector.
#' @param sd_pop_1 a number specifying the known standard deviation of the first population. Default value is \code{NULL}.
#' @param sd_pop_2 a number specifying the known standard deviation of the second population. Default value is \code{NULL}.
#' @param var_equal a logical variable indicating whether to treat the two variances as being equal. If \code{TRUE} then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param parameter a character string specifying the parameter in the normal distribution. Must be one of "mean" (confidence interval for mean difference) or "variance" (confidence interval for variance ratio). Default value is "mean".
#' @param conf_level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param type a character string specifying the type of confidence interval. Must be one of "two.sided" (default), "right" or "left".
#' @param na.rm a logical value indicating whether \code{NA} values should be removed before the computation proceeds.
#'
#' @import stats
#'
#' @details \code{type} specifies the type of confidence interval. If \code{type} is "two.sided",  the returned confidence interval is \code{(lower_ci, upper_ci)} when \code{parameter} is "mean" or "variance". If \code{type} is "left", the returned confidence interval is \code{(lower_ci, Inf)} when \code{parameter} is "mean" or "variance". And, finally, is \code{type} is "right", the returned confidence interval is \code{(-Inf, upper_ci))} when \code{parameter} is "mean", and the returned confidence interval is \code{(0, upper_ci)} when \code{parameter} is "variance".
#'
#' @return A 1 x 3 tibble with 'lower_ci', 'upper_ci', and 'conf_level' columns. Values correspond to the lower and upper bounds of the confidence interval, and to the confidence level, respectively.
#'
#' @examples
#' x <- rnorm(1000, mean = 0, sd = 2)
#' y <- rnorm(1000, mean = 0, sd = 1)
#' # confidence interval for difference in two means, unknown variances
#' ci_2pop_norm(x, y)
#'
#' x <- rnorm(1000, mean = 0, sd = 2)
#' y <- rnorm(1000, mean = 0, sd = 3)
#' # confidence interval for difference in two means, known variances
#' ci_2pop_norm(x, y, sd_pop_1 = 2, sd_pop_2 = 3)
#'
#' x <- rnorm(1000, mean = 0, sd = 2)
#' y <- rnorm(1000, mean = 0, sd = 3)
#' # confidence interval for the ratoi of two population variance
#' ci_2pop_norm(x, y, parameter = "variance")
#'
#' @export
ci_2pop_norm <- function(x, y, sd_pop_1 = NULL, sd_pop_2 = NULL, var_equal = FALSE, parameter = "mean", conf_level= 0.95, type = "two.sided", na.rm = F) {
  if (!(type %in% c("two.sided", "left", "right"))) {
    stop("'type' must be one of 'two.sided', 'left' or 'right'.")
  }

  if (conf_level < 0 | conf_level > 1) {
    stop("'conf_level' must be a number between 0 and 1.")
  }

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }

  if (!(parameter %in% c("mean", "variance"))) {
    stop("'parameter' must be one of 'mean' or 'variance'.")
  }

  if ((!is.null(sd_pop_1) | !is.null(sd_pop_2)) & parameter %in% "variance") {
    stop("You should not build a confidence interval for a variance ratio if you know the population variances.")
  }

  if ((!is.null(sd_pop_1) & is.null(sd_pop_2)) | (is.null(sd_pop_1) & !is.null(sd_pop_2))) {
    stop("'sd_pop_1' and 'sd_pop_2' must be different of NULL.")
  }

  size_x <- length(x)
  size_y <- length(y)


# paramter == "mean" ------------------------------------------------------

  if (!is.null(sd_pop_1) & !is.null(sd_pop_2)) {
# known variance ----------------------------------------------------------

      if (type == "left") {
        lower_ci <- -Inf
        upper_ci <- qnorm(conf_level) * sqrt((sd_pop_1^2) / size_x + (sd_pop_2^2) / size_y) + mean(x) - mean(y)
      } else if (type == "right") {
        lower_ci <- qnorm(1 - conf_level) * sqrt((sd_pop_1^2) / size_x + (sd_pop_2^2) / size_y) + mean(x) - mean(y)
        upper_ci <- Inf
      } else {
        lower_ci <- qnorm((1 - conf_level) / 2) * sqrt((sd_pop_1^2) / size_x + (sd_pop_2^2) / size_y) + mean(x) - mean(y)
        upper_ci <- qnorm((1 + conf_level) / 2) * sqrt((sd_pop_1^2) / size_x + (sd_pop_2^2) / size_y) + mean(x) - mean(y)
      }
  } else if (var_equal == TRUE) {
# unknown equal variances ----------------------------------------------

    s_d <- ((size_x - 1) * var(x) + (size_y - 1) * var(y)) / (size_x + size_y - 2)
    if (type == "left") {
      lower_ci <- -Inf
      upper_ci <- qt(conf_level, df = size_x + size_y - 2) * sqrt(s_d * (1 / size_x + 1 / size_y)) + mean(x) - mean(y)
    } else if (type == "right") {
      lower_ci <- qt(1 - conf_level, df = size_x + size_y - 2) * sqrt(s_d * (1 / size_x + 1 / size_y)) + mean(x) - mean(y)
      upper_ci <- Inf
    } else {
      lower_ci <- qt((1 - conf_level) / 2, df = size_x + size_y - 2) * sqrt(s_d * (1 / size_x + 1 / size_y)) + mean(x) - mean(y)
      upper_ci <- qt((1 + conf_level) / 2, df = size_x + size_y - 2) * sqrt(s_d * (1 / size_x + 1 / size_y)) + mean(x) - mean(y)
    }
  } else if (var_equal == FALSE) {

# unknown unequal variances ----------------------------------------
    gl <- (var(x) / size_x + var(y) / size_y)^2 / ((var(x) / size_x)^2 / (size_x - 1) + (var(y) / size_y)^2 / (size_y - 1))
    s_d <- sqrt(var(x) / size_x + var(y) / size_y)
    if (type == "left") {
      lower_ci <- -Inf
      upper_ci <- qt(conf_level, df = gl) * s_d + mean(x) - mean(y)
    } else if (type == "right") {
      lower_ci <- qt(1 - conf_level, df = gl) * s_d + mean(x) - mean(y)
      upper_ci <- Inf
    } else {
      lower_ci <- qt((1 - conf_level) / 2, df = gl) * s_d + mean(x) - mean(y)
      upper_ci <- qt((1 + conf_level) / 2, df = gl) * s_d + mean(x) - mean(y)
    }
  }


# parameter = "variance" --------------------------------------------------

  if (parameter == "variance") {
    if (type == "left") {
      lower_ci <-  0
      upper_ci  <- qf(conf_level, df1 = size_y - 1, df2 = size_x - 1) * var(x) / var(y)
    } else if (type == "right") {
      lower_ci <- qf(1 - conf_level, df1 = size_y - 1, df2 = size_x - 1) * var(x) / var(y)
      upper_ci <- Inf
    } else {
      lower_ci <- qf((1 - conf_level) / 2, df1 = size_y - 1, df2 = size_x - 1) * var(x) / var(y)
      upper_ci <- qf((1 + conf_level) / 2, df1 = size_y - 1, df2 = size_x - 1) * var(x) / var(y)
    }
  }

  return(tibble::tibble(lower_ci = lower_ci, upper_ci = upper_ci, conf_level = conf_level))
}
