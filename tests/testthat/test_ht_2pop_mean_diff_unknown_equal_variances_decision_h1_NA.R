n_x <- 125
n_y <- 85
mean_pop_x <- 10
mean_pop_y <- 5
sd_pop_x <- 7
sd_pop_y <- 7
delta <- -10
sig_level <- 0.01
conf_level <- 0.99
x <- rnorm(n_x, mean_pop_x, sd_pop_x) |> c(NA, NA, NA) |> sample()
y <- rnorm(n_y, mean_pop_y, sd_pop_y) |> c(NA, NA) |> sample()


# t_test from stats

t_test <- function(...) {
  output <- stats::t.test(...)
  lower_ci <- ifelse(is.na(output$conf.int[1]), -Inf, output$conf.int[1])
  upper_ci <- ifelse(is.na(output$conf.int[2]), Inf, output$conf.int[2])
  p_value <- output$p.value
  statistic <- output$statistic
  return(list(
    statistic = statistic |> base::unname(),
    p_value = p_value,
    lower_ci = lower_ci,
    upper_ci = upper_ci
  ))
}


# two.sided ---------------------------------------------------------------


output1 <- ht_2pop_mean(x, y, delta = delta, sig_level = sig_level, conf_level = conf_level, alternative = "two.sided", var_equal = TRUE, na_rm = T)
output2 <- t_test(x, y, alternative = "two.sided", mu = delta, conf.level = conf_level, var.equal = TRUE, na.rm = T)
testthat::test_that("ttest for comparing means when unknown and equal variances, decidision H1, alternative = 'two.sided'", {
  testthat::expect_equal(output1$statistic, output2$statistic)
  testthat::expect_equal(output1$p_value, output2$p_value)
  testthat::expect_equal(output1$lower_ci, output2$lower_ci)
  testthat::expect_equal(output1$upper_ci, output2$upper_ci)
})


# greater -----------------------------------------------------------------


output1 <- ht_2pop_mean(x, y, delta = delta, sig_level = sig_level, conf_level = conf_level, alternative = "greater", var_equal = TRUE, na_rm = T)
output2 <- t_test(x, y, alternative = "greater", mu = delta, conf.level = conf_level, var.equal = TRUE, na.rm = T)
testthat::test_that("ttest for comparing means when unknown and equal variances, decidision H1, alternative = 'greater'", {
  testthat::expect_equal(output1$statistic, output2$statistic)
  testthat::expect_equal(output1$p_value, output2$p_value)
  testthat::expect_equal(output1$lower_ci, output2$lower_ci)
  testthat::expect_equal(output1$upper_ci, output2$upper_ci)
})


# less --------------------------------------------------------------------


output1 <- ht_2pop_mean(x, y, delta = delta, sig_level = sig_level, conf_level = conf_level, alternative = "less", var_equal = TRUE, na_rm = T)
output2 <- t_test(x, y, alternative = "less", mu = delta, conf.level = conf_level, var.equal = TRUE, na.rm = T)
testthat::test_that("ttest for comparing means when unknown and equal variances, decidision H1, alternative = 'less'", {
  testthat::expect_equal(output1$statistic, output2$statistic)
  testthat::expect_equal(output1$p_value, output2$p_value)
  testthat::expect_equal(output1$lower_ci, output2$lower_ci)
  testthat::expect_equal(output1$upper_ci, output2$upper_ci)
})
