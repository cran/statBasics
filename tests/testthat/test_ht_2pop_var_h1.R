n_x <- 125
n_y <- 85
mean_pop_x <- 10
mean_pop_y <- 5
sd_pop_x <- 7
sd_pop_y <- 7
ratio <- 1
sig_level <- 0.01
conf_level <- 0.99
x <- rnorm(n_x, mean_pop_x, sd_pop_x)
y <- rnorm(n_y, mean_pop_y, sd_pop_y)



# var_test from stats -----------------------------------------------------

var_test <- function(...) {
  output <- var.test(...)
  lower_ci <- ifelse(is.na(output$conf.int[1]), -Inf, output$conf.int[1])
  upper_ci <- ifelse(is.na(output$conf.int[2]), Inf, output$conf.int[2])
  p_value <- output$p.value
  statistic <- output$statistic
  return(tibble::tibble(
    statistic = statistic |> base::unname(),
    p_value = p_value,
    lower_ci = lower_ci,
    upper_ci = upper_ci
  ))
}


# two.sided ---------------------------------------------------------------

output1 <- ht_2pop_var(x, y, ratio = ratio, alternative = "two.sided", conf_level = conf_level, sig_level = sig_level)
output2 <- var_test(x, y, ratio = ratio, alternative = "two.sided", conf.level = conf_level, sig_level = sig_level)
testthat::test_that("ttest for comparing variances (ratio = 1), decidision H0, alternative = 'two.sided'", {
  testthat::expect_equal(output1$statistic[1], output2$statistic)
  testthat::expect_equal(output1$p_value[1], output2$p_value)
  testthat::expect_equal(output1$lower_ci[1], output2$lower_ci)
  testthat::expect_equal(output1$upper_ci[1], output2$upper_ci)
})


# greater -----------------------------------------------------------------


output1 <- ht_2pop_var(x, y, ratio = ratio, alternative = "greater", conf_level = conf_level, sig_level = sig_level)
output2 <- var_test(x, y, ratio = ratio, alternative = "greater", conf.level = conf_level, sig_level = sig_level)
testthat::test_that("ttest for comparing variances (ratio = 1), decidision H0, alternative = 'greater'", {
  testthat::expect_equal(output1$statistic, output2$statistic)
  testthat::expect_equal(output1$p_value, output2$p_value)
  testthat::expect_equal(output1$lower_ci, output2$lower_ci)
  testthat::expect_equal(output1$upper_ci, output2$upper_ci)
})


# less --------------------------------------------------------------------


output1 <- ht_2pop_var(x, y, ratio = ratio, alternative = "less", conf_level = conf_level, sig_level = sig_level)
output2 <- var_test(x, y, ratio = ratio, alternative = "less", conf.level = conf_level, sig_level = sig_level)
testthat::test_that("ttest for comparing variances (ratio = 1), decidision H0, alternative = 'less'", {
  testthat::expect_equal(output1$statistic, output2$statistic)
  testthat::expect_equal(output1$p_value, output2$p_value)
  testthat::expect_equal(output1$lower_ci, output2$lower_ci)
  testthat::expect_equal(output1$upper_ci, output2$upper_ci)
})
