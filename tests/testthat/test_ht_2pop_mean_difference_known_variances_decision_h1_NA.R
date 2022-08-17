n_x <- 125
n_y <- 85
mean_pop_x <- 10
mean_pop_y <- 5
sd_pop_x <- 3
sd_pop_y <- 7
delta <- 0
sig_level <- 0.01
conf_level <- 0.99
x <- rnorm(n_x, mean_pop_x, sd_pop_x) |> c(rep(NA, 5)) |> sample()
y <- rnorm(n_y, mean_pop_y, sd_pop_y) |> c(rep(NA, 10)) |> sample()


# z_test from BSDA -- fixing NA problem with conf.int ---------------------

z_test <- function(...) {
  output <- BSDA::z.test(...)
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
na_rm <- \(x) x[!is.na(x)]

# two.sided ---------------------------------------------------------------


output1 <- ht_2pop_mean(x, y, delta = delta, sd_pop_1 = sd_pop_x, sd_pop_2 = sd_pop_y, sig_level = sig_level, conf_level = conf_level, alternative = "two.sided", na_rm = T)
output2 <- z_test(x |> na_rm(), y |> na_rm(), alternative = "two.sided", mu = delta, sigma.x = sd_pop_x, sigma.y = sd_pop_y, conf.level = conf_level)
testthat::test_that("ztest for comparing means when unknown variances, decidision H0, alternative = 'two.sided'", {
  testthat::expect_equal(output1$statistic, output2$statistic)
  testthat::expect_equal(output1$p_value, output2$p_value)
  testthat::expect_equal(output1$lower_ci, output2$lower_ci)
  testthat::expect_equal(output1$upper_ci, output2$upper_ci)
})


# greater -----------------------------------------------------------------


output1 <- ht_2pop_mean(x, y, delta = delta, sd_pop_1 = sd_pop_x, sd_pop_2 = sd_pop_y, sig_level = sig_level, conf_level = conf_level, alternative = "greater", na_rm = T)
output2 <- z_test(x |> na_rm(), y |> na_rm(), alternative = "greater", mu = delta, sigma.x = sd_pop_x, sigma.y = sd_pop_y, conf.level = conf_level)
testthat::test_that("ztest for comparing means when known variances, decidision H0, alternative = 'greater'", {
  testthat::expect_equal(output1$statistic, output2$statistic)
  testthat::expect_equal(output1$p_value, output2$p_value)
  testthat::expect_equal(output1$lower_ci, output2$lower_ci)
  testthat::expect_equal(output1$upper_ci, output2$upper_ci)
})


# less --------------------------------------------------------------------


output1 <- ht_2pop_mean(x, y, delta = delta, sd_pop_1 = sd_pop_x, sd_pop_2 = sd_pop_y, sig_level = sig_level, conf_level = conf_level, alternative = "less", na_rm = T)
output2 <- z_test(x |> na_rm(), y |> na_rm(), alternative = "less", mu = delta, sigma.x = sd_pop_x, sigma.y = sd_pop_y, conf.level = conf_level)
testthat::test_that("ztest for comparing means when known variances, decidision H0, alternative = 'less'", {
  testthat::expect_equal(output1$statistic, output2$statistic)
  testthat::expect_equal(output1$p_value, output2$p_value)
  testthat::expect_equal(output1$lower_ci, output2$lower_ci)
  testthat::expect_equal(output1$upper_ci, output2$upper_ci)
})
