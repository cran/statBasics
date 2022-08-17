set.seed(1123)
p1_pop <- 0.3
p2_pop <- 0.7
n1 <- 150
n2 <- 400
x1 <- rbinom(n1, 1, p1_pop) |> c(NA, NA, NA) |> sample()
x2 <- rbinom(n2, 1, p2_pop) |> c(NA, NA, NA, NA) |> sample()
delta <- p1_pop - p2_pop
conf_level <- 0.99
sig_level <- 0.01


# prop_test ---------------------------------------------------------------

# R has a slightely different approach to test proportion of 2 groups
# I've decided to follow the approach of Montgomery for didactic purposes
prop_test <- function(...) {
  values <- list(...)

  x1 <- values[[1]][!is.na(values[[1]])]
  x2 <- values[[2]][!is.na(values[[2]])]
  p1 <- mean(x1)
  p2 <- mean(x2)
  n1 <- length(x1)
  n2 <- length(x2)
  p <- (sum(x1) + sum(x2)) / (n1 + n2)
  statistic <- (p1 - p2 - delta) / sqrt(p * (1 - p) * (1 / n1 + 1 / n2))

  sig_level <- values[["sig_level"]]

  if (values$alternative == "two.sided") {
    type = "two.sided"
  } else if (values$alternative == "greater") {
    type = "right"
  } else {
    type = "left"
  }
  ci <- ci_2pop_bern(x1, x2, conf_level = values$conf_level,
                     type = type)

  if (values[["alternative"]] == "two.sided") {
    critical_value <- qnorm(1 - sig_level / 2)
    p_value <- 2 * (1 - pnorm(abs(statistic)))
  } else if (values[["alternative"]] == "greater") {
    critical_value <- qnorm(1 - sig_level)
    p_value <- 1 - pnorm(statistic)
  } else if (values[["alternative"]] == "less") {
    critical_value <- qnorm(sig_level)
    p_value <- pnorm(statistic)
  }

  return(tibble::tibble(
    statistic = statistic |> base::unname(),
    p_value = p_value,
    critical_value = critical_value,
    lower_ci = ci$lower_ci,
    upper_ci = ci$upper_ci
  ))
}


# two.sided ---------------------------------------------------------------

output1 <- ht_2pop_prop(x1, x2, delta = delta, alternative = "two.sided", conf_level = conf_level, sig_level = sig_level, na_rm = T)
output2 <- prop_test(x1, x2, delta = delta, alternative = "two.sided", conf_level = conf_level, correct = FALSE, sig_level = 0.01)
testthat::test_that("prop_test for comparing proportions, decidision H0, alternative = 'two.sided'", {
  testthat::expect_equal(output1$statistic, output2$statistic)
  testthat::expect_equal(output1$p_value, output2$p_value)
  testthat::expect_equal(output1$critical_value, output2$critical_value)
  testthat::expect_equal(output1$lower_ci, output2$lower_ci)
  testthat::expect_equal(output1$upper_ci, output2$upper_ci)
})

# greater -----------------------------------------------------------------


output1 <- ht_2pop_prop(x1, x2, delta = delta, alternative = "greater", conf_level = conf_level, sig_level = sig_level, na_rm = T)
output2 <- prop_test(x1, x2, delta = delta, alternative = "greater", conf_level = conf_level, correct = FALSE, sig_level = 0.01)
testthat::test_that("prop_test for comparing proportions, decidision H0, alternative = 'greater'", {
  testthat::expect_equal(output1$statistic, output2$statistic)
  testthat::expect_equal(output1$p_value, output2$p_value)
  testthat::expect_equal(output1$critical_value, output2$critical_value)
  testthat::expect_equal(output1$lower_ci, output2$lower_ci)
  testthat::expect_equal(output1$upper_ci, output2$upper_ci)
})

# less --------------------------------------------------------------------


output1 <- ht_2pop_prop(x1, x2, delta = delta, alternative = "less", conf_level = conf_level, sig_level = sig_level, na_rm = T)
output2 <- prop_test(x1, x2, delta = delta, alternative = "less", conf_level = conf_level, correct = FALSE, sig_level = 0.01)
testthat::test_that("prop_test for comparing proportions, decidision H0, alternative = 'less'", {
  testthat::expect_equal(output1$statistic, output2$statistic)
  testthat::expect_equal(output1$p_value, output2$p_value)
  testthat::expect_equal(output1$critical_value, output2$critical_value)
  testthat::expect_equal(output1$lower_ci, output2$lower_ci)
  testthat::expect_equal(output1$upper_ci, output2$upper_ci)
})
