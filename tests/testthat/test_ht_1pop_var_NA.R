sd_pop <- 3
amostra <- c(round(rnorm(10, sd = 3), 3), NA, NA)
x0 <- amostra[!is.na(amostra)]
n0 <- length(x0)
size <- length(amostra)
sd0_0 <- 3 # decision for H0
sd0_1 <- 15 # decision for H1
sig_level <- 0.05
conf_level <- 0.95

var_test <- function(x, alternative = "two.sided", conf_level = 0.95, sigma = 1) {
  suppressWarnings({
    output <- EnvStats::varTest(x, alternative = alternative, conf.level = conf_level, sigma.squared = sigma ^ 2)
  })

  ci <- ci_1pop_norm(x, conf_level = conf_level, parameter = 'variance')
  list(
        statistic = base::unname(output$statistic),
        p_value = base::unname(output$p.value),
        alternative = unname(alternative),
        lower_ci = ci$lower_ci,
        upper_ci = ci$upper_ci
    )
}

######################################################
# alternative: "two.sided"
# Decision: H0
# var-test
output <- ht_1pop_var(amostra, sigma = sd0_0, alternative = "two.sided", conf_level = conf_level, sig_level = sig_level, na.rm = TRUE)
testthat::test_that("var-test bilateral, decision H0", {
  testthat::expect_equal(output$statistic[1], var_test(x0, alternative = 'two.sided', conf_level = conf_level, sigma = sd0_0)$statistic)
  testthat::expect_equal(output$p_value[1], var_test(x0, alternative = 'two.sided', conf_level = conf_level, sigma = sd0_0)$p_value)
  testthat::expect_equal(output$lower_ci[1], var_test(x0, alternative = 'two.sided', conf_level = conf_level, sigma = sd0_0)$lower_ci)
  testthat::expect_equal(output$upper_ci[1], var_test(x0, alternative = 'two.sided', conf_level = conf_level, sigma = sd0_0)$upper_ci)
  testthat::expect_equal(output$critical_value[1], qchisq(sig_level / 2, df = n0 - 1))
  testthat::expect_equal(output$critical_value[2], qchisq(1 - sig_level / 2, df = n0 - 1))
})

######################################################
# alternative: "less"
# Decision: H0
# var-test
output <- ht_1pop_var(amostra, sigma = sd0_0, alternative = "less", conf_level = conf_level, sig_level = sig_level, na.rm = TRUE)
testthat::test_that("var-test bilateral, decision H0", {
  testthat::expect_equal(output$statistic[1], var_test(x0, alternative = 'less', conf_level = conf_level, sigma = sd0_0)$statistic)
  testthat::expect_equal(output$p_value[1], var_test(x0, alternative = 'less', conf_level = conf_level, sigma = sd0_0)$p_value)
  testthat::expect_equal(output$lower_ci[1], var_test(x0, alternative = 'less', conf_level = conf_level, sigma = sd0_0)$lower_ci)
  testthat::expect_equal(output$upper_ci[1], var_test(x0, alternative = 'less', conf_level = conf_level, sigma = sd0_0)$upper_ci)
  testthat::expect_equal(output$critical_value[1], qchisq(sig_level, df = n0 - 1))
})

######################################################
# alternative: "greater"
# Decision: H0
# var-test
output <- ht_1pop_var(amostra, sigma = sd0_0, alternative = "greater", conf_level = conf_level, sig_level = sig_level, na.rm = TRUE)
testthat::test_that("var-test bilateral, decision H0", {
  testthat::expect_equal(output$statistic[1], var_test(x0, alternative = 'greater', conf_level = conf_level, sigma = sd0_0)$statistic)
  testthat::expect_equal(output$p_value[1], var_test(x0, alternative = 'greater', conf_level = conf_level, sigma = sd0_0)$p_value)
  testthat::expect_equal(output$lower_ci[1], var_test(x0, alternative = 'greater', conf_level = conf_level, sigma = sd0_0)$lower_ci)
  testthat::expect_equal(output$upper_ci[1], var_test(x0, alternative = 'greater', conf_level = conf_level, sigma = sd0_0)$upper_ci)
  testthat::expect_equal(output$critical_value[1], qchisq(1 - sig_level, df = n0 - 1))
})

######################################################
# alternative: "two.sided"
# Decision: H1
# var-test
output <- ht_1pop_var(amostra, sigma = sd0_1, alternative = "two.sided", conf_level = conf_level, sig_level = sig_level, na.rm = TRUE)
testthat::test_that("var-test bilateral, decision H0", {
  testthat::expect_equal(output$statistic[1], var_test(x0, alternative = 'two.sided', conf_level = conf_level, sigma = sd0_1)$statistic)
  testthat::expect_equal(output$p_value[1], var_test(x0, alternative = 'two.sided', conf_level = conf_level, sigma = sd0_1)$p_value)
  testthat::expect_equal(output$lower_ci[1], var_test(x0, alternative = 'two.sided', conf_level = conf_level, sigma = sd0_1)$lower_ci)
  testthat::expect_equal(output$upper_ci[1], var_test(x0, alternative = 'two.sided', conf_level = conf_level, sigma = sd0_1)$upper_ci)
  testthat::expect_equal(output$critical_value[1], qchisq(sig_level / 2, df = n0 - 1))
  testthat::expect_equal(output$critical_value[2], qchisq(1 - sig_level / 2, df = n0 - 1))
})

######################################################
# alternative: "less"
# Decision: H1
# var-test
output <- ht_1pop_var(amostra, sigma = sd0_1, alternative = "less", conf_level = conf_level, sig_level = sig_level, na.rm = TRUE)
testthat::test_that("var-test bilateral, decision H0", {
  testthat::expect_equal(output$statistic[1], var_test(x0, alternative = 'less', conf_level = conf_level, sigma = sd0_1)$statistic)
  testthat::expect_equal(output$p_value[1], var_test(x0, alternative = 'less', conf_level = conf_level, sigma = sd0_1)$p_value)
  testthat::expect_equal(output$lower_ci[1], var_test(x0, alternative = 'less', conf_level = conf_level, sigma = sd0_1)$lower_ci)
  testthat::expect_equal(output$upper_ci[1], var_test(x0, alternative = 'less', conf_level = conf_level, sigma = sd0_1)$upper_ci)
  testthat::expect_equal(output$critical_value[1], qchisq(sig_level, df = n0 - 1))
})

######################################################
# alternative: "greater"
# Decision: H1
# var-test
output <- ht_1pop_var(amostra, sigma = sd0_1, alternative = "greater", conf_level = conf_level, sig_level = sig_level, na.rm = TRUE)
testthat::test_that("var-test bilateral, decision H0", {
  testthat::expect_equal(output$statistic[1], var_test(x0, alternative = 'greater', conf_level = conf_level, sigma = sd0_1)$statistic)
  testthat::expect_equal(output$p_value[1], var_test(x0, alternative = 'greater', conf_level = conf_level, sigma = sd0_1)$p_value)
  testthat::expect_equal(output$lower_ci[1], var_test(x0, alternative = 'greater', conf_level = conf_level, sigma = sd0_1)$lower_ci)
  testthat::expect_equal(output$upper_ci[1], var_test(x0, alternative = 'greater', conf_level = conf_level, sigma = sd0_1)$upper_ci)
  testthat::expect_equal(output$critical_value[1], qchisq(1 - sig_level, df = n0 - 1))
})
