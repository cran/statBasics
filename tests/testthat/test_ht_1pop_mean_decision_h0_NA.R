amostra <- c(7.71, 10.69, 10.66,  9.14,  9.78,  9.39,  8.12, 10.29,  8.79, 11.31, NA, NA)
x0 <- amostra[!is.na(amostra)]
size  <- length(amostra)
n0 <- length(x0)
sd_pop <- 1.5
mu0_1  <- -10 # decision H1
mu0_0 <- 10 # decision H0
sig_level <- 0.01
conf_level <- 0.95

######################################################
# alternative: "two.sided"
# Decision: H0
# t-test
output <- ht_1pop_mean(amostra, mu = mu0_0, sig_level = sig_level, conf_level = conf_level, na.rm = TRUE)
testthat::test_that("t-test bilateral, decision H0",{
    testthat::expect_equal(output$statistic, base::unname(stats::t.test(x0, mu = mu0_0, conf.level = conf_level)$statistic))
    testthat::expect_equal(output$p_value, base::unname(stats::t.test(x0, mu = mu0_0, conf.level = conf_level)$p.value))
    testthat::expect_equal(output$lower_ci, ci_norm(amostra, conf_level = conf_level, na.rm = TRUE)$lower_ci)
    testthat::expect_equal(output$upper_ci, ci_norm(amostra, conf_level = conf_level, na.rm = TRUE)$upper_ci)
    testthat::expect_equal(output$critical_value, qt(1 - sig_level / 2, df = n0 - 1))
})

######################################################
# alternative: "less"
# Decision: H0
# t-test
output <- ht_1pop_mean(amostra, mu = mu0_0, sig_level = sig_level, conf_level = conf_level, alternative = "less")
testthat::test_that("t-test less, decision H0",{
    testthat::expect_equal(output$statistic, base::unname(stats::t.test(x0, mu = mu0_0, conf.level = conf_level, alternative = "less")$statistic))
    testthat::expect_equal(output$p_value, base::unname(stats::t.test(x0, mu = mu0_0, conf.level = conf_level, alternative = "less")$p.value))
    testthat::expect_equal(output$lower_ci, ci_norm(amostra, conf_level = conf_level, na.rm = TRUE)$lower_ci)
    testthat::expect_equal(output$upper_ci, ci_norm(amostra, conf_level = conf_level, na.rm = TRUE)$upper_ci)
    testthat::expect_equal(output$critical_value, qt(sig_level, df = n0 - 1))
})

######################################################
# alternative: "greater"
# Decision: H0
# t-test
output <- ht_1pop_mean(amostra, mu = mu0_0, sig_level = sig_level, conf_level = conf_level, alternative = "greater")
testthat::test_that("t-test greater, decision H0",{
    testthat::expect_equal(output$statistic, base::unname(stats::t.test(x0, mu = mu0_0, conf.level = conf_level, alternative = "greater")$statistic))
    testthat::expect_equal(output$p_value, base::unname(stats::t.test(x0, mu = mu0_0, conf.level = conf_level, alternative = "greater")$p.value))
    testthat::expect_equal(output$lower_ci, ci_norm(amostra, conf_level = conf_level, na.rm = TRUE)$lower_ci)
    testthat::expect_equal(output$upper_ci, ci_norm(amostra, conf_level = conf_level, na.rm = TRUE)$upper_ci)
    testthat::expect_equal(output$critical_value, qt(1 - sig_level, df = n0 - 1))
})

######################################################
# alternative: "two.sided"
# Decision: H0
# z-test
output <- ht_1pop_mean(amostra, mu = mu0_0, sig_level = sig_level, conf_level = conf_level, sd_pop = sd_pop)
testthat::test_that("z-test bilateral, decision H0",{
    testthat::expect_equal(output$statistic, base::unname(BSDA::z.test(x0, mu = mu0_0, conf.level = conf_level, sigma.x = sd_pop)$statistic))
    testthat::expect_equal(output$p_value, base::unname(BSDA::z.test(x0, mu = mu0_0, conf.level = conf_level, sigma.x = sd_pop)$p.value))
    testthat::expect_equal(output$lower_ci, ci_norm(amostra, conf_level = conf_level, sd_pop = sd_pop, na.rm = TRUE)$lower_ci)
    testthat::expect_equal(output$upper_ci, ci_norm(amostra, conf_level = conf_level, sd_pop = sd_pop, na.rm = TRUE)$upper_ci)
    testthat::expect_equal(output$critical_value, qnorm(1 - sig_level / 2))
})

######################################################
# alternative: "less"
# Decision: H0
# t-test
output <- ht_1pop_mean(amostra, mu = mu0_0, sig_level = sig_level, conf_level = conf_level, alternative = "less", sd_pop = sd_pop)
testthat::test_that("t-test less, decision H0",{
    testthat::expect_equal(output$statistic, base::unname(BSDA::z.test(x0, mu = mu0_0, conf.level = conf_level, alternative = "less", sigma.x = sd_pop)$statistic))
    testthat::expect_equal(output$p_value, base::unname(BSDA::z.test(x0, mu = mu0_0, conf.level = conf_level, alternative = "less", sigma.x = sd_pop)$p.value))
    testthat::expect_equal(output$lower_ci, ci_norm(amostra, conf_level = conf_level, sd_pop = sd_pop, na.rm = TRUE)$lower_ci)
    testthat::expect_equal(output$upper_ci, ci_norm(amostra, conf_level = conf_level, sd_pop = sd_pop, na.rm = TRUE)$upper_ci)
    testthat::expect_equal(output$critical_value, qnorm(sig_level))
})

######################################################
# alternative: "greater"
# Decision: H0
# t-test
output <- ht_1pop_mean(amostra, mu = mu0_0, sig_level = sig_level, conf_level = conf_level, alternative = "greater", sd_pop = sd_pop)
testthat::test_that("t-test greater, decision H0",{
    testthat::expect_equal(output$statistic, base::unname(BSDA::z.test(x0, mu = mu0_0, conf.level = conf_level, alternative = "greater", sigma.x = sd_pop)$statistic))
    testthat::expect_equal(output$p_value, base::unname(BSDA::z.test(x0, mu = mu0_0, conf.level = conf_level, alternative = "greater", sigma.x = sd_pop)$p.value))
    testthat::expect_equal(output$lower_ci, ci_norm(amostra, conf_level = conf_level, sd_pop = sd_pop, na.rm = TRUE)$lower_ci)
    testthat::expect_equal(output$upper_ci, ci_norm(amostra, conf_level = conf_level, sd_pop = sd_pop, na.rm = TRUE)$upper_ci)
    testthat::expect_equal(output$critical_value, qnorm(1 - sig_level))
})
