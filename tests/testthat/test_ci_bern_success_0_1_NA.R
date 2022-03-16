n <- 100
x <- c(rbinom(n, 1, prob = 0.75), NA)


testthat::test_that("Number of sucess and confidence level 90% - type = 'two.sided'", {
    testthat::expect_equal(ci_bern(x, conf_level = 0.90, na.rm = T)$lower_ci, qnorm(0.05) / (2 * sqrt(n)) + mean(x, na.rm = T))
    testthat::expect_equal(ci_bern(x, conf_level = 0.90, na.rm = T)$upper_ci, qnorm(0.95) / (2 * sqrt(n)) + mean(x, na.rm = T))
})

testthat::test_that("Number of sucess and confidence level 90% - type = 'left'", {
    testthat::expect_equal(ci_bern(x, conf_level = 0.90, type = 'left', na.rm = T)$lower_ci, qnorm(0.1) / (2 * sqrt(n)) + mean(x, na.rm = T))
    testthat::expect_equal(ci_bern(x, conf_level = 0.90, type = 'left', na.rm = T)$upper_ci, 1)
})

testthat::test_that("Number of sucess and confidence level 90% - type = 'right'", {
    testthat::expect_equal(ci_bern(x, conf_level = 0.90, type = 'right', na.rm = T)$lower_ci, 0)
    testthat::expect_equal(ci_bern(x, conf_level = 0.90, type = 'right', na.rm = T)$upper_ci, qnorm(0.9) / (2 * sqrt(n)) + mean(x, na.rm = T))
})

#################################################################################################
# two.sided, and number of sucess
# conf_level = 95%

testthat::test_that("Number of sucess and confidence level 95% - type = 'two.sided'", {
    testthat::expect_equal(ci_bern(x, conf_level = 0.95, na.rm = T)$lower_ci, qnorm(0.025) / (2 * sqrt(n)) + mean(x, na.rm = T))
    testthat::expect_equal(ci_bern(x, conf_level = 0.95, na.rm = T)$upper_ci, qnorm(0.975) / (2 * sqrt(n)) + mean(x, na.rm = T))
})

testthat::test_that("Number of sucess and confidence level 95% - type = 'left'", {
    testthat::expect_equal(ci_bern(x, conf_level = 0.95, type = 'left', na.rm = T)$lower_ci, qnorm(0.05) / (2 * sqrt(n)) + mean(x, na.rm = T))
    testthat::expect_equal(ci_bern(x, conf_level = 0.95, type = 'left', na.rm = T)$upper_ci, 1)
})

testthat::test_that("Number of sucess and confidence level 95% - type = 'right'", {
    testthat::expect_equal(ci_bern(x, conf_level = 0.95, type = 'right', na.rm = T)$lower_ci, 0)
    testthat::expect_equal(ci_bern(x, conf_level = 0.95, type = 'right', na.rm = T)$upper_ci, qnorm(0.95) / (2 * sqrt(n)) + mean(x, na.rm = T))
})

#################################################################################################
# two.sided, and number of sucess
# conf_level = 99%

testthat::test_that("Number of sucess and confidence level 99% - type = 'two.sided'", {
    testthat::expect_equal(ci_bern(x, conf_level = 0.99, na.rm = T)$lower_ci, qnorm(0.005) / (2 * sqrt(n)) + mean(x, na.rm = T))
    testthat::expect_equal(ci_bern(x, conf_level = 0.99, na.rm = T)$upper_ci, qnorm(0.995) / (2 * sqrt(n)) + mean(x, na.rm = T))
})

testthat::test_that("Number of sucess and confidence level 99% - type = 'left'", {
    testthat::expect_equal(ci_bern(x, conf_level = 0.99, type = 'left', na.rm = T)$lower_ci, qnorm(0.01) / (2 * sqrt(n)) + mean(x, na.rm = T))
    testthat::expect_equal(ci_bern(x, conf_level = 0.99, type = 'left', na.rm = T)$upper_ci, 1)
})

testthat::test_that("Number of sucess and confidence level 99% - type = 'right'", {
    testthat::expect_equal(ci_bern(x, conf_level = 0.99, type = 'right', na.rm = T)$lower_ci, 0)
    testthat::expect_equal(ci_bern(x, conf_level = 0.99, type = 'right', na.rm = T)$upper_ci, qnorm(0.99) / (2 * sqrt(n)) + mean(x, na.rm = T))
})
