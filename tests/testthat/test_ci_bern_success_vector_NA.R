x0 <- c(2, 3, 5, NA, 5)
n0 <- c(10, 8, 6, 20, NA)
x1  <- c(2, 3, 5)
n1 <- c(10, 8, 6)

testthat::test_that("Number of sucess and confidence level 90% - type = 'two.sided'", {
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.90, na.rm = T)$lower_ci, qnorm(0.05) / (2 * sqrt(sum(n1, na.rm = T))) + sum(x1, na.rm = T) / sum(n1, na.rm = T))
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.90, na.rm = T)$upper_ci, qnorm(0.95) / (2 * sqrt(sum(n1, na.rm = T))) + sum(x1, na.rm = T) / sum(n1, na.rm = T))
})

testthat::test_that("Number of sucess and confidence level 90% - type = 'left'", {
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.90, type = 'left', na.rm = T)$lower_ci, qnorm(0.1) / (2 * sqrt(sum(n1, na.rm = T))) + sum(x1, na.rm = T) / sum(n1, na.rm = T))
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.90, type = 'left', na.rm = T)$upper_ci, 1)
})

testthat::test_that("Number of sucess and confidence level 90% - type = 'right'", {
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.90, type = 'right', na.rm = T)$lower_ci, 0)
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.90, type = 'right', na.rm = T)$upper_ci, qnorm(0.9) / (2 * sqrt(sum(n1, na.rm = T))) + sum(x1, na.rm = T) / sum(n1, na.rm = T))
})

#################################################################################################
# two.sided, and number of sucess
# conf_level = 95%

testthat::test_that("Number of sucess and confidence level 95% - type = 'two.sided'", {
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.95, na.rm = T)$lower_ci, qnorm(0.025) / (2 * sqrt(sum(n1, na.rm = T))) + sum(x1, na.rm = T) / sum(n1, na.rm = T))
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.95, na.rm = T)$upper_ci, qnorm(0.975) / (2 * sqrt(sum(n1, na.rm = T))) + sum(x1, na.rm = T) / sum(n1, na.rm = T))
})

testthat::test_that("Number of sucess and confidence level 95% - type = 'left'", {
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.95, type = 'left', na.rm = T)$lower_ci, qnorm(0.05) / (2 * sqrt(sum(n1, na.rm = T))) + sum(x1, na.rm = T) / sum(n1, na.rm = T))
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.95, type = 'left', na.rm = T)$upper_ci, 1)
})

testthat::test_that("Number of sucess and confidence level 95% - type = 'right'", {
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.95, type = 'right', na.rm = T)$lower_ci, 0)
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.95, type = 'right', na.rm = T)$upper_ci, qnorm(0.95) / (2 * sqrt(sum(n1, na.rm = T))) + sum(x1, na.rm = T) / sum(n1, na.rm = T))
})

#################################################################################################
# two.sided, and number of sucess
# conf_level = 99%

testthat::test_that("Number of sucess and confidence level 99% - type = 'two.sided'", {
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.99, na.rm = T)$lower_ci, qnorm(0.005) / (2 * sqrt(sum(n1, na.rm = T))) + sum(x1, na.rm = T) / sum(n1, na.rm = T))
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.99, na.rm = T)$upper_ci, qnorm(0.995) / (2 * sqrt(sum(n1, na.rm = T))) + sum(x1, na.rm = T) / sum(n1, na.rm = T))
})

testthat::test_that("Number of sucess and confidence level 99% - type = 'left'", {
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.99, type = 'left', na.rm = T)$lower_ci, qnorm(0.01) / (2 * sqrt(sum(n1, na.rm = T))) + sum(x1, na.rm = T) / sum(n1, na.rm = T))
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.99, type = 'left', na.rm = T)$upper_ci, 1)
})

testthat::test_that("Number of sucess and confidence level 99% - type = 'right'", {
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.99, type = 'right', na.rm = T)$lower_ci, 0)
    testthat::expect_equal(ci_bern(x0, n0, conf_level = 0.99, type = 'right', na.rm = T)$upper_ci, qnorm(0.99) / (2 * sqrt(sum(n1, na.rm = T))) + sum(x1, na.rm = T) / sum(n1, na.rm = T))
})
