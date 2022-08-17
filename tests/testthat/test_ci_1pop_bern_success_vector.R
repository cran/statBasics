x <- c(2, 3, 5)
n <- c(10, 8, 6)

testthat::test_that("Number of sucess and confidence level 90% - type = 'two.sided'", {
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.90)$lower_ci, qnorm(0.05) / (2 * sqrt(sum(n))) + sum(x) / sum(n))
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.90)$upper_ci, qnorm(0.95) / (2 * sqrt(sum(n))) + sum(x) / sum(n))
})

testthat::test_that("Number of sucess and confidence level 90% - type = 'left'", {
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.90, type = 'left')$lower_ci, qnorm(0.1) / (2 * sqrt(sum(n))) + sum(x) / sum(n))
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.90, type = 'left')$upper_ci, 1)
})

testthat::test_that("Number of sucess and confidence level 90% - type = 'right'", {
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.90, type = 'right')$lower_ci, 0)
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.90, type = 'right')$upper_ci, qnorm(0.9) / (2 * sqrt(sum(n))) + sum(x) / sum(n))
})

#################################################################################################
# two.sided, and number of sucess
# conf_level = 95%

testthat::test_that("Number of sucess and confidence level 95% - type = 'two.sided'", {
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.95)$lower_ci, qnorm(0.025) / (2 * sqrt(sum(n))) + sum(x) / sum(n))
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.95)$upper_ci, qnorm(0.975) / (2 * sqrt(sum(n))) + sum(x) / sum(n))
})

testthat::test_that("Number of sucess and confidence level 95% - type = 'left'", {
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.95, type = 'left')$lower_ci, qnorm(0.05) / (2 * sqrt(sum(n))) + sum(x) / sum(n))
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.95, type = 'left')$upper_ci, 1)
})

testthat::test_that("Number of sucess and confidence level 95% - type = 'right'", {
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.95, type = 'right')$lower_ci, 0)
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.95, type = 'right')$upper_ci, qnorm(0.95) / (2 * sqrt(sum(n))) + sum(x) / sum(n))
})

#################################################################################################
# two.sided, and number of sucess
# conf_level = 99%

testthat::test_that("Number of sucess and confidence level 99% - type = 'two.sided'", {
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.99)$lower_ci, qnorm(0.005) / (2 * sqrt(sum(n))) + sum(x) / sum(n))
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.99)$upper_ci, qnorm(0.995) / (2 * sqrt(sum(n))) + sum(x) / sum(n))
})

testthat::test_that("Number of sucess and confidence level 99% - type = 'left'", {
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.99, type = 'left')$lower_ci, qnorm(0.01) / (2 * sqrt(sum(n))) + sum(x) / sum(n))
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.99, type = 'left')$upper_ci, 1)
})

testthat::test_that("Number of sucess and confidence level 99% - type = 'right'", {
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.99, type = 'right')$lower_ci, 0)
    testthat::expect_equal(ci_1pop_bern(x, n, conf_level = 0.99, type = 'right')$upper_ci, qnorm(0.99) / (2 * sqrt(sum(n))) + sum(x) / sum(n))
})
