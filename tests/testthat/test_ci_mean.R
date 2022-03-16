size <- 1000
mean_pop <- 15
x <- rpois(size, lambda = size)

# lower <- function(conf_level, type = 'two.sided') {
#   if (type == 'two.sided') {
#     return(2 * size * mean(x) / qchisq((1 - conf_level) / 2, df = 2 * size))
#   } else if (type == 'left') {
#     return(2 * size * mean(x) / qchisq(conf_level, df = 2 * size))
#   } else if (type == 'right') {
#     return(0)
#   }
# }

# upper <- function(conf_level, type = 'two.sided') {
#   if (type == 'two.sided') {
#     return(2 * size * mean(x) / qchisq((1 + conf_level) / 2, df = 2 * size ))
#   } else if (type == 'left') {
#      return(Inf)
#   } else if (type == 'right') {
#       return(2 * size * mean(x) / qchisq(1 - conf_level, df = 2 * size))
#   }
# }

###############################################################################
# type = 'two.sided'

testthat::test_that("Comparing, type = 'two.sided', conf_level = '90%'", {
  testthat::expect_equal(ci_general(x, conf_level = 0.90)$lower_ci, stats::t.test(x, alternative = 'two.sided', conf.level = 0.90)$conf.int[1])
  testthat::expect_equal(ci_general(x, conf_level = 0.90)$upper_ci, stats::t.test(x, alternative = 'two.sided', conf.level = 0.90)$conf.int[2])
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '95%'", {
  testthat::expect_equal(ci_general(x, conf_level = 0.95)$lower_ci, stats::t.test(x, alternative = 'two.sided', conf.level = 0.95)$conf.int[1])
  testthat::expect_equal(ci_general(x, conf_level = 0.95)$upper_ci, stats::t.test(x, alternative = 'two.sided', conf.level = 0.95)$conf.int[2])
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '99%'", {
  testthat::expect_equal(ci_general(x, conf_level = 0.99)$lower_ci, stats::t.test(x, alternative = 'two.sided', conf.level = 0.99)$conf.int[1])
  testthat::expect_equal(ci_general(x, conf_level = 0.99)$upper_ci, stats::t.test(x, alternative = 'two.sided', conf.level = 0.99)$conf.int[2])
})

###############################################################################
# type = 'left'

testthat::test_that("Comparing, type = 'left', conf_level = '90%'", {
  testthat::expect_equal(ci_general(x, conf_level = 0.90, type = 'left')$lower_ci, stats::t.test(x, alternative = 'greater', conf.level = 0.90)$conf.int[1])
  testthat::expect_equal(ci_general(x, conf_level = 0.90, type = 'left')$upper_ci, stats::t.test(x, alternative = 'greater', conf.level = 0.90)$conf.int[2])
})

testthat::test_that("Comparing, type = 'left', conf_level = '95%'", {
  testthat::expect_equal(ci_general(x, conf_level = 0.95, type = 'left')$lower_ci, stats::t.test(x, alternative = 'greater', conf.level = 0.95)$conf.int[1])
  testthat::expect_equal(ci_general(x, conf_level = 0.95, type = 'left')$upper_ci, stats::t.test(x, alternative = 'greater', conf.level = 0.95)$conf.int[2])
})

testthat::test_that("Comparing, type = 'left', conf_level = '99%'", {
  testthat::expect_equal(ci_general(x, conf_level = 0.99, type = 'left')$lower_ci, stats::t.test(x, alternative = 'greater', conf.level = 0.99)$conf.int[1])
  testthat::expect_equal(ci_general(x, conf_level = 0.99, type = 'left')$upper_ci, stats::t.test(x, alternative = 'greater', conf.level = 0.99)$conf.int[2])
})

###############################################################################
# type = 'right'

testthat::test_that("Comparing, type = 'right', conf_level = '90%'", {
  testthat::expect_equal(ci_general(x, conf_level = 0.90, type = 'right')$lower_ci, stats::t.test(x, alternative = 'less', conf.level = 0.90)$conf.int[1])
  testthat::expect_equal(ci_general(x, conf_level = 0.90, type = 'right')$upper_ci, stats::t.test(x, alternative = 'less', conf.level = 0.90)$conf.int[2])
})

testthat::test_that("Comparing, type = 'right', conf_level = '95%'", {
  testthat::expect_equal(ci_general(x, conf_level = 0.95, type = 'right')$lower_ci, stats::t.test(x, alternative = 'less', conf.level = 0.95)$conf.int[1])
  testthat::expect_equal(ci_general(x, conf_level = 0.95, type = 'right')$upper_ci, stats::t.test(x, alternative = 'less', conf.level = 0.95)$conf.int[2])
})

testthat::test_that("Comparing, type = 'right', conf_level = '99%'", {
  testthat::expect_equal(ci_general(x, conf_level = 0.99, type = 'right')$lower_ci, stats::t.test(x, alternative = 'less', conf.level = 0.99)$conf.int[1])
  testthat::expect_equal(ci_general(x, conf_level = 0.99, type = 'right')$upper_ci, stats::t.test(x, alternative = 'less', conf.level = 0.99)$conf.int[2])
})
