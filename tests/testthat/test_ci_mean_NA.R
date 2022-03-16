size <- 1000
mean_pop <- 15
x0 <- c(rpois(size, lambda = size), NA)
x1 <- x0[!is.na(x0)]

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
  testthat::expect_equal(ci_general(x0, conf_level = 0.90, na.rm = T)$lower_ci, stats::t.test(x1, alternative = 'two.sided', conf.level = 0.90)$conf.int[1])
  testthat::expect_equal(ci_general(x0, conf_level = 0.90, na.rm = T)$upper_ci, stats::t.test(x1, alternative = 'two.sided', conf.level = 0.90)$conf.int[2])
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '95%'", {
  testthat::expect_equal(ci_general(x0, conf_level = 0.95, na.rm = T)$lower_ci, stats::t.test(x1, alternative = 'two.sided', conf.level = 0.95)$conf.int[1])
  testthat::expect_equal(ci_general(x0, conf_level = 0.95, na.rm = T)$upper_ci, stats::t.test(x1, alternative = 'two.sided', conf.level = 0.95)$conf.int[2])
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '99%'", {
  testthat::expect_equal(ci_general(x0, conf_level = 0.99, na.rm = T)$lower_ci, stats::t.test(x1, alternative = 'two.sided', conf.level = 0.99)$conf.int[1])
  testthat::expect_equal(ci_general(x0, conf_level = 0.99, na.rm = T)$upper_ci, stats::t.test(x1, alternative = 'two.sided', conf.level = 0.99)$conf.int[2])
})

###############################################################################
# type = 'left'

testthat::test_that("Comparing, type = 'left', conf_level = '90%'", {
  testthat::expect_equal(ci_general(x0, conf_level = 0.90, type = 'left', na.rm = T)$lower_ci, stats::t.test(x1, alternative = 'greater', conf.level = 0.90)$conf.int[1])
  testthat::expect_equal(ci_general(x0, conf_level = 0.90, type = 'left', na.rm = T)$upper_ci, stats::t.test(x1, alternative = 'greater', conf.level = 0.90)$conf.int[2])
})

testthat::test_that("Comparing, type = 'left', conf_level = '95%'", {
  testthat::expect_equal(ci_general(x0, conf_level = 0.95, type = 'left', na.rm = T)$lower_ci, stats::t.test(x1, alternative = 'greater', conf.level = 0.95)$conf.int[1])
  testthat::expect_equal(ci_general(x0, conf_level = 0.95, type = 'left', na.rm = T)$upper_ci, stats::t.test(x1, alternative = 'greater', conf.level = 0.95)$conf.int[2])
})

testthat::test_that("Comparing, type = 'left', conf_level = '99%'", {
  testthat::expect_equal(ci_general(x0, conf_level = 0.99, type = 'left', na.rm = T)$lower_ci, stats::t.test(x1, alternative = 'greater', conf.level = 0.99)$conf.int[1])
  testthat::expect_equal(ci_general(x0, conf_level = 0.99, type = 'left', na.rm = T)$upper_ci, stats::t.test(x1, alternative = 'greater', conf.level = 0.99)$conf.int[2])
})

###############################################################################
# type = 'right'

testthat::test_that("Comparing, type = 'right', conf_level = '90%'", {
  testthat::expect_equal(ci_general(x0, conf_level = 0.90, type = 'right', na.rm = T)$lower_ci, stats::t.test(x1, alternative = 'less', conf.level = 0.90)$conf.int[1])
  testthat::expect_equal(ci_general(x0, conf_level = 0.90, type = 'right', na.rm = T)$upper_ci, stats::t.test(x1, alternative = 'less', conf.level = 0.90)$conf.int[2])
})

testthat::test_that("Comparing, type = 'right', conf_level = '95%'", {
  testthat::expect_equal(ci_general(x0, conf_level = 0.95, type = 'right', na.rm = T)$lower_ci, stats::t.test(x1, alternative = 'less', conf.level = 0.95)$conf.int[1])
  testthat::expect_equal(ci_general(x0, conf_level = 0.95, type = 'right', na.rm = T)$upper_ci, stats::t.test(x1, alternative = 'less', conf.level = 0.95)$conf.int[2])
})

testthat::test_that("Comparing, type = 'right', conf_level = '99%'", {
  testthat::expect_equal(ci_general(x0, conf_level = 0.99, type = 'right', na.rm = T)$lower_ci, stats::t.test(x1, alternative = 'less', conf.level = 0.99)$conf.int[1])
  testthat::expect_equal(ci_general(x0, conf_level = 0.99, type = 'right', na.rm = T)$upper_ci, stats::t.test(x1, alternative = 'less', conf.level = 0.99)$conf.int[2])
})
