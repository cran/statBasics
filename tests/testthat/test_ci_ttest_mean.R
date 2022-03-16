size <- 75
mean_pop <- 3
sd_pop <- 2
x <- rnorm(size, mean = mean_pop, sd = sd_pop)

###############################################################################
# type = 'two.sided'

lower <- function(conf_level, type = 'two.sided') {
  if (type == 'two.sided') {
    return(qt((1 - conf_level) / 2, df = size - 1) * sd(x) / sqrt(size) + mean(x))
  } else if (type == 'left') {
    return(qt(1 - conf_level, df = size - 1) * sd(x) / sqrt(size) + mean(x))
  } else if (type == 'right') {
    return(-Inf)
  }
}

upper <- function(conf_level, type = 'two.sided') {
  if (type == 'two.sided') {
    return(qt((1 + conf_level) / 2, df = size - 1) * sd(x) / sqrt(size) + mean(x))
  } else if (type == 'left') {
     return(Inf)
  } else if (type == 'right') {
      return(qt(conf_level, df = size - 1) * sd(x) / sqrt(size) + mean(x))
  }
}

testthat::test_that("Comparing, type = 'two.sided', conf_level = '90%'", {
  testthat::expect_equal(ci_norm(x, conf_level = 0.90)$lower_ci, lower(0.90))
  testthat::expect_equal(ci_norm(x, conf_level = 0.90)$upper_ci, upper(0.90))
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '95%'", {
  testthat::expect_equal(ci_norm(x, conf_level = 0.95)$lower_ci, lower(0.95))
  testthat::expect_equal(ci_norm(x, conf_level = 0.95)$upper_ci, upper(0.95))
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '99%'", {
  testthat::expect_equal(ci_norm(x, conf_level = 0.99)$lower_ci, lower(0.99))
  testthat::expect_equal(ci_norm(x, conf_level = 0.99)$upper_ci, upper(0.99))
})

###############################################################################
# type = 'left'

testthat::test_that("Comparing, type = 'left', conf_level = '90%'", {
  testthat::expect_equal(ci_norm(x, conf_level = 0.90, type = 'left')$lower_ci, lower(0.90, type = 'left'))
  testthat::expect_equal(ci_norm(x, conf_level = 0.90, type = 'left')$upper_ci, upper(0.90, type = 'left'))
})

testthat::test_that("Comparing, type = 'two.sides', conf_level = '95%'", {
  testthat::expect_equal(ci_norm(x, conf_level = 0.95, type = 'left')$lower_ci, lower(0.95, type = 'left'))
  testthat::expect_equal(ci_norm(x, conf_level = 0.95, type = 'left')$upper_ci, upper(0.95, type = 'left'))
})

testthat::test_that("Comparing, type = 'two.sides', conf_level = '99%'", {
  testthat::expect_equal(ci_norm(x, conf_level = 0.99, type = 'left')$lower_ci, lower(0.99, type = 'left'))
  testthat::expect_equal(ci_norm(x, conf_level = 0.99, type = 'left')$upper_ci, upper(0.99, type = 'left'))
})

###############################################################################
# type = 'right'

testthat::test_that("Comparing, type = 'right', conf_level = '90%'", {
  testthat::expect_equal(ci_norm(x, conf_level = 0.90, type = 'right')$lower_ci, lower(0.90, type = 'right'))
  testthat::expect_equal(ci_norm(x, conf_level = 0.90, type = 'right')$upper_ci, upper(0.90, type = 'right'))
})

testthat::test_that("Comparing, type = 'left', conf_level = '95%'", {
  testthat::expect_equal(ci_norm(x, conf_level = 0.95, type = 'right')$lower_ci, lower(0.95, type = 'right'))
  testthat::expect_equal(ci_norm(x, conf_level = 0.95, type = 'right')$upper_ci, upper(0.95, type = 'right'))
})

testthat::test_that("Comparing, type = 'right', conf_level = '99%'", {
  testthat::expect_equal(ci_norm(x, conf_level = 0.99, type = 'right')$lower_ci, lower(0.99, type = 'right'))
  testthat::expect_equal(ci_norm(x, conf_level = 0.99, type = 'right')$upper_ci, upper(0.99, type = 'right'))
})
