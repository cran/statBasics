size <- 75
mean_pop <- 3
sd_pop <- 2
x <- c(rnorm(size, mean = mean_pop, sd = sd_pop), NA)

###############################################################################
# type = 'two.sided'

lower <- function(conf_level, type = 'two.sided') {
  y <- x[!is.na(x)]
  if (type == 'two.sided') {
    return(qt((1 - conf_level) / 2, df = size - 1) * sd(y) / sqrt(size) + mean(y))
  } else if (type == 'left') {
    return(qt(1 - conf_level, df = size - 1) * sd(y) / sqrt(size) + mean(y))
  } else if (type == 'right') {
    return(-Inf)
  }
}

upper <- function(conf_level, type = 'two.sided') {
  y <- x[!is.na(x)]
  if (type == 'two.sided') {
    return(qt((1 + conf_level) / 2, df = size - 1) * sd(y) / sqrt(size) + mean(y))
  } else if (type == 'left') {
     return(Inf)
  } else if (type == 'right') {
      return(qt(conf_level, df = size - 1) * sd(y) / sqrt(size) + mean(y))
  }
}

testthat::test_that("Comparing, type = 'two.sided', conf_level = '90%'", {
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.90, na.rm = T)$lower_ci, lower(0.90))
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.90, na.rm = T)$upper_ci, upper(0.90))
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '95%'", {
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.95, na.rm = T)$lower_ci, lower(0.95))
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.95, na.rm = T)$upper_ci, upper(0.95))
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '99%'", {
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.99, na.rm = T)$lower_ci, lower(0.99))
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.99, na.rm = T)$upper_ci, upper(0.99))
})

###############################################################################
# type = 'left'

testthat::test_that("Comparing, type = 'left', conf_level = '90%'", {
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.90, type = 'left', na.rm = T)$lower_ci, lower(0.90, type = 'left'))
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.90, type = 'left', na.rm = T)$upper_ci, upper(0.90, type = 'left'))
})

testthat::test_that("Comparing, type = 'two.sides', conf_level = '95%'", {
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.95, type = 'left', na.rm = T)$lower_ci, lower(0.95, type = 'left'))
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.95, type = 'left', na.rm = T)$upper_ci, upper(0.95, type = 'left'))
})

testthat::test_that("Comparing, type = 'two.sides', conf_level = '99%'", {
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.99, type = 'left', na.rm = T)$lower_ci, lower(0.99, type = 'left'))
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.99, type = 'left', na.rm = T)$upper_ci, upper(0.99, type = 'left'))
})

###############################################################################
# type = 'right'

testthat::test_that("Comparing, type = 'right', conf_level = '90%'", {
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.90, type = 'right', na.rm = T)$lower_ci, lower(0.90, type = 'right'))
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.90, type = 'right', na.rm = T)$upper_ci, upper(0.90, type = 'right'))
})

testthat::test_that("Comparing, type = 'left', conf_level = '95%'", {
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.95, type = 'right', na.rm = T)$lower_ci, lower(0.95, type = 'right'))
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.95, type = 'right', na.rm = T)$upper_ci, upper(0.95, type = 'right'))
})

testthat::test_that("Comparing, type = 'right', conf_level = '99%'", {
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.99, type = 'right', na.rm = T)$lower_ci, lower(0.99, type = 'right'))
  testthat::expect_equal(ci_1pop_norm(x, conf_level = 0.99, type = 'right', na.rm = T)$upper_ci, upper(0.99, type = 'right'))
})
