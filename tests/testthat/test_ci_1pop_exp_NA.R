size <- 1000
mean_pop <- 8000
x0 <- c(rexp(size, rate = 1 / mean_pop), NA)
x1 <- x0[!is.na(x0)]


lower <- function(conf_level, type = 'two.sided') {
  if (type == 'two.sided') {
    return(2 * size * mean(x1) / qchisq((1 + conf_level) / 2, df = 2 * size))
  } else if (type == 'left') {
    return(2 * size * mean(x1) / qchisq(conf_level, df = 2 * size))
  } else if (type == 'right') {
    return(0)
  }
}

upper <- function(conf_level, type = 'two.sided') {
  if (type == 'two.sided') {
    return(2 * size * mean(x1) / qchisq((1 - conf_level) / 2, df = 2 * size ))
  } else if (type == 'left') {
     return(Inf)
  } else if (type == 'right') {
      return(2 * size * mean(x1) / qchisq(1 - conf_level, df = 2 * size))
  }
}

###############################################################################
# type = 'two.sided'

testthat::test_that("Comparing, type = 'two.sided', conf_level = '90%'", {
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.90, na.rm = T)$lower_ci, lower(conf_level = 0.90))
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.90, na.rm = T)$upper_ci, upper(conf_level = 0.90))
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '95%'", {
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.95, na.rm = T)$lower_ci, lower(conf_level = 0.95))
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.95, na.rm = T)$upper_ci, upper(conf_level = 0.95))
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '99%'", {
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.99, na.rm = T)$lower_ci, lower(conf_level = 0.99))
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.99, na.rm = T)$upper_ci, upper(conf_level = 0.99))
})

###############################################################################
# type = 'left'

testthat::test_that("Comparing, type = 'left', conf_level = '90%'", {
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.90, type = 'left', na.rm = T)$lower_ci, lower(conf_level = 0.90, type = 'left'))
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.90, type = 'left', na.rm = T)$upper_ci, upper(conf_level = 0.90, type = 'left'))
})

testthat::test_that("Comparing, type = 'left', conf_level = '95%'", {
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.95, type = 'left', na.rm = T)$lower_ci, lower(conf_level = 0.95, type = 'left'))
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.95, type = 'left', na.rm = T)$upper_ci, upper(conf_level = 0.95, type = 'left'))
})

testthat::test_that("Comparing, type = 'left', conf_level = '99%'", {
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.99, type = 'left', na.rm = T)$lower_ci, lower(conf_level = 0.99, type = 'left'))
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.99, type = 'left', na.rm = T)$upper_ci, upper(conf_level = 0.99, type = 'left'))
})

###############################################################################
# type = 'right'

testthat::test_that("Comparing, type = 'right', conf_level = '90%'", {
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.90, type = 'right', na.rm = T)$lower_ci, lower(conf_level = 0.90, type = 'right'))
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.90, type = 'right', na.rm = T)$upper_ci, upper(conf_level = 0.90, type = 'right'))
})

testthat::test_that("Comparing, type = 'right', conf_level = '95%'", {
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.95, type = 'right', na.rm = T)$lower_ci, lower(conf_level = 0.95, type = 'right'))
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.95, type = 'right', na.rm = T)$upper_ci, upper(conf_level = 0.95, type = 'right'))
})

testthat::test_that("Comparing, type = 'right', conf_level = '99%'", {
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.99, type = 'right', na.rm = T)$lower_ci, lower(conf_level = 0.99, type = 'right'))
  testthat::expect_equal(ci_1pop_exp(x0, conf_level = 0.99, type = 'right', na.rm = T)$upper_ci, upper(conf_level = 0.99, type = 'right'))
})
