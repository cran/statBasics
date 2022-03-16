size <- 1000
mean_pop <- 8000
x <- rexp(size, rate = 1 / mean_pop)


lower <- function(conf_level, type = 'two.sided') {
  if (type == 'two.sided') {
    return(2 * size * mean(x) / qchisq((1 + conf_level) / 2, df = 2 * size))
  } else if (type == 'left') {
    return(2 * size * mean(x) / qchisq(conf_level, df = 2 * size))
  } else if (type == 'right') {
    return(0)
  }
}

upper <- function(conf_level, type = 'two.sided') {
  if (type == 'two.sided') {
    return(2 * size * mean(x) / qchisq((1 - conf_level) / 2, df = 2 * size ))
  } else if (type == 'left') {
     return(Inf)
  } else if (type == 'right') {
      return(2 * size * mean(x) / qchisq(1 - conf_level, df = 2 * size))
  }
}

###############################################################################
# type = 'two.sided'

testthat::test_that("Comparing, type = 'two.sided', conf_level = '90%'", {
  testthat::expect_equal(ci_exp(x, conf_level = 0.90)$lower_ci, lower(conf_level = 0.90))
  testthat::expect_equal(ci_exp(x, conf_level = 0.90)$upper_ci, upper(conf_level = 0.90))
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '95%'", {
  testthat::expect_equal(ci_exp(x, conf_level = 0.95)$lower_ci, lower(conf_level = 0.95))
  testthat::expect_equal(ci_exp(x, conf_level = 0.95)$upper_ci, upper(conf_level = 0.95))
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '99%'", {
  testthat::expect_equal(ci_exp(x, conf_level = 0.99)$lower_ci, lower(conf_level = 0.99))
  testthat::expect_equal(ci_exp(x, conf_level = 0.99)$upper_ci, upper(conf_level = 0.99))
})

###############################################################################
# type = 'left'

testthat::test_that("Comparing, type = 'left', conf_level = '90%'", {
  testthat::expect_equal(ci_exp(x, conf_level = 0.90, type = 'left')$lower_ci, lower(conf_level = 0.90, type = 'left'))
  testthat::expect_equal(ci_exp(x, conf_level = 0.90, type = 'left')$upper_ci, upper(conf_level = 0.90, type = 'left'))
})

testthat::test_that("Comparing, type = 'left', conf_level = '95%'", {
  testthat::expect_equal(ci_exp(x, conf_level = 0.95, type = 'left')$lower_ci, lower(conf_level = 0.95, type = 'left'))
  testthat::expect_equal(ci_exp(x, conf_level = 0.95, type = 'left')$upper_ci, upper(conf_level = 0.95, type = 'left'))
})

testthat::test_that("Comparing, type = 'left', conf_level = '99%'", {
  testthat::expect_equal(ci_exp(x, conf_level = 0.99, type = 'left')$lower_ci, lower(conf_level = 0.99, type = 'left'))
  testthat::expect_equal(ci_exp(x, conf_level = 0.99, type = 'left')$upper_ci, upper(conf_level = 0.99, type = 'left'))
})

###############################################################################
# type = 'right'

testthat::test_that("Comparing, type = 'right', conf_level = '90%'", {
  testthat::expect_equal(ci_exp(x, conf_level = 0.90, type = 'right')$lower_ci, lower(conf_level = 0.90, type = 'right'))
  testthat::expect_equal(ci_exp(x, conf_level = 0.90, type = 'right')$upper_ci, upper(conf_level = 0.90, type = 'right'))
})

testthat::test_that("Comparing, type = 'right', conf_level = '95%'", {
  testthat::expect_equal(ci_exp(x, conf_level = 0.95, type = 'right')$lower_ci, lower(conf_level = 0.95, type = 'right'))
  testthat::expect_equal(ci_exp(x, conf_level = 0.95, type = 'right')$upper_ci, upper(conf_level = 0.95, type = 'right'))
})

testthat::test_that("Comparing, type = 'right', conf_level = '99%'", {
  testthat::expect_equal(ci_exp(x, conf_level = 0.99, type = 'right')$lower_ci, lower(conf_level = 0.99, type = 'right'))
  testthat::expect_equal(ci_exp(x, conf_level = 0.99, type = 'right')$upper_ci, upper(conf_level = 0.99, type = 'right'))
})
