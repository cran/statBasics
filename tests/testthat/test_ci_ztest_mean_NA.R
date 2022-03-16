size <- 1000
mean_pop <- 3
sd_pop <- 2
x <- c(rnorm(size, mean = mean_pop, sd = sd_pop), NA)

###############################################################################
# type = 'two.sided'

lower <- function(conf_level, type = 'two.sided') {
  y <- x[!is.na(x)]
  if (type == 'two.sided') {
    return(qnorm((1 - conf_level) / 2) * sd_pop / sqrt(size) + mean(y))
  } else if (type == 'left') {
    return(qnorm(1 - conf_level) * sd_pop / sqrt(size) + mean(y))
  } else if (type == 'right') {
    return(-Inf)
  }
}

upper <- function(conf_level, type = 'two.sided') {
  y <- x[!is.na(x)]
  if (type == 'two.sided') {
    return(qnorm((1 + conf_level) / 2) * sd_pop / sqrt(size) + mean(y))
  } else if (type == 'left') {
     return(Inf)
  } else if (type == 'right') {
      return(qnorm(conf_level) * sd_pop / sqrt(size) + mean(y))
  }
}

testthat::test_that("Comparing, type = 'two.sided', conf_level = '90%'", {
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.90, na.rm = T)$lower_ci, lower(0.90))
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.90, na.rm = T)$upper_ci, upper(0.90))
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '95%'", {
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.95, na.rm = T)$lower_ci, lower(0.95))
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.95, na.rm = T)$upper_ci, upper(0.95))
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '99%'", {
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.99, na.rm = T)$lower_ci, lower(0.99))
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.99, na.rm = T)$upper_ci, upper(0.99))
})

###############################################################################
# type = 'left'

testthat::test_that("Comparing, type = 'left', conf_level = '90%'", {
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.90, type = 'left', na.rm = T)$lower_ci, lower(0.90, type = 'left'))
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.90, type = 'left', na.rm = T)$upper_ci, upper(0.90, type = 'left'))
})

testthat::test_that("Comparing, type = 'two.sides', conf_level = '95%'", {
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.95, type = 'left', na.rm = T)$lower_ci, lower(0.95, type = 'left'))
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.95, type = 'left', na.rm = T)$upper_ci, upper(0.95, type = 'left'))
})

testthat::test_that("Comparing, type = 'two.sides', conf_level = '99%'", {
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.99, type = 'left', na.rm = T)$lower_ci, lower(0.99, type = 'left'))
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.99, type = 'left', na.rm = T)$upper_ci, upper(0.99, type = 'left'))
})

###############################################################################
# type = 'right'

testthat::test_that("Comparing, type = 'right', conf_level = '90%'", {
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.90, type = 'right', na.rm = T)$lower_ci, lower(0.90, type = 'right'))
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.90, type = 'right', na.rm = T)$upper_ci, upper(0.90, type = 'right'))
})

testthat::test_that("Comparing, type = 'left', conf_level = '95%'", {
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.95, type = 'right', na.rm = T)$lower_ci, lower(0.95, type = 'right'))
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.95, type = 'right', na.rm = T)$upper_ci, upper(0.95, type = 'right'))
})

testthat::test_that("Comparing, type = 'right', conf_level = '99%'", {
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.99, type = 'right', na.rm = T)$lower_ci, lower(0.99, type = 'right'))
  testthat::expect_equal(ci_norm(x, sd_pop = sd_pop, conf_level = 0.99, type = 'right', na.rm = T)$upper_ci, upper(0.99, type = 'right'))
})
