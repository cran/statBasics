size <- 1000
mean_pop <- 3
sd_pop <- 2
x <- c(rnorm(size, mean = mean_pop, sd = sd_pop), NA)


lower <- function(conf_level, type = 'two.sided') {
  if (type == 'two.sided') {
    suppressWarnings({
      return(unname(EnvStats::varTest(x, conf.level = conf_level)$conf.int[1]))
    })
  } else if (type == 'left') {
    suppressWarnings({
      return(unname(EnvStats::varTest(x, conf.level = conf_level, alternative = "greater")$conf.int[1]))
    })
  } else if (type == 'right') {
    return(0)
  }
}

upper <- function(conf_level, type = 'two.sided') {
  if (type == 'two.sided') {
    suppressWarnings({
      return(unname(EnvStats::varTest(x, conf.level = conf_level)$conf.int[2]))
    })
  } else if (type == 'left') {
    return(Inf)
  } else if (type == 'right') {
    suppressWarnings({
      return(unname(EnvStats::varTest(x, conf.level = conf_level, alternative = 'less')$conf.int[2]))
    })
  }
}

###############################################################################
# type = 'two.sided'

testthat::test_that("Comparing, type = 'two.sided', conf_level = '90%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.90, na.rm = T)$lower_ci, lower(0.90))
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.90, na.rm = T)$upper_ci, upper(0.90))
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '95%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.95, na.rm = T)$lower_ci, lower(0.95))
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.95, na.rm = T)$upper_ci, upper(0.95))
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '99%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.99, na.rm = T)$lower_ci, lower(0.99))
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.99, na.rm = T)$upper_ci, upper(0.99))
})

###############################################################################
# type = 'left'

testthat::test_that("Comparing, type = 'left', conf_level = '90%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.90, type = 'left', na.rm = T)$lower_ci, lower(0.90, type = 'left'))
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.90, type = 'left', na.rm = T)$upper_ci, upper(0.90, type = 'left'))
})

testthat::test_that("Comparing, type = 'left', conf_level = '95%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.95, type = 'left', na.rm = T)$lower_ci, lower(0.95, type = 'left'))
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.95, type = 'left', na.rm = T)$upper_ci, upper(0.95, type = 'left'))
})

testthat::test_that("Comparing, type = 'left', conf_level = '99%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.99, type = 'left', na.rm = T)$lower_ci, lower(0.99, type = 'left'))
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.99, type = 'left', na.rm = T)$upper_ci, upper(0.99, type = 'left'))
})

###############################################################################
# type = 'right'

testthat::test_that("Comparing, type = 'right', conf_level = '90%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.90, type = 'right', na.rm = T)$lower_ci, lower(0.90, type = 'right'))
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.90, type = 'right', na.rm = T)$upper_ci, upper(0.90, type = 'right'))
})

testthat::test_that("Comparing, type = 'right', conf_level = '95%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.95, type = 'right', na.rm = T)$lower_ci, lower(0.95, type = 'right'))
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.95, type = 'right', na.rm = T)$upper_ci, upper(0.95, type = 'right'))
})

testthat::test_that("Comparing, type = 'right', conf_level = '99%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.99, type = 'right', na.rm = T)$lower_ci, lower(0.99, type = 'right'))
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.99, type = 'right', na.rm = T)$upper_ci, upper(0.99, type = 'right'))
})
