size_x <- 100
size_y <- 500
x <- rbinom(size_x, 1, prob = 0.75) |> c(NA, NA) |> sample()
y <- rbinom(size_y, 1, prob = 0.25) |> c(NA, NA, NA) |> sample()

# expected values ---------------------------------------------------------

lower_ci <- function(conf_level = 0.95, type = "two.sided") {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  if (type == "left") {
    return(
      max(qnorm(1 - conf_level) * sqrt(1 / size_x + 1 / size_y) / 2 + mean(x) - mean(y), -1)
    )
  } else if (type == "right") {
    return(-1)
  } else {
    return(
      max(qnorm((1 - conf_level) / 2) * sqrt(1 / size_x + 1 / size_y) / 2 + mean(x) - mean(y), -1)
    )
  }
}

upper_ci <- function(conf_level = 0.95, type = "two.sided") {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

    if (type == "left") {
    return(1)
  } else if (type == "right") {
    return(
      min(qnorm(conf_level) * sqrt(1 / size_x + 1 / size_y) / 2 + mean(x) - mean(y), 1)
    )
  } else {
    return(
      min(qnorm((1 + conf_level) / 2) * sqrt(1 / size_x + 1 / size_y) / 2 + mean(x) - mean(y), 1)
    )
  }
}


# two sided ---------------------------------------------------------------

testthat::test_that("Comparing 2 proportions - 0-1 vectors, type == 'two.sided', conf_level == 0.90", {
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.90, na.rm = T)$lower_ci, lower_ci(conf_level = 0.90, type = "two.sided"))
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.90, na.rm = T)$upper_ci, upper_ci(conf_level = 0.90, type = "two.sided"))
})

testthat::test_that("Comparing 2 proportions - 0-1 vectors, type == 'two.sided', conf_level == 0.95", {
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.95, na.rm = T)$lower_ci, lower_ci(conf_level = 0.95, type = "two.sided"))
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.95, na.rm = T)$upper_ci, upper_ci(conf_level = 0.95, type = "two.sided"))
})

testthat::test_that("Comparing 2 proportions - 0-1 vectors, type == 'two.sided', conf_level == 0.99", {
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.99, na.rm = T)$lower_ci,  lower_ci(conf_level = 0.99, type = "two.sided"))
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.99, na.rm = T)$upper_ci, upper_ci(conf_level = 0.99, type = "two.sided"))
})


# less --------------------------------------------------------------------


testthat::test_that("Comparing 2 proportions - 0-1 vectors, type == 'left', conf_level == 0.90", {
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.90, type = "left", na.rm = T)$lower_ci, lower_ci(conf_level = 0.90, type = "left"))
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.90, type = "left", na.rm = T)$upper_ci, upper_ci(conf_level = 0.90, type = "left"))
})

testthat::test_that("Comparing 2 proportions - 0-1 vectors, type == 'left', conf_level == 0.95", {
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.95, type = "left", na.rm = T)$lower_ci, lower_ci(conf_level = 0.95, type = "left"))
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.95, type = "left", na.rm = T)$upper_ci, upper_ci(conf_level = 0.95, type = "left"))
})

testthat::test_that("Comparing 2 proportions - 0-1 vectors, type == 'left', conf_level == 0.99", {
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.99, type = "left", na.rm = T)$lower_ci, lower_ci(conf_level = 0.99, type = "left"))
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.99, type = "left", na.rm = T)$upper_ci, upper_ci(conf_level = 0.99, type = "left"))
})


# greater -----------------------------------------------------------------


testthat::test_that("Comparing 2 proportions - 0-1 vectors, type == 'right', conf_level == 0.90", {
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.90, type = "right", na.rm = T)$lower_ci, lower_ci(conf_level = 0.90, type = "right"))
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.90, type = "right", na.rm = T)$upper_ci, upper_ci(conf_level = 0.90, type = "right"))
})

testthat::test_that("Comparing 2 proportions - 0-1 vectors, type == 'right', conf_level == 0.95", {
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.95, type = "right", na.rm = T)$lower_ci, lower_ci(conf_level = 0.95, type = "right"))
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.95, type = "right", na.rm = T)$upper_ci, upper_ci(conf_level = 0.95, type = "right"))
})

testthat::test_that("Comparing 2 proportions - 0-1 vectors, type == 'right', conf_level == 0.99", {
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.99, type = "right", na.rm = T)$lower_ci, lower_ci(conf_level = 0.99, type = "right"))
  testthat::expect_equal(ci_2pop_bern(x, y, conf_level = 0.99, type = "right", na.rm = T)$upper_ci, upper_ci(conf_level = 0.99, type = "right"))
})
