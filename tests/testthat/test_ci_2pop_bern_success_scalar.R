size_x <- 100
size_y <- 666
x <- 33
y <- 400

n_x <- size_x
n_y <- size_y

# expected values ---------------------------------------------------------

lower_ci <- function(conf_level = 0.95, type = "two.sided") {
  if (type == "left") {
    return(
      max(qnorm(1 - conf_level) * sqrt(1 / size_x + 1 / size_y) / 2 + x / size_x - y / size_y, -1)
    )
  } else if (type == "right") {
    return(-1)
  } else {
    return(
      max(qnorm((1 - conf_level) / 2) * sqrt(1 / size_x + 1 / size_y) / 2 + x / size_x - y / size_y, -1)
    )
  }
}

upper_ci <- function(conf_level = 0.95, type = "two.sided") {
  if (type == "left") {
    return(1)
  } else if (type == "right") {
    return(
      min(qnorm(conf_level) * sqrt(1 / size_x + 1 / size_y) / 2 + x / size_x - y / size_y, 1)
    )
  } else {
    return(
      min(qnorm((1 + conf_level) / 2) * sqrt(1 / size_x + 1 / size_y) / 2 + x / size_x - y / size_y, 1)
    )
  }
}


# two sided ---------------------------------------------------------------

testthat::test_that("Comparing 2 proportions - scalar, type == 'two.sided', conf_level == 0.90", {
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.90)$lower_ci, lower_ci(conf_level = 0.90, type = "two.sided"))
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.90)$upper_ci, upper_ci(conf_level = 0.90, type = "two.sided"))
})

testthat::test_that("Comparing 2 proportions - scalar, type == 'two.sided', conf_level == 0.95", {
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.95)$lower_ci, lower_ci(conf_level = 0.95, type = "two.sided"))
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.95)$upper_ci, upper_ci(conf_level = 0.95, type = "two.sided"))
})

testthat::test_that("Comparing 2 proportions - scalar, type == 'two.sided', conf_level == 0.99", {
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.99)$lower_ci, lower_ci(conf_level = 0.99, type = "two.sided"))
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.99)$upper_ci, upper_ci(conf_level = 0.99, type = "two.sided"))
})


# less --------------------------------------------------------------------


testthat::test_that("Comparing 2 proportions - scalar, type == 'left', conf_level == 0.90", {
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.90, type = "left")$lower_ci, lower_ci(conf_level = 0.90, type = "left"))
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.90, type = "left")$upper_ci, upper_ci(conf_level = 0.90, type = "left"))
})

testthat::test_that("Comparing 2 proportions - scalar, type == 'left', conf_level == 0.95", {
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.95, type = "left")$lower_ci, lower_ci(conf_level = 0.95, type = "left"))
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.95, type = "left")$upper_ci, upper_ci(conf_level = 0.95, type = "left"))
})

testthat::test_that("Comparing 2 proportions - scalar, type == 'left', conf_level == 0.99", {
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.99, type = "left")$lower_ci, lower_ci(conf_level = 0.99, type = "left"))
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.99, type = "left")$upper_ci, upper_ci(conf_level = 0.99, type = "left"))
})


# greater -----------------------------------------------------------------


testthat::test_that("Comparing 2 proportions - scalar, type == 'right', conf_level == 0.90", {
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.90, type = "right")$lower_ci, lower_ci(conf_level = 0.90, type = "right"))
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.90, type = "right")$upper_ci, upper_ci(conf_level = 0.90, type = "right"))
})

testthat::test_that("Comparing 2 proportions - scalar, type == 'right', conf_level == 0.95", {
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.95, type = "right")$lower_ci, lower_ci(conf_level = 0.95, type = "right"))
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.95, type = "right")$upper_ci, upper_ci(conf_level = 0.95, type = "right"))
})

testthat::test_that("Comparing 2 proportions - scalar, type == 'right', conf_level == 0.99", {
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.99, type = "right")$lower_ci, lower_ci(conf_level = 0.99, type = "right"))
  testthat::expect_equal(ci_2pop_bern(x, y, n_x, n_y, conf_level = 0.99, type = "right")$upper_ci, upper_ci(conf_level = 0.99, type = "right"))
})
