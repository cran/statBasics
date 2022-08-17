size_x <- 100
size_y <- 150
mean_pop_1 <- 5
mean_pop_2 <- 10
sd_pop_1 <- 9
sd_pop_2 <- 9
x <- rnorm(size_x, mean = mean_pop_1, sd = sd_pop_1)
y <- rnorm(size_y, mean = mean_pop_2, sd = sd_pop_2)


# # expeted value -----------------------------------------------------------
#
# lower_ci <- function(conf_level = 0.95, type = "two.sided") {
#   if (type == "left") {
#     return(-Inf)
#   } else if (type == "right") {
#     s_d <- sqrt(((size_x - 1) * var(x) + (size_y - 1) * var(y)) / (size_x + size_y - 2))
#     return(
#       qt(conf_level, df = size_x + size_y - 2) * s_d * sqrt(1 / size_x + 1 / size_y) +
#         mean(x) - mean(y)
#     )
#   } else {
#     s_d <- sqrt(((size_x - 1) * var(x) + (size_y - 1) * var(y)) / (size_x + size_y - 2))
#     return(
#       qt((1 - conf_level) / 2, df = size_x + size_y - 2) * s_d * sqrt(1 / size_x + 1 / size_y) +
#         mean(x) - mean(y)
#     )
#   }
# }
#
# upper_ci <- function(conf_level = 0.95, type = "two.sided") {
#   if (type == "left") {
#     s_d <- sqrt(((size_x - 1) * var(x) + (size_y - 1) * var(y)) / (size_x + size_y - 2))
#     return(
#       qt(1 - conf_level, df = size_x + size_y - 2) * s_d * sqrt(1 / size_x + 1 / size_y) +
#         mean(x) - mean(y)
#     )
#   } else if (type == "right") {
#     return(Inf)
#   } else {
#     s_d <- sqrt(((size_x - 1) * var(x) + (size_y - 1) * var(y)) / (size_x + size_y - 2))
#     return(
#       qt((1 + conf_level) / 2, df = size_x + size_y - 2) * s_d * sqrt(1 / size_x + 1 / size_y) +
#         mean(x) - mean(y)
#     )
#   }
# }

# type = "two.sided" ------------------------------------------------------

testthat::test_that(
  "Comparing - mean difference, type = 'two.sided', conf_level = 90%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.90, var_equal = TRUE)$lower_ci, stats::t.test(x, y, conf.level = 0.90, var.equal = T)$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.90, var_equal = TRUE)$upper_ci, stats::t.test(x, y, conf.level = 0.90, var.equal = T)$conf.int[2])
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'two.sided', conf_level = 95%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.95, var_equal = TRUE)$lower_ci, stats::t.test(x, y, conf.level = 0.95, var.equal = T)$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.95, var_equal = TRUE)$upper_ci, stats::t.test(x, y, conf.level = 0.95, var.equal = T)$conf.int[2])
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'two.sided', conf_level = 99%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.99, var_equal = TRUE)$lower_ci, stats::t.test(x, y, conf.level = 0.99, var.equal = T)$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.99, var_equal = TRUE)$upper_ci, stats::t.test(x, y, conf.level = 0.99, var.equal = T)$conf.int[2])
  }
)


# type = "left" -----------------------------------------------------------


testthat::test_that(
  "Comparing - mean difference, type = 'left', conf_level = 90%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.90, type = "left", var_equal = TRUE)$lower_ci, stats::t.test(x, y, conf.level = 0.90, alternative = "less", var.equal = T)$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.90, type = "left", var_equal = TRUE)$upper_ci, stats::t.test(x, y, conf.level = 0.90, alternative = "less", var.equal = T)$conf.int[2])
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'left', conf_level = 95%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.95, type = "left", var_equal = TRUE)$lower_ci, stats::t.test(x, y, conf.level = 0.95, alternative = "less", var.equal = T)$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.95, type = "left", var_equal = TRUE)$upper_ci, stats::t.test(x, y, conf.level = 0.95, alternative = "less", var.equal = T)$conf.int[2])
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'left', conf_level = 99%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.99, type = "left")$lower_ci, stats::t.test(x, y, conf.level = 0.99, alternative = "less", var.equal = T)$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.99, type = "left", var_equal = TRUE)$upper_ci, stats::t.test(x, y, conf.level = 0.99, alternative = "less", var.equal = T)$conf.int[2])
  }
)


# type = "right" ----------------------------------------------------------


testthat::test_that(
  "Comparing - mean difference, type = 'right', conf_level = 90%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.90, type = "right", var_equal = TRUE)$lower_ci, stats::t.test(x, y, conf.level = 0.90, alternative = "greater", var.equal = T)$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.90, type = "right", var_equal = TRUE)$upper_ci, stats::t.test(x, y, conf.level = 0.90, alternative = "greater", var.equal = T)$conf.int[2])
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'right', conf_level = 95%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.95, type = "right", var_equal = TRUE)$lower_ci, stats::t.test(x, y, conf.level = 0.95, alternative = "greater", var.equal = T)$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.95, type = "right", var_equal = TRUE)$upper_ci, stats::t.test(x, y, conf.level = 0.95, alternative = "greater", var.equal = T)$conf.int[2])
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'right', conf_level = 99%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.99, type = "right", var_equal = TRUE)$lower_ci, stats::t.test(x, y, conf.level = 0.99, alternative = "greater", var.equal = T)$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.99, type = "right", var_equal = TRUE)$upper_ci, stats::t.test(x, y, conf.level = 0.99, alternative = "greater", var.equal = T)$conf.int[2])
  }
)
