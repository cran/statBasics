size <- 1000
mean_pop_1 <- 5
mean_pop_2 <- 10
sd_pop_1 <- 3
sd_pop_2 <- 4
x <- rnorm(size, mean = mean_pop_1, sd = sd_pop_1)
y <- rnorm(size, mean = mean_pop_2, sd = sd_pop_2)

z_test <- function(...) {
  fit <- BSDA::z.test(...)
  lower_ci <- fit$conf.int[1]
  upper_ci <- fit$conf.int[2]
  return(list(conf.int = c(ifelse(is.na(lower_ci), -Inf, lower_ci), ifelse(is.na(upper_ci), Inf, upper_ci))))
}

# type = "two.sided" ------------------------------------------------------

testthat::test_that(
  "Comparing - mean difference, type = 'two.sided', conf_level = 90%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.90)$lower_ci, z_test(x, y, conf.level = 0.90, sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[1], tolerance = 0.01)
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.90)$upper_ci, z_test(x, y, conf.level = 0.90, sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[2], tolerance = 0.01)
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'two.sided', conf_level = 95%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.95)$lower_ci, z_test(x, y, conf.level = 0.95, sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[1], tolerance = 0.01)
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.95)$upper_ci, z_test(x, y, conf.level = 0.95, sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[2], tolerance = 0.01)
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'two.sided', conf_level = 99%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.99)$lower_ci, z_test(x, y, conf.level = 0.99, sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[1], tolerance = 0.01)
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.99)$upper_ci, z_test(x, y, conf.level = 0.99, sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[2], tolerance = 0.01)
  }
)


# type = "left" -----------------------------------------------------------


testthat::test_that(
  "Comparing - mean difference, type = 'left', conf_level = 90%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.90, type = "left")$lower_ci, z_test(x, y, conf.level = 0.90, alternative = "less", sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[1], tolerance = 0.01)
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.90, type = "left")$upper_ci, z_test(x, y, conf.level = 0.90, alternative = "less", sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[2], tolerance = 0.01)
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'left', conf_level = 95%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.95, type = "left")$lower_ci, z_test(x, y, conf.level = 0.95, alternative = "less", sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[1], tolerance = 0.01)
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.95, type = "left")$upper_ci, z_test(x, y, conf.level = 0.95, alternative = "less", sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[2], tolerance = 0.01)
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'left', conf_level = 99%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.99, type = "left")$lower_ci, z_test(x, y, conf.level = 0.99, alternative = "less", sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[1], tolerance = 0.01)
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.99, type = "left")$upper_ci, z_test(x, y, conf.level = 0.99, alternative = "less", sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[2], tolerance = 0.01)
  }
)


# type = "right" ----------------------------------------------------------


testthat::test_that(
  "Comparing - mean difference, type = 'right', conf_level = 90%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.90, type = "right")$lower_ci, z_test(x, y, conf.level = 0.90, alternative = "greater", sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[1], tolerance = 0.01)
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.90, type = "right")$upper_ci, z_test(x, y, conf.level = 0.90, alternative = "greater", sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[2], tolerance = 0.01)
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'right', conf_level = 95%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.95, type = "right")$lower_ci, z_test(x, y, conf.level = 0.95, alternative = "greater", sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[1], tolerance = 0.01)
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.95, type = "right")$upper_ci, z_test(x, y, conf.level = 0.95, alternative = "greater", sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[2], tolerance = 0.01)
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'right', conf_level = 99%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.99, type = "right")$lower_ci, z_test(x, y, conf.level = 0.99, alternative = "greater", sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[1], tolerance = 0.01)
    testthat::expect_equal(ci_2pop_norm(x, y, var_equal = TRUE, conf_level = 0.99, type = "right")$upper_ci, z_test(x, y, conf.level = 0.99, alternative = "greater", sigma.x = sd_pop_1, sigma.y = sd_pop_2)$conf.int[2], tolerance = 0.01)
  }
)
