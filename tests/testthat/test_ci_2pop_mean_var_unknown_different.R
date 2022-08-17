size <- 1000
mean_pop_1 <- 5
mean_pop_2 <- 10
sd_pop_1 <- 9
sd_pop_2 <- 16
x <- rnorm(size, mean = mean_pop_1, sd = sd_pop_1)
y <- rnorm(size, mean = mean_pop_2, sd = sd_pop_2)


# type = "two.sided" ------------------------------------------------------

testthat::test_that(
  "Comparing - mean difference, type = 'two.sided', conf_level = 90%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.90)$lower_ci, stats::t.test(x, y, conf.level = 0.90)$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.90)$upper_ci, stats::t.test(x, y, conf.level = 0.90)$conf.int[2])
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'two.sided', conf_level = 95%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.95)$lower_ci, stats::t.test(x, y, conf.level = 0.95)$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.95)$upper_ci, stats::t.test(x, y, conf.level = 0.95)$conf.int[2])
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'two.sided', conf_level = 99%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.99)$lower_ci, stats::t.test(x, y, conf.level = 0.99)$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.99)$upper_ci, stats::t.test(x, y, conf.level = 0.99)$conf.int[2])
  }
)


# type = "left" -----------------------------------------------------------


testthat::test_that(
  "Comparing - mean difference, type = 'left', conf_level = 90%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.90, type = "left")$lower_ci, stats::t.test(x, y, conf.level = 0.90, alternative = "less")$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.90, type = "left")$upper_ci, stats::t.test(x, y, conf.level = 0.90, alternative = "less")$conf.int[2])
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'left', conf_level = 95%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.95, type = "left")$lower_ci, stats::t.test(x, y, conf.level = 0.95, alternative = "less")$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.95, type = "left")$upper_ci, stats::t.test(x, y, conf.level = 0.95, alternative = "less")$conf.int[2])
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'left', conf_level = 99%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.99, type = "left")$lower_ci, stats::t.test(x, y, conf.level = 0.99, alternative = "less")$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.99, type = "left")$upper_ci, stats::t.test(x, y, conf.level = 0.99, alternative = "less")$conf.int[2])
  }
)


# type = "right" ----------------------------------------------------------


testthat::test_that(
  "Comparing - mean difference, type = 'right', conf_level = 90%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.90, type = "right")$lower_ci, stats::t.test(x, y, conf.level = 0.90, alternative = "greater")$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.90, type = "right")$upper_ci, stats::t.test(x, y, conf.level = 0.90, alternative = "greater")$conf.int[2])
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'right', conf_level = 95%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.95, type = "right")$lower_ci, stats::t.test(x, y, conf.level = 0.95, alternative = "greater")$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.95, type = "right")$upper_ci, stats::t.test(x, y, conf.level = 0.95, alternative = "greater")$conf.int[2])
  }
)

testthat::test_that(
  "Comparing - mean difference, type = 'right', conf_level = 99%",
  {
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.99, type = "right")$lower_ci, stats::t.test(x, y, conf.level = 0.99, alternative = "greater")$conf.int[1])
    testthat::expect_equal(ci_2pop_norm(x, y, conf_level = 0.99, type = "right")$upper_ci, stats::t.test(x, y, conf.level = 0.99, alternative = "greater")$conf.int[2])
  }
)
