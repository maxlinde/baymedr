context("test-super_bf")

con <- rnorm(100)
exp <- rnorm(100)

test_that("super_bf yields correct S4 class", {
  expect_is(
    super_bf(x = con,
             y = exp),
    "baymedrSuperiority"
  )
  expect_is(
    super_bf(n_x = 100,
             n_y = 100,
             mean_x = 10,
             mean_y = 10,
             sd_x = 5,
             sd_y = 5),
    "baymedrSuperiority"
  )
  expect_is(
    super_bf(n_x = 100,
             n_y = 100,
             mean_x = 10,
             mean_y = 10,
             ci_margin = 5),
    "baymedrSuperiority"
  )
})

test_that("super_bf yields numeric Bayes factor", {
  expect_true(
    is.numeric(super_bf(x = con,
                        y = exp)@bf)
  )
  expect_true(
    is.numeric(super_bf(n_x = 100,
                        n_y = 100,
                        mean_x = 10,
                        mean_y = 10,
                        sd_x = 5,
                        sd_y = 5)@bf)
  )
  expect_true(
    is.numeric(super_bf(n_x = 100,
                        n_y = 100,
                        mean_x = 10,
                        mean_y = 10,
                        ci_margin = 5)@bf)
  )
})

test_that("super_bf gives correct error messages", {
  expect_error(
    super_bf(x = con,
             y = exp,
             ci_margin = 5),
    str_c(
      "Only 'x' and 'y' OR 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', and ",
      "'sd_y' (or 'ci_margin' instead of 'sd_x' and 'sd_y') must be defined."
    ),
    fixed = TRUE
  )
  expect_error(
    super_bf(x = con),
    "Both 'x' and 'y' must be defined.",
    fixed = TRUE
  )
  expect_error(
    super_bf(n_x = 100,
             n_y = 100,
             mean_x = 10,
             sd_x = 5),
    str_c(
      "All 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', and 'sd_y' (or ",
      "'ci_margin' instead of 'sd_x' and 'sd_y') must be defined."
    ),
    fixed = TRUE
  )
  expect_error(
    super_bf(n_x = 100,
             n_y = 100,
             mean_x = 10,
             mean_y = 10,
             sd_x = 5,
             sd_y = 5,
             ci_margin = 5),
    "Only 'sd_x' and 'sd_y' OR 'ci_margin' must be defined.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = con,
             y = c(exp[1:99], NA)),
    "'x' and 'y' must not contain missing values.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = con,
             y = c(exp[1:99], Inf)),
    "'x' and 'y' must not contain infinite values.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = con,
             y = rep(letters[1:10],
                     each = 10)),
    "'x' and 'y' must be numeric vectors.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = con,
             y = exp,
             prior_scale = "x"),
    "'prior_scale' must be a single numeric value.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = con,
             y = exp,
             prior_scale = c(1, 2)),
    "'prior_scale' must be a single numeric value.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = con,
             y = exp,
             alternative = 5),
    "'alternative' must be a single character value.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = con,
             y = exp,
             alternative = c("greater", "less")),
    "'alternative' must be a single character value.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = con,
             y = exp,
             alternative = "abc"),
    "'alternative' must be one of 'greater', 'two_sided', or 'less'.",
    fixed = TRUE
  )
})
