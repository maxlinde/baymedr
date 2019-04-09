context("test-super_bf")

test_that("super_bf yields correct S4 class", {
  expect_is(
    super_bf(x = rnorm(100),
             y = rnorm(100)),
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
    is.numeric(super_bf(x = rnorm(100),
                        y = rnorm(100))@bf)
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
    super_bf(x = rnorm(100),
             y = rnorm(100),
             ci_margin = 5),
    str_c(
      "Only 'x' and 'y' OR 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', and ",
      "'sd_y' (or 'ci_margin' instead of 'sd_x' and 'sd_y') must be defined."
    ),
    fixed = TRUE
  )
  expect_error(
    super_bf(x = rnorm(100)),
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
    super_bf(x = rnorm(100),
             y = c(rnorm(99), NA)),
    "'x' and 'y' must not contain missing values.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = rnorm(100),
             y = c(rnorm(99), Inf)),
    "'x' and 'y' must not contain infinite values.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = rnorm(100),
             y = rep(letters[1:10],
                     each = 10)),
    "'x' and 'y' must be numeric vectors.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = rnorm(100),
             y = rnorm(100),
             prior_scale = "x"),
    "'prior_scale' must be a single numeric value.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = rnorm(100),
             y = rnorm(100),
             prior_scale = c(1, 2)),
    "'prior_scale' must be a single numeric value.",
    fixed = TRUE
  )
  expect_error(
    super_bf(x = rnorm(100),
             y = rnorm(100),
             one_sided = "Yes"),
    "'one_sided' must be a logical value.",
    fixed = TRUE
  )
})
