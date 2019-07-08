context("test-infer_bf")

con <- rnorm(100)
exp <- rnorm(100)

test_that("infer_bf yields correct S4 class", {
  expect_is(
    infer_bf(x = con,
             y = exp,
             ni_margin = 0.5),
    "baymedrNonInferiority"
  )
  expect_is(
    infer_bf(n_x = 100,
             n_y = 100,
             mean_x = 10,
             mean_y = 10,
             sd_x = 5,
             sd_y = 5,
             ni_margin = 0.5),
    "baymedrNonInferiority"
  )
})

test_that("infer_bf yields numeric Bayes factor", {
  expect_true(
    is.numeric(infer_bf(x = con,
                        y = exp,
                        ni_margin = 0.5)@bf)
  )
  expect_true(
    is.numeric(infer_bf(n_x = 100,
                        n_y = 100,
                        mean_x = 10,
                        mean_y = 10,
                        sd_x = 5,
                        sd_y = 5,
                        ni_margin = 0.5)@bf)
  )
})

test_that("infer_bf gives correct error messages", {
  expect_error(
    infer_bf(x = con,
             y = exp,
             n_x = 80,
             ni_margin = 0.5),
    str_c(
      "Only 'x', 'y', and 'ni_margin' OR 'n_x', 'n_y', 'mean_x', 'mean_y', ",
      "'sd_x', 'sd_y', and 'ni_margin' (or 'ci_margin' and 'ci_level' instead ",
      "of 'sd_x' and 'sd_y') must be defined."
    ),
    fixed = TRUE
  )
  expect_error(
    infer_bf(x = con),
    "All 'x', 'y', and 'ni_margin' must be defined.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(n_x = 100,
             n_y = 100,
             mean_x = 10,
             sd_x = 5),
    str_c(
      "All 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', 'sd_y', and ",
      "'ni_margin' (or 'ci_margin' and 'ci_level' instead of 'sd_x' and ",
      "'sd_y') must be defined."
    ),
    fixed = TRUE
  )
  expect_error(
    infer_bf(x = con,
             y = c(exp[1:99], NA),
             ni_margin = -0.5),
    "'x' and 'y' must not contain missing values.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(x = con,
             y = c(exp[1:99], Inf),
             ni_margin = -0.5),
    "'x' and 'y' must not contain infinite values.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(x = con,
             y = rep(letters[1:10],
                     each = 10),
             ni_margin = -0.5),
    "'x' and 'y' must be numeric vectors.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(x = con,
             y = exp,
             ni_margin = -0.5,
             prior_scale = "x"),
    "'prior_scale' must be a single numeric value.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(x = con,
             y = exp,
             ni_margin = -0.5,
             prior_scale = c(1, 2)),
    "'prior_scale' must be a single numeric value.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(x = con,
             y = exp,
             ni_margin = "0.5"),
    "'ni_margin' must be a single positive numeric value.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(x = con,
             y = exp,
             ni_margin = c(-0.5, 0.5)),
    "'ni_margin' must be a single positive numeric value.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(x = con,
             y = exp,
             ni_margin = -0.5),
    "'ni_margin' must be a single positive numeric value.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(x = con,
             y = exp,
             ni_margin = 0.5,
             direction = 5),
    "'direction' must be a single character value.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(x = con,
             y = exp,
             ni_margin = 0.5,
             direction = c("low", "high")),
    "'direction' must be a single character value.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(x = con,
             y = exp,
             ni_margin = 0.5,
             direction = "abc"),
    "'direction' must be one of 'low' or 'high'.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(n_x = 100,
             n_y = 100,
             mean_x = 0,
             mean_y = 2,
             ci_margin = 5,
             ci_level = 1.5,
             ni_margin = 0.5),
    "'ci_level' must be a single numeric value between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(n_x = 100,
             n_y = 100,
             mean_x = 0,
             mean_y = 2,
             ci_margin = 5,
             ci_level = "high",
             ni_margin = 0.5),
    "'ci_level' must be a single numeric value between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    infer_bf(n_x = 100,
             n_y = 100,
             mean_x = 0,
             mean_y = 2,
             ci_margin = 5,
             ci_level = c(0.3, 0.7),
             ni_margin = 0.5),
    "'ci_level' must be a single numeric value between 0 and 1.",
    fixed = TRUE
  )
})
