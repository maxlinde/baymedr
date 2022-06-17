context("test-coxph_bf")

time <- survival::aml$time
event <- survival::aml$status
group <- ifelse(test = survival::aml$x == "Maintained",
                yes = 0,
                no = 1)

test_that("coxph_bf yields correct S4 class", {
  expect_is(
    coxph_bf(time = time,
             event = event,
             group = group),
    "baymedrCoxProportionalHazards"
  )
  expect_is(
    coxph_bf(time = time,
             event = event,
             group = group,
             save_samples = TRUE),
    "baymedrCoxProportionalHazardsSamples"
  )
})

test_that("coxph_bf yields numeric Bayes factor", {
  expect_true(
    is.numeric(coxph_bf(time = time,
                        event = event,
                        group = group)@bf)
  )
  expect_true(
    is.numeric(coxph_bf(time = time,
                        event = event,
                        group = group,
                        save_samples = TRUE)@bf)
  )
})

test_that("coxph_bf gives correct error messages", {
  expect_error(
    coxph_bf(time = sample(x = letters,
                           size = 100,
                           replace = TRUE),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE)),
    "'time' must be a non-negative numeric vector.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = rnorm(n = 100,
                          mean = -100),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE)),
    "'time' must be a non-negative numeric vector.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 100,
                          min = 0,
                          max = 100),
             event = sample(x = letters,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE)),
    "'event' must be a numeric vector containing only the values 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 100,
                          min = 0,
                          max = 100),
             event = sample(x = 0:5,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE)),
    "'event' must be a numeric vector containing only the values 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 100,
                          min = 0,
                          max = 100),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = letters,
                            size = 100,
                            replace = TRUE)),
    "'group' must be a numeric vector containing only the values 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 100,
                          min = 0,
                          max = 100),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:5,
                            size = 100,
                            replace = TRUE)),
    "'group' must be a numeric vector containing only the values 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 50,
                          min = 0,
                          max = 100),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE)),
    "'time', 'event', and 'group' must have the same length.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 100,
                          min = 0,
                          max = 100),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             null_value = "a"),
    "'null_value' must be a single numeric value.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 100,
                          min = 0,
                          max = 100),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             null_value = -1:1),
    "'null_value' must be a single numeric value.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 100,
                          min = 0,
                          max = 100),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             alternative = "abc"),
    "'alternative' must be one of one.sided or two.sided.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 100,
                          min = 0,
                          max = 100),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             alternative = "two.sided",
             direction = -1),
    "When 'alternative' is two.sided, 'direction' must be NULL.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 100,
                          min = 0,
                          max = 100),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             alternative = "one.sided",
             direction = NULL),
    "When 'alternative' is one.sided, 'direction' must be -1 or 1.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 100,
                          min = 0,
                          max = 100),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             prior_sd = 1:3),
    str_c("'prior_mean' and 'prior_sd' must be single numeric values. ",
          "'prior_sd' must be non-negative."),
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 100,
                          min = 0,
                          max = 100),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             save_samples = "yes"),
    "'save_samples' must be a single logical value.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(time = runif(n = 100,
                          min = 0,
                          max = 100),
             event = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             group = sample(x = 0:1,
                            size = 100,
                            replace = TRUE),
             prior_sd = -1),
    str_c("'prior_mean' and 'prior_sd' must be single numeric values. ",
          "'prior_sd' must be non-negative."),
    fixed = TRUE
  )
})
