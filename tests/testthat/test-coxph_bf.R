context("test-coxph_bf")

data <- survival::aml
names(data) <- c("time", "event", "group")
data$group <- ifelse(test = survival::aml$x == "Maintained",
                     yes = 0,
                     no = 1)

sim_data <- coxph_data_sim(n_data = 3,
                           ns_c = 20,
                           ns_e = 56,
                           ne_c = 18,
                           ne_e = 40,
                           cox_hr = c(0.433, 0.242, 0.774),
                           cox_hr_ci_level = 0.95,
                           maxit = 25)


test_that("coxph_bf yields correct S4 class", {
  expect_is(
    coxph_bf(data = data),
    "baymedrCoxProportionalHazards"
  )
  expect_is(
    coxph_bf(data = sim_data),
    "baymedrCoxProportionalHazardsMulti"
  )
})

test_that("coxph_bf yields numeric Bayes factor", {
  expect_true(
    is.numeric(coxph_bf(data = data)@bf)
  )
  expect_true(
    is.numeric(coxph_bf(data = sim_data)@bf)
  )
})

test_that("coxph_bf gives correct error messages", {
  expect_error(
    coxph_bf(data = factor(sample(1:5, 100, TRUE))),
    str_c("'data' must be either a single data.frame or a list resulting ",
          "from coxph_data_sim()."),
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = sample(x = letters,
                    size = 100,
                    replace = TRUE),
      event = sample(x = 0:1,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:1,
                     size = 100,
                     replace = TRUE)
    )),
    "The first column in 'data' must be a non-negative numeric vector.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = rnorm(n = 100,
                   mean = -100),
      event = sample(x = 0:1,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:1,
                     size = 100,
                     replace = TRUE)
    )),
    "The first column in 'data' must be a non-negative numeric vector.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = runif(n = 100,
                   min = 0,
                   max = 100),
      event = sample(x = letters,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:1,
                     size = 100,
                     replace = TRUE)
    )),
    str_c("The second column in 'data' must be a numeric vector containing ",
          "only the values 0 and 1."),
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = runif(n = 100,
                   min = 0,
                   max = 100),
      event = sample(x = 0:5,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:1,
                     size = 100,
                     replace = TRUE)
    )),
    str_c("The second column in 'data' must be a numeric vector containing ",
          "only the values 0 and 1."),
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = runif(n = 100,
                   min = 0,
                   max = 100),
      event = sample(x = 0:1,
                     size = 100,
                     replace = TRUE),
      group = sample(x = letters,
                     size = 100,
                     replace = TRUE)
    )),
    str_c("The third column in 'data' must be a numeric vector containing ",
          "only the values 0 and 1."),
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = runif(n = 100,
                   min = 0,
                   max = 100),
      event = sample(x = 0:1,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:5,
                     size = 100,
                     replace = TRUE)
    )),
    str_c("The third column in 'data' must be a numeric vector containing ",
          "only the values 0 and 1."),
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = runif(n = 100,
                   min = 0,
                   max = 100),
      event = sample(x = 0:1,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:1,
                     size = 100,
                     replace = TRUE)
    ),
    null_value = "a"),
    "'null_value' must be a single numeric value.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = runif(n = 100,
                   min = 0,
                   max = 100),
      event = sample(x = 0:1,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:1,
                     size = 100,
                     replace = TRUE)
    ),
    null_value = -1:1),
    "'null_value' must be a single numeric value.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = runif(n = 100,
                   min = 0,
                   max = 100),
      event = sample(x = 0:1,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:1,
                     size = 100,
                     replace = TRUE)
    ),
    alternative = "abc"),
    "'alternative' must be one of one.sided or two.sided.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = runif(n = 100,
                   min = 0,
                   max = 100),
      event = sample(x = 0:1,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:1,
                     size = 100,
                     replace = TRUE)
    ),
    alternative = "two.sided",
    direction = -1),
    "When 'alternative' is two.sided, 'direction' must be NULL.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = runif(n = 100,
                   min = 0,
                   max = 100),
      event = sample(x = 0:1,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:1,
                     size = 100,
                     replace = TRUE)
    ),
    alternative = "one.sided",
    direction = NULL),
    "When 'alternative' is one.sided, 'direction' must be low or high.",
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = runif(n = 100,
                   min = 0,
                   max = 100),
      event = sample(x = 0:1,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:1,
                     size = 100,
                     replace = TRUE)
    ),
    prior_sd = 1:3),
    str_c("'prior_mean' and 'prior_sd' must be single numeric values. ",
          "'prior_sd' must be positive."),
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = runif(n = 100,
                   min = 0,
                   max = 100),
      event = sample(x = 0:1,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:1,
                     size = 100,
                     replace = TRUE)
    ),
    prior_sd = -1),
    str_c("'prior_mean' and 'prior_sd' must be single numeric values. ",
          "'prior_sd' must be positive."),
    fixed = TRUE
  )
  expect_error(
    coxph_bf(data = data.frame(
      time = c(runif(n = 99,
                     min = 0,
                     max = 100), NA),
      event = sample(x = 0:1,
                     size = 100,
                     replace = TRUE),
      group = sample(x = 0:1,
                     size = 100,
                     replace = TRUE)
    )),
    "'data' must not contain any missing values.",
    fixed = TRUE
  )
})
