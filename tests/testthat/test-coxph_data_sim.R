context("test-coxph_data_sim")

ns_c <- 20
ns_e <- 56
ne_c <- 18
ne_e <- 40
cox_hr <- c(0.433, 0.242, 0.774)
cox_hr_ci_level <- 0.95
test_that("coxph_bf yields correct type", {
  expect_is(
    coxph_data_sim(n_data = 2,
                   ns_c = ns_c,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 100,
                   cores = 1,
                   maxit = 25),
    "list"
  )
})

test_that("coxph_data_sim gives correct error messages", {
  expect_error(
    coxph_data_sim(n_data = 0,
                   ns_c = ns_c,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 100,
                   cores = 1,
                   maxit = 25),
    "'n_data' must be a single positive integer.",
    fixed = TRUE
  )
  expect_error(
    coxph_data_sim(n_data = 2,
                   ns_c = 0,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 100,
                   cores = 1,
                   maxit = 25),
    "'ns_c', 'ns_e', 'ne_c', and 'ne_e' must be single positive integers.",
    fixed = TRUE
  )
  expect_error(
    coxph_data_sim(n_data = 2,
                   ns_c = ns_c,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   cox_hr = c(NA, 4, 6),
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 100,
                   cores = 1,
                   maxit = 25),
    str_c("'cox_hr' must be a numeric vector of length 3 containing only ",
          "positive values. The second entry must be lower than the first ",
          "entry and the third entry must be higher than the first entry."),
    fixed = TRUE
  )
  expect_error(
    coxph_data_sim(n_data = 2,
                   ns_c = ns_c,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = 95,
                   max_t = 100,
                   cores = 1,
                   maxit = 25),
    "'cox_hr_ci_level' must be between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    coxph_data_sim(n_data = 2,
                   ns_c = ns_c,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = c(5, 3),
                   cores = 1,
                   maxit = 25),
    "'max_t' must be a single positive number.",
    fixed = TRUE
  )
  expect_error(
    coxph_data_sim(n_data = 2,
                   ns_c = ns_c,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 100,
                   cores = 0,
                   maxit = 25),
    "'cores' must be a single positive integer.",
    fixed = TRUE
  )
})
