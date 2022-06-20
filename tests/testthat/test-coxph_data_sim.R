context("test-coxph_data_sim")

ns_c <- 20
ns_e <- 56
ne_c <- 18
ne_e <- 40
km_med_c <- c(22, 15, 40)
km_med_e <- c(130, 78, 185)
cox_hr <- c(0.433, 0.242, 0.774)
km_med_ci_level <- 0.9
cox_hr_ci_level <- 0.95
test_that("coxph_bf yields correct type", {
  expect_is(
    coxph_data_sim(n_data = 2,
                   ns_c = ns_c,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   km_med_c = km_med_c,
                   km_med_e = km_med_e,
                   km_med_ci_level = km_med_ci_level,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 2 * max(km_med_c, km_med_e, na.rm = TRUE),
                   cores = 1,
                   maxit = 50),
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
                   km_med_c = km_med_c,
                   km_med_e = km_med_e,
                   km_med_ci_level = km_med_ci_level,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 2 * max(km_med_c, km_med_e, na.rm = TRUE),
                   cores = 1,
                   maxit = 50),
    "'n_data' must be a single positive integer.",
    fixed = TRUE
  )
  expect_error(
    coxph_data_sim(n_data = 2,
                   ns_c = 0,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   km_med_c = km_med_c,
                   km_med_e = km_med_e,
                   km_med_ci_level = km_med_ci_level,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 2 * max(km_med_c, km_med_e, na.rm = TRUE),
                   cores = 1,
                   maxit = 50),
    "'ns_c', 'ns_e', 'ne_c', and 'ne_e' must be single positive integers.",
    fixed = TRUE
  )
  expect_error(
    coxph_data_sim(n_data = 2,
                   ns_c = ns_c,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   km_med_c = c(2, 1),
                   km_med_e = km_med_e,
                   km_med_ci_level = km_med_ci_level,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 2 * max(km_med_c, km_med_e, na.rm = TRUE),
                   cores = 1,
                   maxit = 50),
    str_c("'km_med_c', 'km_med_e', and 'cox_hr' must be numeric vectors ",
          "of length 3 containing only positive value. The second entry ",
          "must be lower than the first entry and the third entry must ",
          "higher than the first entry."),
    fixed = TRUE
  )
  expect_error(
    coxph_data_sim(n_data = 2,
                   ns_c = ns_c,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   km_med_c = km_med_c,
                   km_med_e = km_med_e,
                   km_med_ci_level = -0.5,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 2 * max(km_med_c, km_med_e, na.rm = TRUE),
                   cores = 1,
                   maxit = 50),
    "'km_med_ci_level' and 'cox_hr_ci_level' must be between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    coxph_data_sim(n_data = 2,
                   ns_c = ns_c,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   km_med_c = km_med_c,
                   km_med_e = km_med_e,
                   km_med_ci_level = km_med_ci_level,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 1 / 2 * max(km_med_c, km_med_e, na.rm = TRUE),
                   cores = 1,
                   maxit = 50),
    str_c("'max_t' must be a single positive number that is higher than ",
          "the maximum of 'km_med_c' and 'km_med_e'."),
    fixed = TRUE
  )
  expect_error(
    coxph_data_sim(n_data = 2,
                   ns_c = ns_c,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   km_med_c = km_med_c,
                   km_med_e = km_med_e,
                   km_med_ci_level = km_med_ci_level,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 2 * max(km_med_c, km_med_e, na.rm = TRUE),
                   w = c(2, 1, 2, 1, 6, 3),
                   cores = 1,
                   maxit = 50),
    str_c("'w' must be a numeric vector of length 9 containing only ",
          "positive numbers."),
    fixed = TRUE
  )
  expect_error(
    coxph_data_sim(n_data = 2,
                   ns_c = ns_c,
                   ns_e = ns_e,
                   ne_c = ne_c,
                   ne_e = ne_e,
                   km_med_c = km_med_c,
                   km_med_e = km_med_e,
                   km_med_ci_level = km_med_ci_level,
                   cox_hr = cox_hr,
                   cox_hr_ci_level = cox_hr_ci_level,
                   max_t = 2 * max(km_med_c, km_med_e, na.rm = TRUE),
                   cores = 0,
                   maxit = 50),
    "'cores' must be a single positive integer.",
    fixed = TRUE
  )
})
