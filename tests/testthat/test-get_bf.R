context("test-get_bf")

con <- rnorm(100)
exp <- rnorm(100)

mod_super <- super_bf(x = con,
                      y = exp)

mod_equiv <- equiv_bf(x = con,
                      y = exp)

mod_infer <- infer_bf(x = con,
                      y = exp,
                      ni_margin = 0.5)

data <- survival::aml
names(data) <- c("time", "event", "group")
data$group <- ifelse(test = data$group == "Maintained",
                     yes = 0,
                     no = 1)

mod_coxph <- coxph_bf(data = data,
                      save_samples = FALSE,
                      iter = 5000)

mod_coxph_samples <- coxph_bf(data = data,
                              save_samples = TRUE,
                              iter = 5000)

sim_data <- coxph_data_sim(n_data = 3,
                           ns_c = 20,
                           ns_e = 56,
                           ne_c = 18,
                           ne_e = 40,
                           cox_hr = c(0.433, 0.242, 0.774),
                           cox_hr_ci_level = 0.95,
                           maxit = 25)

mod_coxph_multi <- coxph_bf(data = sim_data,
                            save_samples = FALSE,
                            chains = 1,
                            iter = 5000)

mod_coxph_samples_multi <- coxph_bf(data = sim_data,
                                    save_samples = TRUE,
                                    chains = 1,
                                    iter = 5000)

test_that("get_bf extracts numeric Bayes factor from S4 object", {
  expect_true(
    is.numeric(get_bf(mod_super))
  )
  expect_true(
    is.numeric(get_bf(mod_equiv))
  )
  expect_true(
    is.numeric(get_bf(mod_infer))
  )
  expect_true(
    is.numeric(get_bf(mod_coxph))
  )
  expect_true(
    is.numeric(get_bf(mod_coxph_samples))
  )
  expect_true(
    is.numeric(get_bf(mod_coxph_multi))
  )
  expect_true(
    is.numeric(get_bf(mod_coxph_samples_multi))
  )
})

test_that("get_bf gives correct error messages", {
  expect_error(
    get_bf(t.test(x = con,
                  y = exp)),
    str_c(
      "Bayes factors can only be extracted from S4 objects of classes ",
      "'baymedrEquivalence', 'baymedrNonInferiority', ",
      "'baymedrSuperiority', 'baymedrCoxProportionalHazards', ",
      "'baymedrCoxProportionalHazardsSamples', ",
      "'baymedrCoxProportionalHazardsMulti', and ",
      "'baymedrCoxProportionalHazardsSamplesMulti'."
    ),
    fixed = TRUE
  )
})
