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
})

test_that("get_bf gives correct error messages", {
  expect_error(
    get_bf(t.test(x = con,
                  y = exp)),
    str_c(
      "Bayes factors can only be extracted from S4 objects of classes ",
      "'baymedrEquivalence', 'baymedrNonInferiority', and ",
      "'baymedrSuperiority'."
    ),
    fixed = TRUE
  )
})
