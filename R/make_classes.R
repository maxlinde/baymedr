#' @import methods
baymedrSuperiority <- function(test,
                               hypotheses,
                               data,
                               prior_scale,
                               bf) {
  new(Class = "baymedrSuperiority",
      test = test,
      hypotheses = hypotheses,
      data = data,
      prior_scale = prior_scale,
      bf = bf)
}

baymedrEquivalence <- function(test,
                               hypotheses,
                               interval,
                               data,
                               prior_scale,
                               bf) {
  new(Class = "baymedrEquivalence",
      test = test,
      hypotheses = hypotheses,
      interval = interval,
      data = data,
      prior_scale = prior_scale,
      bf = bf)
}

baymedrNonInferiority <- function(test,
                                  hypotheses,
                                  ni_margin,
                                  data,
                                  prior_scale,
                                  bf) {
  new(Class = "baymedrNonInferiority",
      test = test,
      hypotheses = hypotheses,
      ni_margin = ni_margin,
      data = data,
      prior_scale = prior_scale,
      bf = bf)
}
