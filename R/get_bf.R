#' Extract Bayes factor from S4 object
#'
#' \code{\link{get_bf}} extracts the Bayes factor from an S4 object (i.e.,
#' \linkS4class{baymedrSuperiority}, \linkS4class{baymedrEquivalence},
#' \linkS4class{baymedrNonInferiority}), created from the functions
#' \code{\link{super_bf}}, \code{\link{equiv_bf}}, or \code{\link{infer_bf}}.
#'
#' @param object An S4 object of class \linkS4class{baymedrSuperiority},
#'   \linkS4class{baymedrEquivalence}, or \linkS4class{baymedrNonInferiority}.
#'
#' @return A numeric scalar, providing the Bayes factor from an S4 object.
#'
#' @export
#'
#' @examples
#' # Extract Bayes factor from a baymedrSuperiority object using raw data:
#' mod_super <- super_bf(x = rnorm(100, 10, 15),
#'                       y = rnorm(130, 13, 10))
#'
#' get_bf(object = mod_super)
#'
#' # Extract Bayes factor from a baymedrEquivalence object using raw data:
#' mod_equiv <- equiv_bf(x = rnorm(100, 10, 15),
#'                       y = rnorm(130, 13, 10))
#'
#' get_bf(object = mod_equiv)
#'
#' # Extract Bayes factor from a baymedrNonInferiority object using raw data:
#' mod_infer <- infer_bf(x = rnorm(100, 10, 15),
#'                       y = rnorm(130, 13, 10),
#'                       ni_margin = 1)
#'
#' get_bf(object = mod_infer)
get_bf <- function(object) {
  if (all(!sapply(c("baymedrSuperiority",
                    "baymedrEquivalence",
                    "baymedrNonInferiority"),
                  function(x) {
                    is(object = object,
                       class2 = x)
                  }))) {
    abort(str_c(
      "Bayes factors can only be extracted from S4 objects of classes ",
      "'baymedrEquivalence', 'baymedrNonInferiority', and ",
      "'baymedrSuperiority'."
    ))
  }
  object@bf
}
