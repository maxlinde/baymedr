#' Extract Bayes factor from S4 object
#'
#' This function extracts the Bayes factor from S4 objects.
#'
#' @param object An S4 object of class 'baymedrSuperiority',
#' 'baymedrEquivalence', or 'baymedrNonInferiority'.
#'
#' @return A numeric scalar, providing the Bayes factor from an S4 object.
#'
#' @export
#'
#' @examples
#' # Extract Bayes factor from super_bf using raw data:
#' mod_super <- super_bf(x = rnorm(100, 10, 15),
#'                       y = rnorm(130, 13, 10))
#'
#' get_bf(object = mod_super)
#'
#' # Extract Bayes factor from equiv_bf using raw data:
#' mod_equiv <- equiv_bf(x = rnorm(100, 10, 15),
#'                       y = rnorm(130, 13, 10))
#'
#' get_bf(object = mod_equiv)
#'
#' # Extract Bayes factor from infer_bf using raw data:
#' mod_infer <- infer_bf(x = rnorm(100, 10, 15),
#'                       y = rnorm(130, 13, 10),
#'                       ni_margin = -1)
#'
#' get_bf(object = mod_infer)
get_bf <- function(object) {
  object@bf
}
