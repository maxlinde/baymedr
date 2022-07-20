#' Extract Bayes factor from S4 object
#'
#' \code{\link{get_bf}} extracts the Bayes factor from an S4 object (i.e.,
#' \linkS4class{baymedrSuperiority}, \linkS4class{baymedrEquivalence},
#' \linkS4class{baymedrNonInferiority}),
#' \linkS4class{baymedrCoxProportionalHazards}, and
#' \linkS4class{baymedrCoxProportionalHazardsMulti}.
#'
#' @param object An S4 object of class \linkS4class{baymedrSuperiority},
#'   \linkS4class{baymedrEquivalence}, \linkS4class{baymedrNonInferiority},
#'   \linkS4class{baymedrCoxProportionalHazards}, or
#'   \linkS4class{baymedrCoxProportionalHazardsMulti}.
#'
#' @return A numeric vector, providing the Bayes factor(s) from an S4 object.
#'
#' @import survival
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
#'
#' # Extract Bayes factor from a baymedrCoxProportionalHazards object:
#' data <- survival::aml
#' names(data) <- c("time", "event", "group")
#' data$group <- ifelse(test = data$group == "Maintained",
#'                      yes = 0,
#'                      no = 1)
#'
#' mod_coxph <- coxph_bf(data = data)
#'
#' get_bf(object = mod_coxph)
get_bf <- function(object) {
  if (all(!sapply(c("baymedrSuperiority",
                    "baymedrEquivalence",
                    "baymedrNonInferiority",
                    "baymedrCoxProportionalHazards",
                    "baymedrCoxProportionalHazardsMulti"),
                  function(x) {
                    is(object = object,
                       class2 = x)
                  }))) {
    stop(str_c(
      "Bayes factors can only be extracted from S4 objects of classes ",
      "'baymedrEquivalence', 'baymedrNonInferiority', ",
      "'baymedrSuperiority', 'baymedrCoxProportionalHazards', and ",
      "'baymedrCoxProportionalHazardsMulti'."
    ),
    call. = FALSE)
  }
  object@bf
}
