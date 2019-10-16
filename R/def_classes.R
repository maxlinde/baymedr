#' S4 classes to represent different models
#'
#' The S4 classes \linkS4class{baymedrSuperiority},
#' \linkS4class{baymedrEquivalence}, and \linkS4class{baymedrNonInferiority}
#' represent models for the superiority (\code{\link{super_bf}}), equivalence
#' (\code{\link{equiv_bf}}), and non-inferiority (\code{\link{infer_bf}}) tests,
#' respectively.
#'
#' @slot test Type of test that was conducted.
#' @slot hypotheses The hypotheses that are tested.
#' @slot data The type of data that was used.
#' @slot prior_scale The Cauchy prior scale that was used.
#' @slot bf The resulting Bayes factor.
#' @slot interval The equivalence interval in case of \code{\link{equiv_bf}}.
#' @slot ni_margin The non-inferiority margin in case of \code{\link{infer_bf}}.
#'
#' @name model-classes
#'
#' @import methods
NULL

#' @rdname model-classes
setClass(Class = "baymedrSuperiority",
         slots = c(
           test = "character",
           hypotheses = "list",
           data = "list",
           prior_scale = "numeric",
           bf = "numeric"
         ),
         prototype = list(
           test = NA_character_,
           hypotheses = list(),
           data = list(),
           prior_scale = NA_real_,
           bf = NA_real_
         ))

#' @rdname model-classes
setClass(Class = "baymedrEquivalence",
         slots = c(
           test = "character",
           hypotheses = "list",
           interval = "list",
           data = "list",
           prior_scale = "numeric",
           bf = "numeric"
         ),
         prototype = list(
           test = NA_character_,
           hypotheses = list(),
           interval = list(),
           data = list(),
           prior_scale = NA_real_,
           bf = NA_real_
         ))

#' @rdname model-classes
setClass(Class = "baymedrNonInferiority",
         slots = c(
           test = "character",
           hypotheses = "list",
           ni_margin = "list",
           data = "list",
           prior_scale = "numeric",
           bf = "numeric"
         ),
         prototype = list(
           test = NA_character_,
           hypotheses = list(),
           ni_margin = list(),
           data = list(),
           prior_scale = NA_real_,
           bf = NA_real_
         ))
