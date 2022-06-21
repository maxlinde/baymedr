#' S4 classes to represent different models
#'
#' The S4 classes \linkS4class{baymedrSuperiority},
#' \linkS4class{baymedrEquivalence}, \linkS4class{baymedrNonInferiority}, and
#' \linkS4class{baymedrCoxProportionalHazards} represent models for the
#' superiority (\code{\link{super_bf}}), equivalence (\code{\link{equiv_bf}}),
#' non-inferiority (\code{\link{infer_bf}}), and Cox proportional hazards
#' (\code{\link{coxph_bf}}) models, respectively. In addition, there is the class
#' \linkS4class{baymedrCoxProportionalHazardsSamples}, which is used when
#' posterior samples are saved for Cox proportional hazards
#' (\code{\link{coxph_bf}}) models.
#'
#' @slot test Type of test that was conducted.
#' @slot hypotheses The hypotheses that are tested.
#' @slot data The type of data that was used.
#' @slot prior_scale The Cauchy prior scale that was used.
#' @slot bf The resulting Bayes factor.
#' @slot interval The equivalence interval in case of \code{\link{equiv_bf}}.
#' @slot ni_margin The non-inferiority margin in case of \code{\link{infer_bf}}.
#' @slot prior The mean and standard deviation of the Normal prior in case of
#'   \code{\link{coxph_bf}}.
#' @slot samples Posterior samples in case of \code{\link{coxph_bf}}.
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

#' @rdname model-classes
setClass(Class = "baymedrCoxProportionalHazards",
         slots = c(
           test = "character",
           hypotheses = "list",
           prior = "list",
           bf = "numeric"
         ),
         prototype = list(
           test = NA_character_,
           hypotheses = list(),
           prior = list(),
           bf = NA_real_
         ))

#' @rdname model-classes
setClass(Class = "baymedrCoxProportionalHazardsSamples",
         representation = representation(
           test = "character",
           hypotheses = "list",
           prior = "list",
           bf = "numeric",
           samples = "stanfit"
         ))

#' @rdname model-classes
setClass(Class = "baymedrCoxProportionalHazardsMulti",
         slots = c(
           test = "character",
           hypotheses = "list",
           prior = "list",
           bf = "numeric"
         ),
         prototype = list(
           test = NA_character_,
           hypotheses = list(),
           prior = list(),
           bf = NA_real_
         ))

#' @rdname model-classes
setClass(Class = "baymedrCoxProportionalHazardsSamplesMulti",
         representation = representation(
           test = "character",
           hypotheses = "list",
           prior = "list",
           bf = "numeric",
           samples = "list"
         ))
