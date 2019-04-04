#' @import methods
setClass(Class = "baymedrSuperiority",
         slots = c(test = "character",
                   hypotheses = "list",
                   data = "list",
                   prior_scale = "numeric",
                   bf = "numeric"))

setClass(Class = "baymedrEquivalence",
         slots = c(test = "character",
                   hypotheses = "list",
                   interval = "list",
                   data = "list",
                   prior_scale = "numeric",
                   bf = "numeric"))

setClass(Class = "baymedrNonInferiority",
         slots = c(test = "character",
                   hypotheses = "list",
                   ni_margin = "numeric",
                   data = "list",
                   prior_scale = "numeric",
                   bf = "numeric"))
