#' @import methods
setMethod(
  f = "show",
  signature = "baymedrSuperiority",
  definition = function(object) {
    cat("******************************\n",
        object@test,
        "\n",
        rep("-",
            times = nchar(object@test)),
        "\n",
        "H0: ",
        object@hypotheses$h0,
        "; Ha: ",
        object@hypotheses$ha,
        "\n",
        "Prior scale: ",
        round(x = object@prior_scale,
              digits = 3),
        "\n\n",
        "    BF = ",
        round(x = object@bf,
              digits = 3),
        "\n",
        "******************************\n",
        sep = "")
  }
)

setMethod(
  f = "show",
  signature = "baymedrEquivalence",
  definition = function(object) {
    cat("******************************\n",
        object@test,
        "\n",
        rep("-",
            times = nchar(object@test)),
        "\n",
        "H0: ",
        object@hypotheses$h0,
        "; Ha: ",
        object@hypotheses$ha,
        "\n",
        "Equivalence interval: Lower = ",
        object@interval$lower,
        "; Upper = ",
        object@interval$upper,
        "\n",
        "Prior scale: ",
        round(x = object@prior_scale,
              digits = 3),
        "\n\n",
        "    BF = ",
        round(x = object@bf,
              digits = 3),
        "\n",
        "******************************\n",
        sep = "")
  }
)

setMethod(
  f = "show",
  signature = "baymedrNonInferiority",
  definition = function(object) {
    cat("******************************\n",
        object@test,
        "\n",
        rep("-",
            times = nchar(object@test)),
        "\n",
        "H0: ",
        object@hypotheses$h0,
        "; Ha: ",
        object@hypotheses$ha,
        "\n",
        "Non-inferiority margin: ",
        object@ni_margin, "\n",
        "Prior scale: ",
        round(x = object@prior_scale,
              digits = 3),
        "\n\n",
        "    BF = ",
        round(x = object@bf,
              digits = 3),
        "\n",
        "******************************\n",
        sep = "")
  }
)
