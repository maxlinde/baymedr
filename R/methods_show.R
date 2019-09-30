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
        "Data: ",
        object@data$type,
        "\n",
        "H0 (non-superiority): ",
        object@hypotheses$h0,
        "\n",
        "H1 (superiority):     ",
        object@hypotheses$h1,
        "\n",
        "Prior scale: ",
        formatC(x = object@prior_scale,
                digits = 3,
                format = "f"),
        "\n\n",
        "    BF10 (superiority) = ",
        if (object@bf > 1 / 1000 && object@bf < 1000) {
          formatC(x = object@bf,
                  digits = 2,
                  format = "f")
        } else {
          formatC(x = object@bf,
                  digits = 2,
                  format = "e")
        },
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
        "Data: ",
        object@data$type,
        "\n",
        "H0 (equivalence):     ",
        object@hypotheses$h0,
        "\n",
        "H1 (non-equivalence): ",
        object@hypotheses$h1,
        "\n",
        "Equivalence interval: Lower = ",
        object@interval$lower,
        "; Upper = ",
        object@interval$upper,
        "\n",
        "Prior scale: ",
        formatC(x = object@prior_scale,
                digits = 3,
                format = "f"),
        "\n\n",
        "    BF01 (equivalence) = ",
        if (object@bf > 1 / 1000 && object@bf < 1000) {
          formatC(x = object@bf,
                  digits = 2,
                  format = "f")
        } else {
          formatC(x = object@bf,
                  digits = 2,
                  format = "e")
        },
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
        "Data: ",
        object@data$type,
        "\n",
        "H0 (inferiority):     ",
        object@hypotheses$h0,
        "\n",
        "H1 (non-inferiority): ",
        object@hypotheses$h1,
        "\n",
        "Non-inferiority margin: ",
        object@ni_margin, "\n",
        "Prior scale: ",
        formatC(x = object@prior_scale,
                digits = 3,
                format = "f"),
        "\n\n",
        "    BF10 (non-inferiority) = ",
        if (object@bf > 1 / 1000 && object@bf < 1000) {
          formatC(x = object@bf,
                  digits = 2,
                  format = "f")
        } else {
          formatC(x = object@bf,
                  digits = 2,
                  format = "e")
        },
        "\n",
        "******************************\n",
        sep = "")
  }
)
