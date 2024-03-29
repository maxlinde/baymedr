#' @import methods
setMethod(
  f = "show",
  signature = "baymedrSuperiority",
  definition = function(object) {
    cat("******************************",
        "\n",
        object@test,
        "\n",
        rep("-",
            times = nchar(object@test)),
        "\n",
        "Data:                         ",
        object@data$type,
        "\n",
        "H0 (non-superiority):         ",
        object@hypotheses$h0,
        "\n",
        if (object@hypotheses$h1 == "mu_y < mu_x") {
          "H- (superiority):             "
        },
        if (object@hypotheses$h1 == "mu_y > mu_x") {
          "H+ (superiority):             "
        },
        object@hypotheses$h1,
        "\n",
        "Cauchy prior scale:           ",
        formatC(x = object@prior_scale,
                digits = 3,
                format = "f"),
        "\n\n",
        if (object@hypotheses$h1 == "mu_y < mu_x") {
          "    BF-0 (superiority) = "
        },
        if (object@hypotheses$h1 == "mu_y > mu_x") {
          "    BF+0 (superiority) = "
        },
        if (object@bf > 1 / 1000 && object@bf < 1000) {
          formatC(x = object@bf,
                  digits = 3,
                  format = "f")
        } else {
          formatC(x = object@bf,
                  digits = 3,
                  format = "e")
        },
        "\n",
        "******************************",
        "\n",
        sep = "")
  }
)

setMethod(
  f = "show",
  signature = "baymedrEquivalence",
  definition = function(object) {
    cat("******************************",
        "\n",
        object@test,
        "\n",
        rep("-",
            times = nchar(object@test)),
        "\n",
        "Data:                         ",
        object@data$type,
        "\n",
        "H0 (equivalence):             ",
        object@hypotheses$h0,
        "\n",
        "H1 (non-equivalence):         ",
        object@hypotheses$h1,
        "\n",
        "Equivalence interval:         Lower = ",
        formatC(x = object@interval$lower_std,
                digits = 2,
                format = "f"),
        "; Upper = ",
        formatC(x = object@interval$upper_std,
                digits = 2,
                format = "f"),
        " (standardised)",
        "\n",
        "                              Lower = ",
        formatC(x = object@interval$lower_unstd,
                digits = 2,
                format = "f"),
        "; Upper = ",
        formatC(x = object@interval$upper_unstd,
                digits = 2,
                format = "f"),
        " (unstandardised)",
        "\n",
        "Cauchy prior scale:           ",
        formatC(x = object@prior_scale,
                digits = 3,
                format = "f"),
        "\n\n",
        "    BF01 (equivalence) = ",
        if (object@bf > 1 / 1000 && object@bf < 1000) {
          formatC(x = object@bf,
                  digits = 3,
                  format = "f")
        } else {
          formatC(x = object@bf,
                  digits = 3,
                  format = "e")
        },
        "\n",
        "******************************",
        "\n",
        sep = "")
  }
)

setMethod(
  f = "show",
  signature = "baymedrNonInferiority",
  definition = function(object) {
    cat("******************************",
        "\n",
        object@test,
        "\n",
        rep("-",
            times = nchar(object@test)),
        "\n",
        "Data:                         ",
        object@data$type,
        "\n",
        if (object@hypotheses$h1 == "mu_y - mu_x < ni_margin") {
          paste0("H+ (inferiority):             ",
                 object@hypotheses$h0,
                 "\n",
                 "H- (non-inferiority):         ",
                 object@hypotheses$h1)
        },
        if (object@hypotheses$h1 == "mu_y - mu_x > -ni_margin") {
          paste0("H- (inferiority):             ",
                 object@hypotheses$h0,
                 "\n",
                 "H+ (non-inferiority):         ",
                 object@hypotheses$h1)
        },
        "\n",
        "Non-inferiority margin:       ",
        formatC(x = object@ni_margin$ni_mar_std,
                digits = 2,
                format = "f"),
        " (standardised)",
        "\n",
        "                              ",
        formatC(x = object@ni_margin$ni_mar_unstd,
                digits = 2,
                format = "f"),
        " (unstandardised)",
        "\n",
        "Cauchy prior scale:           ",
        formatC(x = object@prior_scale,
                digits = 3,
                format = "f"),
        "\n\n",
        if (object@hypotheses$h1 == "mu_y - mu_x < ni_margin") {
          "    BF-+ (non-inferiority) = "
        },
        if (object@hypotheses$h1 == "mu_y - mu_x > -ni_margin") {
          "    BF+- (non-inferiority) = "
        },
        if (object@bf > 1 / 1000 && object@bf < 1000) {
          formatC(x = object@bf,
                  digits = 3,
                  format = "f")
        } else {
          formatC(x = object@bf,
                  digits = 3,
                  format = "e")
        },
        "\n",
        "******************************",
        "\n",
        sep = "")
  }
)

setMethod(
  f = "show",
  signature = "baymedrCoxProportionalHazards",
  definition = function(object) {
    cat("******************************",
        "\n",
        object@test,
        "\n",
        rep("-",
            times = nchar(object@test)),
        "\n",
        "H0:              ",
        object@hypotheses$h0,
        "\n",
        if (startsWith(x = object@hypotheses$h1,
                       prefix = "beta <")) {
          "H-:              "
        },
        if (startsWith(x = object@hypotheses$h1,
                       prefix = "beta >")) {
          "H+:              "
        },
        if (startsWith(x = object@hypotheses$h1,
                       prefix = "beta !=")) {
          "H1:              "
        },
        object@hypotheses$h1,
        "\n",
        "Normal prior:    Mean = ",
        formatC(x = object@prior$mean,
                digits = 3,
                format = "f"),
        "\n",
        "                 SD = ",
        formatC(x = object@prior$sd,
                digits = 3,
                format = "f"),
        "\n\n",
        if (startsWith(x = object@hypotheses$h1,
                       prefix = "beta <")) {
          "    BF-0 = "
        },
        if (startsWith(x = object@hypotheses$h1,
                       prefix = "beta >")) {
          "    BF+0 = "
        },
        if (startsWith(x = object@hypotheses$h1,
                       prefix = "beta !=")) {
          "    BF10 = "
        },
        if (object@bf > 1 / 1000 && object@bf < 1000) {
          formatC(x = object@bf,
                  digits = 3,
                  format = "f")
        } else {
          formatC(x = object@bf,
                  digits = 3,
                  format = "e")
        },
        "\n",
        "******************************",
        "\n",
        sep = "")
  }
)

setMethod(
  f = "show",
  signature = "baymedrCoxProportionalHazardsMulti",
  definition = function(object) {
    cat("******************************",
        "\n",
        object@test,
        "\n",
        rep("-",
            times = nchar(object@test)),
        "\n",
        "H0:              ",
        object@hypotheses$h0,
        "\n",
        if (startsWith(x = object@hypotheses$h1,
                       prefix = "beta <")) {
          "H-:              "
        },
        if (startsWith(x = object@hypotheses$h1,
                       prefix = "beta >")) {
          "H+:              "
        },
        if (startsWith(x = object@hypotheses$h1,
                       prefix = "beta !=")) {
          "H1:              "
        },
        object@hypotheses$h1,
        "\n",
        "Normal prior:    Mean = ",
        formatC(x = object@prior$mean,
                digits = 3,
                format = "f"),
        "\n",
        "                 SD = ",
        formatC(x = object@prior$sd,
                digits = 3,
                format = "f"),
        "\n\n",
        if (length(object@bf) > 1) {
          paste0(
            if (startsWith(x = object@hypotheses$h1,
                           prefix = "beta <")) {
              "    Median BF-0 = "
            },
            if (startsWith(x = object@hypotheses$h1,
                           prefix = "beta >")) {
              "    Median BF+0 = "
            },
            if (startsWith(x = object@hypotheses$h1,
                           prefix = "beta !=")) {
              "    Median BF10 = "
            },
            if (median(object@bf) > 1 / 1000 && median(object@bf) < 1000) {
              formatC(x = median(object@bf),
                      digits = 3,
                      format = "f")
            } else {
              formatC(x = median(object@bf),
                      digits = 3,
                      format = "e")
            },
            "\n",
            if (startsWith(x = object@hypotheses$h1,
                           prefix = "beta <")) {
              "    MAD SD BF-0 = "
            },
            if (startsWith(x = object@hypotheses$h1,
                           prefix = "beta >")) {
              "    MAD SD BF+0 = "
            },
            if (startsWith(x = object@hypotheses$h1,
                           prefix = "beta !=")) {
              "    MAD SD BF10 = "
            },
            if (1 / qnorm(3 / 4) * median(abs(object@bf - median(object@bf))) > 1 / 1000 &&
                1 / qnorm(3 / 4) * median(abs(object@bf - median(object@bf))) < 1000) {
              formatC(x = 1 / qnorm(3 / 4) * median(abs(object@bf - median(object@bf))),
                      digits = 3,
                      format = "f")
            } else {
              formatC(x = 1 / qnorm(3 / 4) * median(abs(object@bf - median(object@bf))),
                      digits = 3,
                      format = "e")
            }
          )
        } else {
          paste0(
            if (startsWith(x = object@hypotheses$h1,
                           prefix = "beta <")) {
              "    BF-0 = "
            },
            if (startsWith(x = object@hypotheses$h1,
                           prefix = "beta >")) {
              "    BF+0 = "
            },
            if (startsWith(x = object@hypotheses$h1,
                           prefix = "beta !=")) {
              "    BF10 = "
            },
            if (object@bf > 1 / 1000 && object@bf < 1000) {
              formatC(x = object@bf,
                      digits = 3,
                      format = "f")
            } else {
              formatC(x = object@bf,
                      digits = 3,
                      format = "e")
            }
          )
        },
        "\n",
        "******************************",
        "\n",
        sep = "")
  }
)
