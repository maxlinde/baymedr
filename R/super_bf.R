#' Bayes factors for superiority designs
#'
#' This function computes Bayes factors for superiority designs.
#'
#' The Bayes factor resulting from \code{super_bf} tests the null hypothesis
#' that the experimental group (e.g., a new medication) is not better than the
#' control group (e.g., a placebo or existing medication). In other words, the
#' null hypothesis of \code{super_bf} states that the true population effect
#' size is exactly zero.
#'
#' As the name 'superiority' implies, the test is typically understood as a
#' one-tailed test. In practice, however, a two-tailed test is often employed.
#' To cover both research practices, \code{super_bf} calculates two Bayes
#' factors, one for each alternative hypothesis (see Value).
#'
#' Importantly, \code{super_bf} can be utilized to calculate Bayes factors
#' based on raw data (i.e., if arguments x and y are defined) or summary
#' statistics (i.e., if arguments n_x, n_y, mean_x, and mean_y are defined). In
#' the latter case, the user has the freedom to supply values either for the
#' arguments sd_x and sd_y OR ci_margin. The choice should depend on the
#' information that is available to the user.
#'
#'
#'
#' @param x A vector of numeric observations for the control group.
#' @param y A vector of numeric observations for the experimental group.
#' @param n_x A scalar, specifying the sample size of the control group.
#' @param n_y A scalar, specifying the sample size of the experimental group.
#' @param mean_x A scalar, specifying the mean of the dependent variable in the
#' control group.
#' @param mean_y A scalar, specifying the mean of the dependent variable in the
#' experimental group.
#' @param sd_x A scalar, specifying the standard deviation of the dependent
#' variable in the control group. Only sd_x and sd_y OR ci_margin should be
#' defined (see Details).
#' @param sd_y A scalar, specifying the standard deviation of the dependent
#' variable in the experimental group. Only sd_x and sd_y OR ci_margin should be
#' defined (see Details).
#' @param ci_margin A scalar, specifying the margin of the confidence interval
#' (i.e., the width of the confidence interval divided by 2) of the difference
#' on the dependent variable between the control and experimental groups. Only
#' sd_x and sd_y OR ci_margin should be defined (see Details).
#' @param prior_scale A scalar, specifying the scale of the prior distribution
#'   (see Details). The default value is 1 / sqrt(2) (see Rouder et al., 2009).
#'
#' @return Two Bayes factors are obtained from \code{super_bf}. The first one
#'   corresponds to a one-tailed alternative hypothesis (i.e., \eqn{\mu_control
#'   < \mu_experimental}), whereas the second one corresponds to a two-tailed
#'   alternative hypothesis. This is done to accomodate different research
#'   practices, with some researchers employing a one-tailed and others a
#'   two-tailed test. Importantly, both Bayes factors refer to the evidence in
#'   favour of the alternative hypothesis.
#'
#' @export
#' @import rlang stats
#'
#' @references
#' Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2018). Informed
#' bayesian t-tests. Manuscript submitted for publication.
#'
#' Rouder, J. N., Speckman, P. L., Sun, D., & Morey, R. D. (2009). Bayesian t
#' tests for accepting and rejecting the null hypothesis. \emph{Psychonomic
#' Bulletin & Review}, \emph{16}(2), 225-237.
#'
#' van Ravenzwaaij, D., Monden, R., Tendeiro, J. N., & Ioannidis, J. P. A.
#' (2019). Bayes factors for superiority, non-inferiority, and equivalence
#' designs. Manuscript submitted for publication.
#'
#' @examples
#' # super_bf using raw data:
#' super_bf(x = rnorm(100, 10, 15), y = rnorm(130, 13, 10))
#'
#' # super_bf using summary statistics. The case where sd_x and sd_y are known:
#' super_bf(n_x = 100, n_y = 130,
#'          mean_x = 10, mean_y = 13,
#'          sd_x = 15, sd_y = 10)
#'
#' # super_bf using summary statistics. The case where sd_x and sd_y are not
#' # known:
#' super_bf(n_x = 100, n_y = 130,
#'          mean_x = 10, mean_y = 13,
#'          ci_margin = 4)
super_bf <- function(x = NULL,
                     y = NULL,
                     n_x = NULL,
                     n_y = NULL,
                     mean_x = NULL,
                     mean_y = NULL,
                     sd_x = NULL,
                     sd_y = NULL,
                     ci_margin = NULL,
                     prior_scale = 1 / sqrt(2)) {
  if (any(!is.null(x),
          !is.null(y)) && any(!is.null(n_x),
                              !is.null(n_y),
                              !is.null(mean_x),
                              !is.null(mean_y),
                              !is.null(sd_x),
                              !is.null(sd_y),
                              !is.null(ci_margin))) {
    abort("Only 'x', and 'y' OR 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', and
          'sd_y' (or 'ci_margin' instead of 'sd_x' and 'sd_y') must be
          defined.")
  }
  if (xor(!is.null(x),
          !is.null(y))) {
    abort("Both 'x' and 'y' must be defined.")
  }
  if (any(!is.null(n_x),
          !is.null(n_y),
          !is.null(mean_x),
          !is.null(mean_y))) {
    if (any(is.null(n_x),
            is.null(n_y),
            is.null(mean_x),
            is.null(mean_y)) ||
        ((is.null(sd_x) || is.null(sd_y)) && is.null(ci_margin))) {
      abort("All 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', and 'sd_y' (or
            'ci_margin' instead of 'sd_x' and 'sd_y') must be defined.")
    }
    if (!xor(!is.null(sd_x) && !is.null(sd_y),
             !is.null(ci_margin))) {
      abort("Only 'sd_x' and 'sd_y' OR 'ci_margin' must be defined.")
    }
  }
  if (!is.null(x) && !is.null(y)) {
    if (any(is.na(x)) || any(is.na(y))) {
      abort("'x' and 'y' must not contain missing values.")
    }
    if (any(is.infinite(x)) || any(is.infinite(y))) {
      abort("'x' and 'y' must not contain infinite values.")
    }
    if (!is.numeric(x) || !is.numeric(y)) {
      abort("'x' and 'y' must be numeric vectors.")
    }
    n_x <- length(x)
    n_y <- length(y)
    mean_x <- mean(x)
    mean_y <- mean(y)
    sd_x <- sd(x)
    sd_y <- sd(y)
  }
  if (!is.null(sd_x) && !is.null(sd_y)) {
    sd_pooled <- sqrt(((n_x - 1) * sd_x ^ 2 + (n_y - 1) * sd_y ^ 2) /
                        (n_x + n_y - 2))
    se <- sd_pooled * sqrt(1 / n_x + 1 / n_y)
  } else {
    se <- ci_margin / qt(p = 0.975,
                         df = n_x + n_y - 2)
  }
  t_stat <- (mean_x - mean_y) / se
  res = bf10_t(t = t_stat,
               n1 = n_x,
               n2 = n_y,
               ind_samples = TRUE,
               prior_loc = 0,
               prior_scale = prior_scale,
               prior_df = 1)
  bf_onetailed = res[[3]]
  bf_twotailed = res[[1]]
  names(bf_onetailed) = "BF one-tailed"
  names(bf_twotailed) = "BF two-tailed"
  c(bf_onetailed, bf_twotailed)
}
