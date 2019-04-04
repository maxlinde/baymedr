#' Bayes factor for equivalence designs
#'
#' This function computes a Bayes factor for equivalence designs.
#'
#' The Bayes factor resulting from \code{equiv_bf} tests the null hypothesis
#' that the control group (e.g., a placebo or existing medication) and the
#' experimental group (e.g., a new medication) are equivalent. The alternative
#' hypothesis is that the two groups are not equivalent.
#'
#' In contrast to null hypothesis significance testing (NHST), \code{equiv_bf}
#' has the advantage that it is not compulsory to specify an equivalence
#' interval (see van Ravenzwaaij et al., 2019). Therefore, the default value of
#' the argument \code{interval} is 0, indicating a point null hypothesis.
#' However, if the user prefers to have a equivalence interval, the argument
#' \code{interval} can be set with a positive number, specifying the upper
#' bound of a symmetric equivalence interval.
#'
#' Importantly, \code{equiv_bf} can be utilized to calculate Bayes factors
#' based on raw data (i.e., if arguments 'x' and 'y' are defined) or summary
#' statistics (i.e., if arguments 'n_x', 'n_y', 'mean_x', and 'mean_y' are
#' defined). In the latter case, the user has the freedom to supply values
#' either for the arguments 'sd_x' and 'sd_y' \strong{OR} 'ci_margin'. The
#' choice should depend on the information that is available to the user.
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
#' variable in the control group. Only \code{sd_x} and \code{sd_y} \strong{OR}
#' \code{ci_margin} should be defined (see Details).
#' @param sd_y A scalar, specifying the standard deviation of the dependent
#' variable in the experimental group. Only \code{sd_x} and \code{sd_y}
#' \strong{OR} \code{ci_margin} should be defined (see Details).
#' @param ci_margin A scalar, specifying the margin of the confidence interval
#' (i.e., the width of the confidence interval divided by 2) of the difference
#' on the dependent variable between the control and experimental groups. Only
#' \code{sd_x} and \code{sd_y} \strong{OR} \code{ci_margin} should be defined
#' (see Details).
#' @param interval A numeric vector of length 1 or 2, specifying the upper
#' bound of the equivalence interval in unstandardized units
#' (see van Ravenzwaaij et al., 2019). If a numeric vector of length 1 is
#' specified, a symmetric equivalence interval will be used
#' (e.g., a 0.5 is equivalent to c(-0.5, 0.5)). A numeric vector of length 2
#' provides the possibility to specify a asymmetric equivalence interval
#' (e.g., c(-0.3, 0.7)). The default is 0, indicating a point null hypothesis
#' rather than an interval (see Details).
#' @param prior_scale A scalar, specifying the scale of the prior distribution
#' (see Details). The default value is \eqn{1 / \sqrt{2}}
#' (see Rouder et al., 2009).
#'
#' @return ##TODO##
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
#' # equiv_bf using raw data:
#' equiv_bf(x = rnorm(100, 10, 15), y = rnorm(130, 13, 10))
#'
#' # equiv_bf using summary statistics. The case where sd_x and sd_y are known:
#' equiv_bf(n_x = 100, n_y = 130,
#'          mean_x = 10, mean_y = 13,
#'          sd_x = 15, sd_y = 10)
#'
#' # equiv_bf using summary statistics. The case where sd_x and sd_y are not
#' # known:
#' equiv_bf(n_x = 100, n_y = 130,
#'          mean_x = 10, mean_y = 13,
#'          ci_margin = 4)
equiv_bf <- function(x = NULL,
                     y = NULL,
                     n_x = NULL,
                     n_y = NULL,
                     mean_x = NULL,
                     mean_y = NULL,
                     sd_x = NULL,
                     sd_y = NULL,
                     ci_margin = NULL,
                     interval = 0,
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
  if (all(!is.null(n_x),
          !is.null(n_y),
          !is.null(mean_x),
          !is.null(mean_y)) && (xor(!is.null(sd_x) && !is.null(sd_y),
                                    !is.null(ci_margin)))) {
    data <- list(type = "summary data",
                 data = list(n_x = n_x,
                             n_y = n_y,
                             mean_x = mean_x,
                             mean_y = mean_y,
                             sd_x = sd_x,
                             sd_y = sd_y,
                             ci_margin = ci_margin))
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
    data <- list(type = "raw data",
                 data = list(x = x,
                             y = y))
  }
  if (!is.numeric(prior_scale) || length(prior_scale) > 1) {
    abort("'prior_scale' must be a single numeric value.")
  }
  if (!is.numeric(interval) || length(interval) > 2) {
    abort("'interval' must be a numeric vector of length one or two.")
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
  if (length(interval) == 1) {
    interval <- c(-interval, interval)
  }
  if (identical(interval, c(0, 0))) {
    res <- bf10_t(t = t_stat,
                  n1 = n_x,
                  n2 = n_y,
                  ind_samples = TRUE,
                  prior_loc = 0,
                  prior_scale = prior_scale,
                  prior_df = 1)
    bf <- 1 / res[[1]]
    h0 <- "mu2 = mu1"
    ha <- "mu2 != mu1"
  } else {
    post_dens <- cdf_t(x = interval[[2]],
                       t = t_stat,
                       n1 = n_x,
                       n2 = n_y,
                       ind_samples = TRUE,
                       prior_loc = 0,
                       prior_scale = prior_scale,
                       prior_df = 1) - cdf_t(x = interval[[1]],
                                             t = t_stat,
                                             n1 = n_x,
                                             n2 = n_y,
                                             ind_samples = TRUE,
                                             prior_loc = 0,
                                             prior_scale = prior_scale,
                                             prior_df = 1)
    prior_dens <- pcauchy(q = interval[[2]],
                          scale = prior_scale) - pcauchy(q = interval[[1]],
                                                         scale = prior_scale)
    bf <- (post_dens / prior_dens) /
      ((1 - post_dens) / (1 - prior_dens))
    h0 <- "c_low < mu2 - mu1 < c_high"
    ha <- "c_low !< mu2 - mu1 !< c_high"
  }
  test <- "Equivalence analysis"
  hypotheses <- list(h0 = h0,
                     ha = ha)
  interval <- list(lower = interval[[1]],
                   upper = interval[[2]])
  baymedrEquivalence(test = test,
                     hypotheses = hypotheses,
                     interval = interval,
                     data = data,
                     prior_scale = prior_scale,
                     bf = bf)
}
