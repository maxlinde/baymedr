#' Bayes factor for non-inferiority designs
#'
#' This function computes a Bayes factor for non-inferiority designs.
#'
#' The Bayes factor resulting from \code{infer_bf} tests the null hypothesis
#' that the experimental group (e.g., a new medication) is not better than the
#' control group (e.g., a placebo or existing medication) minus a constant
#' value (c). The alternative hypothesis is that the experimental group is
#' better than the control group minus a constant value (c). Put more formally,
#' the null hypothesis states that the true population effect size < -c,
#' resulting in the point null hypothesis that the true population effect size
#' = -c against the one-sided alternative hypothesis that the true population
#' effect size > -c.
#'
#' Importantly, \code{infer_bf} can be utilized to calculate Bayes factors
#' based on raw data (i.e., if arguments 'x' and 'y' are defined) or summary
#' statistics (i.e., if arguments 'n_x', 'n_y', 'mean_x', and 'mean_y' are
#' defined).
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
#' variable in the control group.
#' @param sd_y A scalar, specifying the standard deviation of the dependent
#' variable in the experimental group.
#' @param ni_margin ##TODO##
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
#' # infer_bf using raw data:
#' infer_bf(x = rnorm(100, 10, 15), y = rnorm(130, 13, 10), ni_margin = 2)
#'
#' # infer_bf using summary statistics:
#' infer_bf(n_x = 100, n_y = 130,
#'          mean_x = 10, mean_y = 13,
#'          sd_x = 15, sd_y = 10,
#'          ni_margin = 2)
infer_bf <- function(x = NULL,
                     y = NULL,
                     n_x = NULL,
                     n_y = NULL,
                     mean_x = NULL,
                     mean_y = NULL,
                     sd_x = NULL,
                     sd_y = NULL,
                     ni_margin = NULL,
                     prior_scale = 1 / sqrt(2)) {
  if (any(!is.null(x),
          !is.null(y)) && any(!is.null(n_x),
                              !is.null(n_y),
                              !is.null(mean_x),
                              !is.null(mean_y),
                              !is.null(sd_x),
                              !is.null(sd_y))) {
    abort("Only 'x', 'y', and 'ni_margin' OR 'n_x', 'n_y', 'mean_x', 'mean_y',
          'sd_x', 'sd_y', and 'ni_margin' must be defined.")
  }
  if (any(!is.null(x),
          !is.null(y))) {
    if (any(is.null(x),
            is.null(y),
            is.null(ni_margin))) {
      abort("All 'x', 'y', and 'ni_margin' must be defined.")
    }
  }
  if (any(!is.null(n_x),
          !is.null(n_y),
          !is.null(mean_x),
          !is.null(mean_y))) {
    if (any(is.null(n_x),
            is.null(n_y),
            is.null(mean_x),
            is.null(mean_y),
            is.null(ni_margin))) {
      abort("All 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', 'sd_y', and
            'ni_margin' must be defined.")
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
  sd_pooled <- sqrt(((n_x - 1) * sd_x ^ 2 + (n_y - 1) * sd_y ^ 2) /
                      (n_x + n_y - 2))
  se <- sd_pooled * sqrt(1 / n_x + 1 / n_y)
  cohen_d <- ni_margin / sd_pooled
  t_stat <- (mean_x - mean_y - ni_margin) / se
  res <- bf10_t(t = t_stat,
                n1 = n_x,
                n2 = n_y,
                ind_samples = TRUE,
                prior_loc = cohen_d,
                prior_scale = prior_scale,
                prior_df = 1)
  ni_bf <- res[[3]] * (1 / res[[2]])
  names(ni_bf) <- "BF non-inferiority"
  return(ni_bf)
}
