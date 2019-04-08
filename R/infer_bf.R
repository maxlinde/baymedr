#' Bayes factor for non-inferiority designs
#'
#' This function computes a Bayes factor for non-inferiority designs.
#'
#' The Bayes factor resulting from \code{\link{infer_bf}} tests the null
#' hypothesis that the experimental group (e.g., a new medication) is not better
#' than the control group (e.g., a placebo or existing medication) minus a
#' constant value (c). The alternative hypothesis is that the experimental group
#' is better than the control group minus a constant value (c). Put more
#' formally, the null hypothesis states that the true population effect size <
#' -c, resulting in the point null hypothesis that the true population effect
#' size = -c against the one-sided alternative hypothesis that the true
#' population effect size > -c.
#'
#' Importantly, \code{\link{infer_bf}} can be utilized to calculate a Bayes
#' factor based on raw data (i.e., if arguments \code{x} and \code{y} are
#' defined) or summary statistics (i.e., if arguments \code{n_x}, \code{n_y},
#' \code{mean_x}, and \code{mean_y} are defined).
#'
#' ##TODO## ni_margin
#'
#' ##TODO## prior_scale
#'
#' @param ni_margin ##TODO##
#' @inheritParams super_bf
#'
#' @return ##TODO##
#'
#' @export
#' @import rlang stats
#'
#' @references Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2018). Informed
#'   bayesian t-tests. Manuscript submitted for publication.
#'
#'   Rouder, J. N., Speckman, P. L., Sun, D., & Morey, R. D. (2009). Bayesian t
#'   tests for accepting and rejecting the null hypothesis. \emph{Psychonomic
#'   Bulletin & Review}, \emph{16}(2), 225-237.
#'
#'   van Ravenzwaaij, D., Monden, R., Tendeiro, J. N., & Ioannidis, J. P. A.
#'   (2019). Bayes factors for superiority, non-inferiority, and equivalence
#'   designs. Manuscript submitted for publication.
#'
#' @examples
#' # infer_bf using raw data:
#' infer_bf(x = rnorm(100, 10, 15),
#'          y = rnorm(130, 13, 10),
#'          ni_margin = -1)
#'
#' # infer_bf using summary statistics:
#' infer_bf(n_x = 100,
#'          n_y = 130,
#'          mean_x = 10,
#'          mean_y = 13,
#'          sd_x = 15,
#'          sd_y = 10,
#'          ni_margin = -1)
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
          !is.null(mean_y),
          !is.null(sd_x),
          !is.null(sd_y))) {
    if (any(is.null(n_x),
            is.null(n_y),
            is.null(mean_x),
            is.null(mean_y),
            is.null(sd_x),
            is.null(sd_y),
            is.null(ni_margin))) {
      abort("All 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', 'sd_y', and
            'ni_margin' must be defined.")
    }
  }
  if (all(!is.null(n_x),
          !is.null(n_y),
          !is.null(mean_x),
          !is.null(mean_y),
          !is.null(sd_x),
          !is.null(sd_y))) {
    data <- list(type = "summary data",
                 data = list(n_x = n_x,
                             n_y = n_y,
                             mean_x = mean_x,
                             mean_y = mean_y,
                             sd_x = sd_x,
                             sd_y = sd_y))
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
  bf <- res[[3]] * (1 / res[[2]])
  test <- "Non-inferiority analysis"
  h0 <- "mu2 - mu1 = ni_margin"
  ha <- "mu2 - mu1 > ni_margin"
  hypotheses <- list(h0 = h0,
                     ha = ha)
  baymedrNonInferiority(test = test,
                        hypotheses = hypotheses,
                        ni_margin = ni_margin,
                        data = data,
                        prior_scale = prior_scale,
                        bf = bf)
}
