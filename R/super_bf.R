#' Bayes factors for superiority designs
#'
#' This function computes Bayes factors for superiority designs.
#'
#' The Bayes factor resulting from \code{super_bf} tests the null hypothesis
#' that the experimental group (e.g., a new medication) is not better than the
#' control group (e.g., a placebo or existing medication). In other words, the
#' null hypothesis of \code{super_bf} states that the true population effect
#' size is exactly zero, whereas the alternative hypothesis of \code{super_bf}
#' states that the true population effect size is larger than zero (i.e.,
#' \eqn{\mu_control < \mu_experimental}).
#'
#' @param x A vector of numeric observations for the control group.
#' @param y A vector of numeric observations for the experimental group.
#' @param formula A formula specifying the desired model.
#' @param data A \code{data.frame} containing all the data (used in combination
#' with \code{formula}).
#' @param prior_scale A scalar, specifying the scale of the prior distribution
#' (see Details).
#' @param ind_samples A logical value, indicating whether the groups are
#' independent (TRUE) or dependent (FALSE).
#'
#' @return Two Bayes factors are obtained from \code{super_bf}. The first one
#' corresponds to a one-sided alternative hypothesis (i.e.,
#' \eqn{\mu_control < \mu_experimental}); the second one corresponds to a
#' two-sided alternative hypothesis. Importantly, both Bayes factors refer to
#' the evidence in favour of the alternative hypothesis.
#'
#' @export
#' @import rlang stats tibble
#'
#' @references
#' Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2018). Informed bayesian
#' t-tests. Manuscript submitted for publication.
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
#' # super_bf using x and y:
#' super_bf(x = rnorm(100, 10, 15), y = rnorm(130, 13, 10))
super_bf <- function(x = NULL,
                     y = NULL,
                     formula = NULL,
                     data = NULL,
                     prior_scale = 1 / sqrt(2),
                     ind_samples = TRUE) {
  if (!is.null(formula) && !is.null(x)) {
    abort("You must define only one of 'formula' or 'x'.")
  }
  if (xor(is.null(x), is.null(y))) {
    abort("Both 'x' and 'y' must be defined.")
  }
  if (!is.null(data)) {
    if (inherits(x = data,
                 what = c("tbl_df", "tbl", "data.frame"))) {
      data <- as_tibble(data)
      #warn("'data' is converted to tibble.")
    }
  }
  if (!is.null(formula) && is.null(data)) {
    abort("'data' must be defined when 'formula' is used.")
  }
  if (!is.null(x)) {
    if (any(is.na(x))) {
      abort("'x' must not contain any missing values.")
    }
    if (any(!is.finite(x))) {
      abort("'x' must not contain any infinite values.")
    }
  }
  if (!is.null(y)) {
    if (any(is.na(y))) {
      abort("'y' must not contain any missing values.")
    }
    if (any(!is.finite(y))) {
      abort("'y' must not contain any infinite values.")
    }
  }
  n_x <- length(x)
  n_y <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  sd_x <- sd(x)
  sd_y <- sd(y)
  sd_pooled <- sqrt(((n_x - 1) * sd_x ^ 2 + (n_y - 1) * sd_y ^ 2) /
                      (n_x + n_y - 2))
  se <- sd_pooled * sqrt(1 / n_x + 1 / n_y)
  t_stat <- (mean_x - mean_y) / se
  res = bf10_t(t = t_stat,
               n1 = n_x,
               n2 = n_y,
               ind_samples = ind_samples,
               prior_loc = 0,
               prior_scale = prior_scale,
               prior_df = 1)
  bf_sup1 = res[[3]]
  bf_sup2 = res[[1]]
  names(bf_sup1) = "BFsup1"
  names(bf_sup2) = "BFsup2"
  c(bf_sup1, bf_sup2)
}
