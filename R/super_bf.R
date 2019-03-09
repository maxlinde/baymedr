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
#' @param x A vector of numeric observations for the control group.
#' @param y A vector of numeric observations for the experimental group.
#' @param formula A formula specifying the desired model.
#' @param data A \code{data.frame} containing all the data (used in combination
#'   with \code{formula}).
#' @param prior_scale A scalar, specifying the scale of the prior distribution
#'   (see Details).
#' @param ind_samples A logical value, indicating whether the groups are
#'   independent (TRUE; the default) or dependent (FALSE).
#'
#' @return Two Bayes factors are obtained from \code{super_bf}. The first one
#'   corresponds to a one-tailed alternative hypothesis (i.e., \eqn{\mu_control
#'   < \mu_experimental}), whereas the second one corresponds to a two-tailed
#'   alternative hypothesis. This is done to accomodate different research
#'   practices, with some researchers employing a one-tailed and some a
#'   two-tailed test. Importantly, both Bayes factors refer to the evidence in
#'   favour of the alternative hypothesis.
#'
#' @export
#' @import rlang stats tibble
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
  if (!is.null(x)) {
    if (any(is.na(x)) || any(!is.finite(x))) {
      abort("'x' must not contain any missing or infinite values.")
    }
  }
  if (!is.null(y)) {
    if (any(is.na(y)) || any(!is.finite(y))) {
      abort("'y' must not contain any missing or infinite values.")
    }
  }
  if (isFALSE(ind_samples) && (length(x) != length(y))) {
    abort("If 'ind_samples' is FALSE, 'x' and 'y' must have the same length.")
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    abort("'x' and 'y' must be numeric.")
  }
  if (!is.null(data)) {
    if (inherits(x = data,
                 what = c("tbl_df", "tbl", "data.frame"))) {
      data <- as_tibble(data)
      message("'data' is converted to tibble.")
    } else {
      abort("'data' must be a data.frame or a tibble.")
    }
  }
  if (!is.null(formula) && is.null(data)) {
    abort("'data' must be defined when 'formula' is used.")
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
  bf_onetailed = res[[3]]
  bf_twotailed = res[[1]]
  names(bf_onetailed) = "BF one-tailed"
  names(bf_twotailed) = "BF two-tailed"
  c(bf_onetailed, bf_twotailed)
}
