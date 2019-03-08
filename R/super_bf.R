#' Bayes factors for superiority designs
#'
#' This function computes Bayes factors for superiority designs.
#'
#' The Bayes factor resulting from \code{super_bf} tests the null hypothesis
#' that the experimental group (e.g., a new medication) is not better than the
#' control group (e.g., a placebo or existing medication). In other words, the
#' null hypothesis states that the true population effect size is exactly zero.
#'
#'
#' @param formula Bla
#' @param x Bla
#' @param y Bla
#' @param data Bla
#'
#' @return Bla
#' @export
#' @import rlang tibble
#'
#' @references
#' van Ravenzwaaij, D., Monden, R., Tendeiro, J. N., & Ioannidis, J. P. A.
#' (2019). Bayes factors for superiority, non-inferiority, and equivalence
#' designs. Manuscript submitted for publication.
#'
#' Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2018). Informed bayesian
#' t-tests. Manuscript submitted for publication.
#'
#' @examples
#' Bla
super_bf <- function(formula = NULL,
                     x = NULL,
                     y = NULL,
                     data = NULL) {
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
      warn("'data' is converted to tibble.")
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
               ind_samples = TRUE,
               prior_loc = 0,
               prior_scale = 1 / sqrt(2),
               prior_df = 1)
  bf_sup1 = res[[3]]
  bf_sup2 = res[[1]]
  names(bf_sup1) = "BFsup1"
  names(bf_sup2) = "BFsup2"
  return(c(bf_sup1, bf_sup2))
}
