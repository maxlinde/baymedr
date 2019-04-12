#' Bayes factor for equivalence designs
#'
#' This function computes a Bayes factor for equivalence designs.
#'
#' The Bayes factor resulting from \code{\link{equiv_bf}} tests the null
#' hypothesis that the control group (e.g., a placebo or existing medication)
#' and the experimental group (e.g., a new medication) are equivalent. The
#' alternative hypothesis is that the two groups are not equivalent.
#' Importantly, with \code{\link{equiv_bf}}, the goal is to express evidence in
#' favour of the null hypothesis (equivalence). Thus, in contrast to other tests
#' (e.g., \code{\link{super_bf}} and \code{\link{infer_bf}}), the Bayes factor
#' resulting from \code{\link{equiv_bf}} expresses evidence in favour of the
#' null hypothesis. Quantification of evidence in favour of the null hypothesis
#' is logically sound and legitimate within the Bayesian framework but not in
#' the traditional frequentist framework (see e.g., van Ravenzwaaij et al.
#' (2019)).
#'
#' In contrast to null hypothesis significance testing (NHST),
#' \code{\link{equiv_bf}} has the advantage that it is not compulsory to specify
#' an equivalence interval (see van Ravenzwaaij et al., 2019). Therefore, the
#' default value of the argument \code{interval} is 0, indicating a point null
#' hypothesis. However, if the user prefers to have an equivalence interval, the
#' argument \code{interval} can be set in two ways: If a \emph{symmetric}
#' interval is desired, the user can either specify a numeric vector of length
#' one (e.g., 0.1, which is converted to c(-0.1, 0.1)) or a numeric vector of
#' length two (e.g., c(-0.1, 0.1)); if an \emph{asymmetric} interval is desired,
#' the user can specify a numeric vector of length two (e.g., c(-0.1, 0.2)).
#'
#' Importantly, \code{\link{equiv_bf}} can be utilized to calculate a Bayes
#' factor based on raw data (i.e., if arguments \code{x} and \code{y} are
#' defined) or summary statistics (i.e., if arguments \code{n_x}, \code{n_y},
#' \code{mean_x}, and \code{mean_y} are defined). In the latter case, the user
#' has the freedom to supply values either for the arguments \code{sd_x} and
#' \code{sd_y} \strong{OR} \code{ci_margin}. The choice should depend on the
#' information that is available to the user.
#'
#' ##TODO## prior_scale
#'
#' @param interval A numeric vector of length one or two, specifying the
#'   boundaries of the equivalence interval in unstandardized units (see van
#'   Ravenzwaaij et al., 2019). If a numeric vector of length one is specified,
#'   a symmetric equivalence interval will be used (e.g., a 0.1 is equivalent
#'   to c(-0.1, 0.1)). A numeric vector of length two provides the possibility
#'   to specify an asymmetric equivalence interval (e.g., c(-0.1, 0.2)). The
#'   default is 0, indicating a point null hypothesis rather than an interval
#'   (see Details).
#' @inheritParams super_bf
#'
#' @return ##TODO##
#'
#' @export
#' @import rlang stats stringr
#'
#' @references Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2019). Informed
#'   bayesian t-tests. \emph{The American Statistician}, 1-13.
#'
#'   Rouder, J. N., Speckman, P. L., Sun, D., & Morey, R. D. (2009). Bayesian t
#'   tests for accepting and rejecting the null hypothesis. \emph{Psychonomic
#'   Bulletin & Review}, \emph{16}(2), 225-237.
#'
#'   van Ravenzwaaij, D., Monden, R., Tendeiro, J. N., & Ioannidis, J. P. A.
#'   (2019). Bayes factors for superiority, non-inferiority, and equivalence
#'   designs. \emph{BMC Medical Research Methodology}, \emph{19}(1), 71.
#'
#' @examples
#' ## equiv_bf using raw data:
#'
#' # Assign model to variable.
#' equiv_raw <- equiv_bf(x = rnorm(100, 10, 15),
#'                           y = rnorm(130, 13, 10))
#'
#' # Extract Bayes factor from variable.
#' get_bf(equiv_raw)
#'
#' # ----------
#' # ----------
#'
#' ## equiv_bf using summary statistics with data from Steiner et al. (2015).
#' ## With a point null hypothesis:
#'
#' # Assign model to variable.
#' equiv_sum_point <- equiv_bf(n_x = 538,
#'                             n_y = 560,
#'                             mean_x = 8.516,
#'                             mean_y = 8.683,
#'                             sd_x = 3.6,
#'                             sd_y = 3.6)
#'
#' # Extract Bayes factor from model.
#' get_bf(equiv_sum_point)
#'
#' # ----------
#' # ----------
#'
#' ## equiv_bf using summary statistics with data from Steiner et al. (2015).
#' ## With an interval null hypothesis:
#'
#' # Assign model to variable.
#' equiv_sum_interval <- equiv_bf(n_x = 538,
#'                                n_y = 560,
#'                                mean_x = 8.516,
#'                                mean_y = 8.683,
#'                                sd_x = 3.6,
#'                                sd_y = 3.6,
#'                                interval = 0.05)
#'
#' # Extract Bayes factor from model.
#' get_bf(equiv_sum_interval)
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
    abort(str_c(
      "Only 'x' and 'y' OR 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', and ",
      "'sd_y' (or 'ci_margin' instead of 'sd_x' and 'sd_y') must be defined."
    ))
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
      abort(str_c(
        "All 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', and 'sd_y' (or ",
        "'ci_margin' instead of 'sd_x' and 'sd_y') must be defined."
      ))
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
    h0 <- "mu_y == mu_x"
    ha <- "mu_y != mu_x"
  } else {
    cdf_t_upper <- cdf_t(x = interval[[2]],
                         t = t_stat,
                         n1 = n_x,
                         n2 = n_y,
                         ind_samples = TRUE,
                         prior_loc = 0,
                         prior_scale = prior_scale,
                         prior_df = 1)
    cdf_t_lower <- cdf_t(x = interval[[1]],
                         t = t_stat,
                         n1 = n_x,
                         n2 = n_y,
                         ind_samples = TRUE,
                         prior_loc = 0,
                         prior_scale = prior_scale,
                         prior_df = 1)
    if (cdf_t_upper > 1) {
      cdf_t_upper <- 1
      warn(str_c(
        "Caution: An approximation for the integral is invoked. The resulting ",
        "Bayes factor is very large and is thus set to Inf."
      ))
    }
    if (cdf_t_lower < 0) {
      cdf_t_lower <- 0
      warn(str_c(
        "Caution: An approximation for the integral is invoked. The resulting ",
        "Bayes factor is very large and is thus set to Inf."
      ))
    }
    post_dens <- cdf_t_upper - cdf_t_lower
    prior_dens <- pcauchy(q = interval[[2]],
                          scale = prior_scale) - pcauchy(q = interval[[1]],
                                                         scale = prior_scale)
    bf <- (post_dens / prior_dens) /
      ((1 - post_dens) / (1 - prior_dens))
    h0 <- "mu_y - mu_x > c_low AND mu_y - mu_x < c_high"
    ha <- "mu_y - mu_x < c_low OR mu_y - mu_x > c_high"
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
