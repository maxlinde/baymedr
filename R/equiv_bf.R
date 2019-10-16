#' Bayes factor for equivalence designs
#'
#' \code{\link{equiv_bf}} computes a Bayes factor for equivalence designs.
#'
#' The Bayes factor resulting from \code{\link{equiv_bf}} tests the null
#' hypothesis that the experimental group (e.g., a new medication) and the
#' control group (e.g., a placebo or an already existing medication) are
#' equivalent. The alternative hypothesis is that the two groups are not
#' equivalent.
#'
#' Since the main goal of \code{\link{equiv_bf}} is to establish equivalence,
#' the resulting Bayes factor quantifies evidence in favour of the null
#' hypothesis (i.e., BF01). However, evidence for the alternative hypothesis can
#' easily be calculated by taking the reciprocal of the original Bayes factor
#' (i.e., BF10 = 1 / BF01). Quantification of evidence in favour of the null
#' hypothesis is logically sound and legitimate within the Bayesian framework
#' but not in the traditional frequentist framework (see e.g., van Ravenzwaaij
#' et al., 2019).
#'
#' Importantly, \code{\link{equiv_bf}} can be utilized to calculate a Bayes
#' factor based on raw data (i.e., if arguments \code{x} and \code{y} are
#' defined) or summary statistics (i.e., if arguments \code{n_x}, \code{n_y},
#' \code{mean_x}, and \code{mean_y} are defined). In the latter case, the user
#' has the freedom to supply values either for the arguments \code{sd_x} and
#' \code{sd_y} \strong{OR} \code{ci_margin} and \code{ci_level}. The choice
#' should depend on the information that is available to the user. Arguments
#' with 'x' as a name or suffix correspond to the control group, whereas
#' arguments with 'y' as a name or suffix correspond to the experimental group.
#'
#' Using the argument \code{interval}, the user can specify the equivalence
#' interval. In contrast to null hypothesis significance testing (NHST),
#' \code{\link{equiv_bf}} has the advantage that it is not compulsory to specify
#' an equivalence interval (see van Ravenzwaaij et al., 2019). Therefore, the
#' default value of the argument \code{interval} is 0, indicating a point null
#' hypothesis. However, if the user prefers to have an equivalence interval, the
#' argument \code{interval} can be set in two ways: If a \emph{symmetric}
#' interval is desired, the user can either specify a numeric vector of length
#' one (e.g., 0.1, which is converted to c(-0.1, 0.1)) or a numeric vector of
#' length two (e.g., c(-0.1, 0.1)); if an \emph{asymmetric} interval is desired,
#' the user can specify a numeric vector of length two (e.g., c(-0.1, 0.2)). It
#' can be specified whether the equivalence interval (i.e., \code{interval}) is
#' given in standardised or unstandardised units with the \code{interval_std}
#' argument, where TRUE, corresponding to standardised units, is the default.
#'
#' For the calculation of the Bayes factor, we chose a Cauchy prior density for
#' the effect size under the alternative hypothesis. The shape of the Cauchy
#' distribution can be manipulated with its location and scale parameters. The
#' standard Cauchy distribution, with a location parameter of 0 and a scale
#' parameter of 1, resembles a standard Normal distribution, except that the
#' Cauchy distribution has less mass at the centre but heavier tails (see, e.g.,
#' Rouder et al., 2009, for a visualisation). Mathematically, the standard
#' Cauchy distribution is equivalent to a Normal distribution with a mean of 0
#' and a variance that follows and inverse chi-square distribution with one
#' degree of freedom, for which the variance is integrated out (Liang et al.,
#' 2008). The argument \code{prior_scale} specifies the width of the Cauchy
#' prior, which corresponds to half of the interquartile range. Thus, by
#' adjusting the Cauchy prior scale with \code{prior_scale}, we can emphasise
#' different ranges of effect sizes that might be expected. The default prior
#' scale is set to r = 1 / sqrt(2).
#'
#' \code{\link{equiv_bf}} creates an S4 object of class
#' \linkS4class{baymedrEquivalence}, which has multiple slots/entries (e.g.,
#' type of data, prior scale, Bayes factor, etc.; see Value). If it is desired
#' to store or extract solely the Bayes factor, the user can do this with
#' \code{\link{get_bf}}, by setting the S4 object as an argument (see Examples).
#'
#' @param interval A numeric vector of length one or two, specifying the
#'   boundaries of the equivalence interval. If a numeric vector of length one
#'   is specified, a symmetric equivalence interval will be used (e.g., a 0.1 is
#'   equivalent to c(-0.1, 0.1)). A numeric vector of length two provides the
#'   possibility to specify an asymmetric equivalence interval (e.g., c(-0.1,
#'   0.2)). The default is 0, indicating a point null hypothesis rather than an
#'   interval (see Details).
#' @param interval_std A logical vector of length one, specifying whether the
#'   equivalence interval (i.e., \code{interval}) is given in standardised
#'   (TRUE; the default) or unstandardised (FALSE) units.
#' @inheritParams super_bf
#'
#' @return An S4 object of class \linkS4class{baymedrEquivalence} is returned.
#'   Contained are a description of the model and the resulting Bayes factor:
#'   \itemize{ \item test: The type of analysis \item hypotheses: A statement of
#'   the hypotheses \itemize{ \item h0: The null hypothesis \item h1: The
#'   alternative hypothesis} \item interval: Specification of the equivalence
#'   interval in standardised and unstandardised units \itemize{ \item
#'   lower_std: The standardised lower boundary of the equivalence interval
#'   \item upper_std: The standardised upper boundary of the equivalence
#'   interval \item lower_unstd: The unstandardised lower boundary of the
#'   equivalence interval \item upper_unstd: The unstandardised upper boundary
#'   of the equivalence interval} \item data: A description of the data
#'   \itemize{ \item type: The type of data ('raw' when arguments \code{x} and
#'   \code{y} are used or 'summary' when arguments \code{n_x}, \code{n_y},
#'   \code{mean_x}, \code{mean_y}, \code{sd_x}, and \code{sd_y} (or
#'   \code{ci_margin} and \code{ci_level} instead of \code{sd_x} and
#'   \code{sd_y}) are used) \item ...: values for the arguments used, depending
#'   on 'raw' or summary'} \item prior_scale: The width of the Cauchy prior
#'   distribution \item bf: The resulting Bayes factor } A summary of the model
#'   is shown by printing the object.
#'
#' @export
#' @import rlang stats stringr
#'
#' @references Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2019). Informed
#'   Bayesian t-tests. \emph{The American Statistician}.
#'
#'   Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008).
#'   Mixtures of g priors for Bayesian variable selection. \emph{Journal of the
#'   American Statistical Association}, \emph{103}(481), 410-423.
#'
#'   Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G.
#'   (2009). Bayesian t tests for accepting and rejecting the null hypothesis.
#'   \emph{Psychonomic Bulletin & Review}, \emph{16}(2), 225-237.
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
#'                       y = rnorm(130, 13, 10))
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
#' equiv_sum_point <- equiv_bf(n_x = 560,
#'                             n_y = 538,
#'                             mean_x = 8.683,
#'                             mean_y = 8.516,
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
#' equiv_sum_interval <- equiv_bf(n_x = 560,
#'                                n_y = 538,
#'                                mean_x = 8.683,
#'                                mean_y = 8.516,
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
                     ci_level = NULL,
                     interval = 0,
                     interval_std = TRUE,
                     prior_scale = 1 / sqrt(2)) {
  if (any(!is.null(x),
          !is.null(y)) && any(!is.null(n_x),
                              !is.null(n_y),
                              !is.null(mean_x),
                              !is.null(mean_y),
                              !is.null(sd_x),
                              !is.null(sd_y),
                              !is.null(ci_margin),
                              !is.null(ci_level))) {
    abort(str_c(
      "Only 'x' and 'y' OR 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', and ",
      "'sd_y' (or 'ci_margin' and 'ci_level' instead of 'sd_x' and 'sd_y') ",
      "must be defined."
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
        ((is.null(sd_x) || is.null(sd_y)) &&
         (is.null(ci_margin) || is.null(ci_level)))) {
      abort(str_c(
        "All 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', and 'sd_y' (or ",
        "'ci_margin' and 'ci_level' instead of 'sd_x' and 'sd_y') must be ",
        "defined."
      ))
    }

    if (!xor(!is.null(sd_x) && !is.null(sd_y),
             !is.null(ci_margin) && !is.null(ci_level))) {
      abort(str_c("Only 'sd_x' and 'sd_y' OR 'ci_margin' and 'ci_level' must ",
                  "be defined."))
    }
  }
  if (all(!is.null(n_x),
          !is.null(n_y),
          !is.null(mean_x),
          !is.null(mean_y)) && (xor(
            !is.null(sd_x) && !is.null(sd_y),
            !is.null(ci_margin) && !is.null(ci_level)
          ))) {
    if (!is.null(ci_level) && (length(ci_level) > 1 || ci_level <= 0 ||
                               ci_level >= 1 || !is.numeric(ci_level))) {
      abort("'ci_level' must be a single numeric value between 0 and 1.")
    }
    data <- list(type = "summary data",
                 data = list(n_x = n_x,
                             n_y = n_y,
                             mean_x = mean_x,
                             mean_y = mean_y,
                             sd_x = sd_x,
                             sd_y = sd_y,
                             ci_margin = ci_margin,
                             ci_level = ci_level))
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
  if (!is.logical(interval_std) || length(interval_std) > 1) {
    abort("'interval_std' must be a single logical value.")
  }
  if (!is.null(sd_x) && !is.null(sd_y)) {
    sd_pooled <- sqrt(((n_x - 1) * sd_x ^ 2 + (n_y - 1) * sd_y ^ 2) /
                        (n_x + n_y - 2))
    se <- sd_pooled * sqrt(1 / n_x + 1 / n_y)
  } else {
    perc <- 1 - ((1 - ci_level) / 2)
    se <- ci_margin / qt(p = perc,
                         df = n_x + n_y - 2)
    sd_pooled <- se / sqrt(1 / n_x + 1 / n_y)
  }
  t_stat <- (mean_y - mean_x) / se
  if (isFALSE(interval_std)) {
    inter_std <- interval / sd_pooled
    inter_unstd <- interval
  } else {
    inter_std <- interval
    inter_unstd <- interval * sd_pooled
  }
  if (length(inter_std) == 1) {
    inter_std <- c(-inter_std, inter_std)
    inter_unstd <- c(-inter_unstd, inter_unstd)
  }
  if (identical(inter_std, c(0, 0))) {
    res <- bf10_t(t = t_stat,
                  n1 = n_x,
                  n2 = n_y,
                  ind_samples = TRUE,
                  prior_loc = 0,
                  prior_scale = prior_scale,
                  prior_df = 1)
    bf <- 1 / res[[1]]
    h0 <- "mu_y == mu_x"
    h1 <- "mu_y != mu_x"
  } else {
    cdf_t_upper <- cdf_t(x = inter_std[[2]],
                         t = t_stat,
                         n1 = n_x,
                         n2 = n_y,
                         ind_samples = TRUE,
                         prior_loc = 0,
                         prior_scale = prior_scale,
                         prior_df = 1)
    cdf_t_lower <- cdf_t(x = inter_std[[1]],
                         t = t_stat,
                         n1 = n_x,
                         n2 = n_y,
                         ind_samples = TRUE,
                         prior_loc = 0,
                         prior_scale = prior_scale,
                         prior_df = 1)
    post_dens <- cdf_t_upper - cdf_t_lower
    if (post_dens < 0) {
      post_dens <- 0
      warn(str_c(
        "Numerical integration yields a posterior density slightly lower ",
        "than 0. The posterior density has been replaced by 0."
      ))
    }
    prior_dens <- pcauchy(q = inter_std[[2]],
                          scale = prior_scale) - pcauchy(q = inter_std[[1]],
                                                         scale = prior_scale)
    bf <- (post_dens / prior_dens) /
      ((1 - post_dens) / (1 - prior_dens))
    h0 <- "mu_y - mu_x > c_low AND mu_y - mu_x < c_high"
    h1 <- "mu_y - mu_x < c_low OR mu_y - mu_x > c_high"
  }
  test <- "Equivalence analysis"
  hypotheses <- list(h0 = h0,
                     h1 = h1)
  interval <- list(lower_std = inter_std[[1]],
                   upper_std = inter_std[[2]],
                   lower_unstd = inter_unstd[[1]],
                   upper_unstd = inter_unstd[[2]])
  new(Class = "baymedrEquivalence",
      test = test,
      hypotheses = hypotheses,
      interval = interval,
      data = data,
      prior_scale = prior_scale,
      bf = bf)
}
