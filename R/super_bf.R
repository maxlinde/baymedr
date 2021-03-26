#' Bayes factor for superiority designs
#'
#' \code{\link{super_bf}} computes a Bayes factor for superiority designs with a
#' continuous dependent variable.
#'
#' The formulation of the null and alternative hypotheses for the superiority
#' design differs depending on whether high or low scores on the dependent
#' variable represent superiority. In both cases, the null hypothesis (i.e., H0)
#' states that the population means of the experimental group and the control
#' group are equivalent. In the case where high scores correspond to
#' superiority, the alternative hypothesis states that the population mean of
#' the experimental group is higher than the population mean of the control
#' group. Thus, the alternative hypothesis goes in the positive direction (i.e.,
#' H+). In turn, in the case where low scores correspond to superiority, the
#' alternative hypothesis states that the population mean of the experimental
#' group is lower than the population mean of the control group. Thus, the
#' alternative hypothesis goes in the negative direction (i.e., H-). The
#' dependent variable must be continuous.
#'
#' Since the main goal of \code{\link{super_bf}} is to establish superiority,
#' the resulting Bayes factor quantifies evidence in favor of the alternative
#' hypothesis. In the case where low values represent superiority we have BF-0,
#' whereas in the case where high values represent superiority we have BF+0.
#' Evidence for the null hypothesis can easily be calculated by taking the
#' reciprocal of the original Bayes factor (i.e., BF0- = 1 / BF-0 and BF0+ = 1 /
#' BF+0). Quantification of evidence in favor of the null hypothesis is
#' logically sound and legitimate within the Bayesian framework (see e.g., van
#' Ravenzwaaij et al., 2019).
#'
#' \code{\link{super_bf}} can be utilized to calculate a Bayes factor based on
#' raw data (i.e., if arguments \code{x} and \code{y} are defined) or summary
#' statistics (i.e., if arguments \code{n_x}, \code{n_y}, \code{mean_x}, and
#' \code{mean_y} are defined). In the latter case, the user has the freedom to
#' supply values either for the arguments \code{sd_x} and \code{sd_y}
#' \strong{OR} \code{ci_margin} and \code{ci_level}. Arguments with 'x' as a
#' name or suffix correspond to the control group, whereas arguments with 'y' as
#' a name or suffix correspond to the experimental group (i.e., the group for
#' which we seek to establish superiority).
#'
#' For the calculation of the Bayes factor, a Cauchy prior density centered on 0
#' is chosen for the effect size under the alternative hypothesis. The standard
#' Cauchy distribution, with a location parameter of 0 and a scale parameter of
#' 1, resembles a standard Normal distribution, except that the Cauchy
#' distribution has less mass at the center but heavier tails (Liang et al.,
#' 2008; Rouder et al., 2009). The argument \code{prior_scale} specifies the
#' width of the Cauchy prior, which corresponds to half of the interquartile
#' range. Thus, by adjusting the Cauchy prior scale with \code{prior_scale},
#' different ranges of expected effect sizes can be emphasized. The default
#' prior scale is set to r = 1 / sqrt(2).
#'
#' \code{\link{super_bf}} creates an S4 object of class
#' \linkS4class{baymedrSuperiority}, which has multiple slots/entries (e.g.,
#' type of data, prior scale, Bayes factor, etc.; see Value). If it is desired
#' to store or extract solely the Bayes factor, the user can do this with
#' \code{\link{get_bf}}, by setting the S4 object as an argument (see Examples).
#'
#' @param x A numeric vector of observations for the control group.
#' @param y A numeric vector of observations for the experimental group.
#' @param n_x A numeric vector of length one, specifying the sample size of the
#'   control group.
#' @param n_y A numeric vector of length one, specifying the sample size of the
#'   experimental group.
#' @param mean_x A numeric vector of length one, specifying the mean of the
#'   dependent variable in the control group.
#' @param mean_y A numeric vector of length one, specifying the mean of the
#'   dependent variable in the experimental group.
#' @param sd_x A numeric vector of length one, specifying the standard deviation
#'   of the dependent variable in the control group. Only \code{sd_x} and
#'   \code{sd_y} \strong{OR} \code{ci_margin} and \code{ci_level} should be
#'   defined (see Details).
#' @param sd_y A numeric vector of length one, specifying the standard deviation
#'   of the dependent variable in the experimental group. Only \code{sd_x} and
#'   \code{sd_y} \strong{OR} \code{ci_margin} and \code{ci_level} should be
#'   defined (see Details).
#' @param ci_margin A numeric vector of length one, specifying the margin of the
#'   confidence interval (i.e., the width of the confidence interval divided by
#'   2) of the mean difference on the dependent variable between the
#'   experimental and control groups. The value should be a positive number Only
#'   \code{sd_x} and \code{sd_y} \strong{OR} \code{ci_margin} and
#'   \code{ci_level} should be defined (see Details).
#' @param ci_level A numeric vector of length one, specifying the confidence
#'   level of \code{ci_margin}. The value must be between 0 and 1 (e.g., 0.95
#'   for a 95\% confidence interval). Only \code{sd_x} and \code{sd_y}
#'   \strong{OR} \code{ci_margin} and \code{ci_level} should be defined (see
#'   Details).
#' @param prior_scale A numeric vector of length one, specifying the scale of
#'   the Cauchy prior distribution for the effect size under the alternative
#'   hypothesis (see Details). The default value is r = 1 / sqrt(2).
#' @param direction A character vector of length one, specifying the direction
#'   of superior scores. 'low' indicates that low scores on the measure of
#'   interest correspond to a superior outcome and 'high' (the default)
#'   indicates that high scores on the measure of interest correspond to a
#'   superior outcome (see Details).
#'
#' @return An S4 object of class \linkS4class{baymedrSuperiority} is returned.
#'   Contained are a description of the model and the resulting Bayes factor:
#'   \itemize{ \item test: The type of analysis \item hypotheses: A statement of
#'   the hypotheses \itemize{ \item h0: The null hypothesis \item h1: The
#'   alternative hypothesis} \item data: A description of the data \itemize{
#'   \item type: The type of data ('raw' when arguments \code{x} and \code{y}
#'   are used or 'summary' when arguments \code{n_x}, \code{n_y}, \code{mean_x},
#'   \code{mean_y}, \code{sd_x}, and \code{sd_y} (or \code{ci_margin} and
#'   \code{ci_level} instead of \code{sd_x} and \code{sd_y}) are used) \item
#'   ...: values for the arguments used, depending on 'raw' or 'summary'} \item
#'   prior_scale: The scale of the Cauchy prior distribution \item bf: The
#'   resulting Bayes factor } A summary of the model is shown by printing the
#'   object.
#'
#' @export
#' @import rlang stats stringr
#'
#' @references Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2020). Informed
#'   Bayesian t-tests. \emph{The American Statistician}, \emph{74}(2), 137-143.
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
#' ## super_bf using raw data:
#'
#' # Assign model to variable.
#' super_raw <- super_bf(x = rnorm(100, 10, 15),
#'                       y = rnorm(130, 13, 10))
#'
#' # Extract Bayes factor from model.
#' get_bf(super_raw)
#'
#' # ----------
#' # ----------
#'
#' ## super_bf using summary statistics with data from Skjerven et al. (2013).
#' ## EXAMPLE 1
#'
#' # Assign model to variable.
#' super_sum_ex1 <- super_bf(n_x = 201,
#'                           n_y = 203,
#'                           mean_x = 68.1,
#'                           mean_y = 63.6,
#'                           ci_margin = (15.5 - (-6.5)) / 2,
#'                           ci_level = 0.95,
#'                           direction = "low")
#'
#' # Extract Bayes factor from model.
#' get_bf(super_sum_ex1)
#'
#' # ----------
#'
#' ## super_bf using summary statistics with data from Skjerven et al. (2013).
#' ## EXAMPLE 2
#'
#' # Assign model to variable.
#' super_sum_ex2 <- super_bf(n_x = 200,
#'                           n_y = 204,
#'                           mean_x = 47.6,
#'                           mean_y = 61.3,
#'                           ci_margin = (24.4 - 2.9) / 2,
#'                           ci_level = 0.95,
#'                           direction = "low")
#'
#' # Extract Bayes factor from model.
#' get_bf(super_sum_ex2)
super_bf <- function(x = NULL,
                     y = NULL,
                     n_x = NULL,
                     n_y = NULL,
                     mean_x = NULL,
                     mean_y = NULL,
                     sd_x = NULL,
                     sd_y = NULL,
                     ci_margin = NULL,
                     ci_level = NULL,
                     prior_scale = 1 / sqrt(2),
                     direction = "high") {
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
      abort(
        "Only 'sd_x' and 'sd_y' OR 'ci_margin' and 'ci_level' must be defined."
      )
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
  if (!is.character(direction) || length(direction) > 1) {
    abort("'direction' must be a single character value.")
  }
  if (!is.null(sd_x) && !is.null(sd_y)) {
    sd_pooled <- sqrt(((n_x - 1) * sd_x ^ 2 + (n_y - 1) * sd_y ^ 2) /
                        (n_x + n_y - 2))
    se <- sd_pooled * sqrt(1 / n_x + 1 / n_y)
  } else {
    perc <- 1 - ((1 - ci_level) / 2)
    se <- ci_margin / qt(p = perc,
                         df = n_x + n_y - 2)
  }
  t_stat <- (mean_y - mean_x) / se
  res <- bf10_t(t = t_stat,
                n1 = n_x,
                n2 = n_y,
                ind_samples = TRUE,
                prior_loc = 0,
                prior_scale = prior_scale,
                prior_df = 1)
  if (str_detect(direction,
                 "low")) {
    bf <- res[[3]]
    h1 <- "mu_y < mu_x"
  } else if (str_detect(direction,
                        "high")) {
    bf <- res[[2]]
    h1 <- "mu_y > mu_x"
  } else {
    abort("'direction' must be one of 'low' or 'high'.")
  }
  test <- "Superiority analysis"
  h0 <- "mu_y == mu_x"
  hypotheses <- list(h0 = h0,
                     h1 = h1)
  new(Class = "baymedrSuperiority",
      test = test,
      data = data,
      hypotheses = hypotheses,
      prior_scale = prior_scale,
      bf = bf)
}
