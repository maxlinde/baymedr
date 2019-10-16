#' Bayes factor for non-inferiority designs
#'
#' \code{\link{infer_bf}} computes a Bayes factor for non-inferiority designs.
#'
#' The formulation of the null and alternative hypotheses differ depending on
#' whether high or low scores on the measure of interest represent
#' non-inferiority. In the case where high scores correspond to non-inferiority
#' (e.g., amount of social interaction), the Bayes factor resulting from
#' \code{\link{infer_bf}} tests the null hypothesis that the experimental group
#' (e.g., a new medication) is lower than the control group (e.g., a placebo or
#' an already existing medication) minus a constant value, which is given by the
#' non-inferiority margin. The alternative hypothesis is that the experimental
#' group is higher than the control group minus the non-inferiority margin. In
#' turn, when low values on the measure of interest correspond to
#' non-inferiority (e.g., severity of symptoms), the Bayes factor resulting from
#' \code{\link{infer_bf}} tests the null hypothesis that the experimental group
#' is higher than the control group plus the non-inferiority margin. The
#' alternative hypothesis states that the experimental group is lower than the
#' control group plus the non-inferiority margin.
#'
#' Since the main goal of \code{\link{infer_bf}} is to establish
#' non-inferiority, the resulting Bayes factor quantifies evidence in favour of
#' the alternative hypothesis (i.e., BF10). However, evidence for the null
#' hypothesis can easily be calculated by taking the reciprocal of the original
#' Bayes factor (i.e., BF01 = 1 / BF10). Quantification of evidence in favour of
#' the null hypothesis is logically sound and legitimate within the Bayesian
#' framework but not in the traditional frequentist framework (see e.g., van
#' Ravenzwaaij et al., 2019).
#'
#' Importantly, \code{\link{infer_bf}} can be utilized to calculate a Bayes
#' factor based on raw data (i.e., if arguments \code{x} and \code{y} are
#' defined) or summary statistics (i.e., if arguments \code{n_x}, \code{n_y},
#' \code{mean_x}, and \code{mean_y} (or \code{ci_margin} and \code{ci_level}
#' instead of \code{sd_x} and \code{sd_y}) are defined). Arguments with 'x' as a
#' name or suffix correspond to the control group, whereas arguments with 'y' as
#' a name or suffix correspond to the experimental group (i.e., the group for
#' which we seek to establish non-inferiority).
#'
#' Since sometimes high scores on the dependent variable are considered
#' non-inferior (e.g., amount of social interactions) and sometimes rather the
#' low scores (e.g., severity of symptoms), the user can specify the direction
#' of non-inferiority with the argument \code{direction}. For the case where
#' higher values on the dependent variable indicate non-inferiority, the user
#' should specify 'high' (the default) for the argument \code{direction}; if
#' lower values on the dependent variable indicate non-inferiority, 'low' should
#' be specified for the argument \code{direction}.
#'
#' With the argument \code{ni_margin}, the user can determine the
#' non-inferiority margin. \code{ni_margin} should be a positive number.
#' Following ethical scientific rigour, \code{ni_margin} should be defined prior
#' to data collection and data analysis. The user can declare whether the
#' non-inferiority margin was specified in standardised or unstandardised units
#' with the \code{ni_margin_std} argument, where TRUE, corresponding to
#' standardised units, is the default.
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
#' scale is set to 1 / sqrt(2).
#'
#' \code{\link{infer_bf}} creates an S4 object of class
#' \linkS4class{baymedrNonInferiority}, which has multiple slots/entries (e.g.,
#' type of data, prior scale, Bayes factor, etc.; see Value). If it is desired
#' to store or extract solely the Bayes factor, the user can do this with
#' \code{\link{get_bf}}, by setting the S4 object as an argument (see Examples).
#'
#' @param ni_margin A numeric vector of length one, specifying the
#'   non-inferiority margin. The value should be a positive number.
#' @param ni_margin_std A logical vector of length one, specifying whether the
#'   non-inferiority margin (i.e., \code{ni_margin}) is given in standardised
#'   (TRUE; the default) or unstandardised (FALSE) units.
#' @param direction A character vector of length one, specifying the direction
#'   of non-inferior scores. 'low' indicates that low scores on the measure of
#'   interest correspond to a non-inferior outcome and 'high' (the default)
#'   indicates that high scores on the measure of interest correspond to a
#'   non-inferior outcome (see Details).
#' @inheritParams super_bf
#'
#' @return An S4 object of class \linkS4class{baymedrNonInferiority} is
#'   returned. Contained are a description of the model and the resulting Bayes
#'   factor: \itemize{ \item test: The type of analysis \item hypotheses: A
#'   statement of the hypotheses \itemize{ \item h0: The null hypothesis \item
#'   h1: The alternative hypothesis} \item ni_margin: The value for ni_margin in
#'   standardised and unstandardised units \itemize{ \item ni_mar_std: The
#'   standardised non-inferiority margin \item ni_mar_unstd: The unstandardised
#'   non-inferiority margin} \item data: A description of the data \itemize{
#'   \item type: The type of data ('raw' when arguments \code{x} and \code{y}
#'   are used or 'summary' when arguments \code{n_x}, \code{n_y}, \code{mean_x},
#'   \code{mean_y}, \code{sd_x}, and \code{sd_y} (or \code{ci_margin} and
#'   \code{ci_level} instead of \code{sd_x} and \code{sd_y}) are used) \item
#'   ...: values for the arguments used, depending on 'raw' or summary'} \item
#'   prior_scale: The width of the Cauchy prior distribution \item bf: The
#'   resulting Bayes factor } A summary of the model is shown by printing the
#'   object.
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
#' ## infer_bf using raw data:
#'
#' # Assign model to variable.
#' infer_raw <- infer_bf(x = rnorm(100, 10, 15),
#'                       y = rnorm(130, 13, 10),
#'                       ni_margin = 1.5,
#'                       ni_margin_std = FALSE)
#'
#' # Extract Bayes factor from model.
#' get_bf(infer_raw)
#'
#' # ----------
#' # ----------
#'
#' ## infer_bf using summary statistics with data from Andersson et al. (2013).
#' ## Test at timepoint 1:
#'
#' # Assign model to variable.
#' infer_sum_t1 <- infer_bf(n_x = 33,
#'                          n_y = 32,
#'                          mean_x = 17.1,
#'                          mean_y = 13.6,
#'                          sd_x = 8,
#'                          sd_y = 9.8,
#'                          ni_margin = 2,
#'                          ni_margin_std = FALSE,
#'                          direction = "low")
#'
#' # Extract Bayes factor from model
#' get_bf(infer_sum_t1)
#'
#' # ----------
#' # ----------
#'
#' ## infer_bf using summary statistics with data from Andersson et al. (2013).
#' ## Test at timepoint 2:
#'
#' # Assign model to variable.
#' infer_sum_t2 <- infer_bf(n_x = 30,
#'                          n_y = 32,
#'                          mean_x = 13.5,
#'                          mean_y = 9.2,
#'                          sd_x = 8.7,
#'                          sd_y = 7.6,
#'                          ni_margin = 2,
#'                          ni_margin_std = FALSE,
#'                          direction = "low")
#'
#' # Extract Bayes factor from model
#' get_bf(infer_sum_t2)
infer_bf <- function(x = NULL,
                     y = NULL,
                     n_x = NULL,
                     n_y = NULL,
                     mean_x = NULL,
                     mean_y = NULL,
                     sd_x = NULL,
                     sd_y = NULL,
                     ci_margin = NULL,
                     ci_level = NULL,
                     ni_margin = NULL,
                     ni_margin_std = TRUE,
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
      "Only 'x', 'y', and 'ni_margin' OR 'n_x', 'n_y', 'mean_x', 'mean_y', ",
      "'sd_x', 'sd_y', and 'ni_margin' (or 'ci_margin' and 'ci_level' instead ",
      "of 'sd_x' and 'sd_y') must be defined."
    ))
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
            is.null(ni_margin)) ||
        ((is.null(sd_x) || is.null(sd_y)) &&
         (is.null(ci_margin) || is.null(ci_level)))) {
      abort(str_c(
        "All 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', 'sd_y', and ",
        "'ni_margin' (or 'ci_margin' and 'ci_level' instead of 'sd_x' and ",
        "'sd_y') must be defined."
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
  if (!is.numeric(ni_margin) || length(ni_margin) > 1 || ni_margin < 0) {
    abort("'ni_margin' must be a single positive numeric value.")
  }
  if (!is.logical(ni_margin_std) || length(ni_margin_std) > 1) {
    abort("'ni_margin_std' must be a single locial value.")
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
    sd_pooled <- se / sqrt(1 / n_x + 1 / n_y)
  }
  if (str_detect(direction,
                 "low")) {
    if (isFALSE(ni_margin_std)) {
      ni_mar_std <- ni_margin / sd_pooled
      ni_mar_unstd <- ni_margin
    } else {
      ni_mar_std <- ni_margin
      ni_mar_unstd <- ni_margin * sd_pooled
    }
    t_stat <- (mean_y - mean_x - ni_margin) / se
    res <- bf10_t(t = t_stat,
                  n1 = n_x,
                  n2 = n_y,
                  ind_samples = TRUE,
                  prior_loc = ni_mar_std,
                  prior_scale = prior_scale,
                  prior_df = 1)
    bf <- res[[3]] / res[[2]]
    h0 <- "mu_y - mu_x > ni_margin"
    h1 <- "mu_y - mu_x < ni_margin"
  } else if (str_detect(direction,
                        "high")) {
    if (isFALSE(ni_margin_std)) {
      ni_mar_std <- -ni_margin / sd_pooled
      ni_mar_unstd <- -ni_margin
    } else {
      ni_mar_std <- -ni_margin
      ni_mar_unstd <- -ni_margin * sd_pooled
    }
    t_stat <- (mean_y - mean_x + ni_margin) / se
    res <- bf10_t(t = t_stat,
                  n1 = n_x,
                  n2 = n_y,
                  ind_samples = TRUE,
                  prior_loc = ni_mar_std,
                  prior_scale = prior_scale,
                  prior_df = 1)
    bf <- res[[2]] / res[[3]]
    h0 <- "mu_y - mu_x < -ni_margin"
    h1 <- "mu_y - mu_x > -ni_margin"
  } else {
    abort("'direction' must be one of 'low' or 'high'.")
  }
  test <- "Non-inferiority analysis"
  hypotheses <- list(h0 = h0,
                     h1 = h1)
  ni_margin <- list(ni_mar_std = abs(ni_mar_std),
                    ni_mar_unstd = abs(ni_mar_unstd))
  new(Class = "baymedrNonInferiority",
      test = test,
      hypotheses = hypotheses,
      ni_margin = ni_margin,
      data = data,
      prior_scale = prior_scale,
      bf = bf)
}
