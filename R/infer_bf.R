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
#' Since sometimes high scores on the dependent variable are considered
#' non-inferior (e.g., amount of social interactions) and sometimes rather the
#' low scores (e.g., severity of symptoms), the user can specify the direction
#' of non-inferiority with the argument \code{alternative}. For the case where
#' higher values on the dependent variable indicate non-inferiority, the user
#' should specify 'greater' (the default) for the argument \code{alternative};
#' if lower values on the dependent variable indicate non-inferiority, 'less'
#' should be specified for the argument \code{alternative}.
#'
#' Importantly, \code{\link{infer_bf}} can be utilized to calculate a Bayes
#' factor based on raw data (i.e., if arguments \code{x} and \code{y} are
#' defined) or summary statistics (i.e., if arguments \code{n_x}, \code{n_y},
#' \code{mean_x}, and \code{mean_y} are defined).
#'
#' ##TODO## ni_margin
#'
#' The argument \code{prior_scale} specifies the width of the prior distribution
#' on effect size. This prior follows a Cauchy distribution with one degree of
#' freedom. Visually, it resembles a normal distribution, although with much
#' heavier tails (see e.g., Rouder et al., 2009). Mathematically, a Cauchy
#' distribution with one degree of freedom is equivalent to a normal
#' distribution with a mean of zero and a variance that follows an inverse
#' chi-square distribution with one degree of freedom, for which the variance is
#' integrated out (Liang et al., 2008). \code{prior_scale} corresponds to half
#' of the interquartile range of the Cauchy prior. In general, the larger the
#' value for \code{prior_scale}, the broader the Cauchy prior distribution, and
#' the higher the relative support for the null hypothesis, reflected in the
#' resulting Bayes factor.
#'
#' \code{\link{infer_bf}} creates an S4 object of class 'baymedrNonInferiority',
#' which has multiple slots/entries (e.g., type of data, prior scale, Bayes
#' factor, etc.; see Value). If it is desired to store or extract solely the
#' Bayes factor, the user can do this with \code{\link{get_bf}}, by setting the
#' S4 object as an argument (see Examples).
#'
#' @param ni_margin ##TODO##
#' @param alternative A character vector of length one, specifying the direction
#'   of the alternative hypothesis. 'greater' (the default) corresponds to mu_y
#'   > mu_x  and 'less' refers to mu_y < mu_x (see Details).
#' @inheritParams super_bf
#'
#' @return An S4 object of class 'baymedrSuperiority' is returned. Contained are
#'   a description of the model and the resulting Bayes factor: \itemize{ \item
#'   test: The type of analysis \item hypotheses: A statement of the hypotheses
#'   \itemize{ \item h0: The null hypothesis \item h1: The alternative
#'   hypothesis} \item ni_margin: The value for ni_margin \item data: A
#'   description of the data \itemize{ \item type: The type of data ('raw' when
#'   arguments \code{x} and \code{y} are used or 'summary' when arguments
#'   \code{n_x}, \code{n_y}, \code{mean_x}, \code{mean_y}, \code{sd_x}, and
#'   \code{sd_y} (or \code{ci_margin} instead of \code{sd_x} and \code{sd_y})
#'   are used) \item ...: values for the arguments used, depending on 'raw' or
#'   'summary'} \item prior_scale: The width of the Cauchy prior distribution
#'   \item bf: The resulting Bayes factor } A summary of the model is shown by
#'   printing the object.
#'
#' @export
#' @import rlang stats stringr
#'
#' @references Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2019). Informed
#'   bayesian t-tests. \emph{The American Statistician}, 1-13.
#'
#'   Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008).
#'   Mixtures of g priors for bayesian variable selection. \emph{Journal of the
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
#'                       ni_margin = -1)
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
#' infer_sum_t1 <- infer_bf(n_x = 32,
#'                          n_y = 33,
#'                          mean_x = 13.6,
#'                          mean_y = 17.1,
#'                          sd_x = 9.8,
#'                          sd_y = 8,
#'                          ni_margin = 2)
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
#' infer_sum_t2 <- infer_bf(n_x = 32,
#'                          n_y = 30,
#'                          mean_x = 9.2,
#'                          mean_y = 13.5,
#'                          sd_x = 7.6,
#'                          sd_y = 8.7,
#'                          ni_margin = 2)
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
                     ni_margin = NULL,
                     prior_scale = 1 / sqrt(2),
                     alternative = "greater") {
  if (any(!is.null(x),
          !is.null(y)) && any(!is.null(n_x),
                              !is.null(n_y),
                              !is.null(mean_x),
                              !is.null(mean_y),
                              !is.null(sd_x),
                              !is.null(sd_y))) {
    abort(str_c(
      "Only 'x', 'y', and 'ni_margin' OR 'n_x', 'n_y', 'mean_x', 'mean_y', ",
      "'sd_x', 'sd_y', and 'ni_margin' must be defined."
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
      abort(str_c(
        "All 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', 'sd_y', and ",
        "'ni_margin' must be defined."
      ))
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
  if (!is.numeric(ni_margin) || length(ni_margin) > 1) {
    abort("'ni_margin' must be a single numeric value.")
  }
  if (ni_margin < 0) {
    warn(str_c(
      "'ni_margin' should be a single positive numeric value. The resulting ",
      "Bayes factor should be treated with caution because of the possibility ",
      "of an inadvertently incorrect specification of 'ni_margin'."
    ))
  }
  if (!is.character(alternative) || length(alternative) > 1) {
    abort("'alternative' must be a single character value.")
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
  if (str_detect(alternative,
                 "greater")) {
    bf <- res[[3]] / res[[2]]
    h0 <- "mu_y - mu_x < ni_margin"
    h1 <- "mu_y - mu_x > ni_margin"
  } else if (str_detect(alternative,
                        "less")) {
    bf <- res[[2]] / res[[3]]
    h0 <- "mu_y - mu_x > ni_margin"
    h1 <- "mu_y - mu_x < ni_margin"
  } else {
    abort("'alternative' must be one of 'greater' or 'less'.")
  }
  test <- "Non-inferiority analysis"
  hypotheses <- list(h0 = h0,
                     h1 = h1)
  baymedrNonInferiority(test = test,
                        hypotheses = hypotheses,
                        ni_margin = ni_margin,
                        data = data,
                        prior_scale = prior_scale,
                        bf = bf)
}
