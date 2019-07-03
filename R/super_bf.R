#' Bayes factor for superiority designs
#'
#' This function computes a Bayes factor for superiority designs.
#'
#' The Bayes factor resulting from \code{\link{super_bf}} tests the null
#' hypothesis that the experimental group (e.g., a new medication) is equal to
#' the control group (e.g., a placebo or existing medication). In other words,
#' the null hypothesis of \code{\link{super_bf}} states that the true population
#' effect size is exactly zero.
#'
#' As the name 'superiority' implies, the test is typically understood as a
#' one-tailed test. In practice, however, a two-tailed test is often employed,
#' followed by subsequent inspection of the descriptive statistics to confirm
#' that results are in the expected direction. To cover both research practices,
#' the user has the possibility to specify which of these two alternatives
#' should be employed through the argument \code{alternative}. Furthermore,
#' since sometimes high scores on the dependent variable are considered superior
#' (e.g., amount of social interactions) and sometimes rather the low scores
#' (e.g., severity of symptoms), the user can specify the direction of
#' superiority with the argument \code{alternative}. For the case where higher
#' values on the dependent variable indicate superiority, the user should
#' specify 'greater' (the default) for the argument \code{alternative}; if lower
#' values on the dependent variable indicate superiority, 'less' should be
#' specified for the argument \code{alternative}.
#'
#' Importantly, \code{\link{super_bf}} can be utilized to calculate a Bayes
#' factor based on raw data (i.e., if arguments \code{x} and \code{y} are
#' defined) or summary statistics (i.e., if arguments \code{n_x}, \code{n_y},
#' \code{mean_x}, and \code{mean_y} are defined). In the latter case, the user
#' has the freedom to supply values either for the arguments \code{sd_x} and
#' \code{sd_y} \strong{OR} \code{ci_margin}. The choice should depend on the
#' information that is available to the user.
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
#' \code{\link{super_bf}} creates an S4 object of class 'baymedrSuperiority',
#' which has multiple slots/entries (e.g., type of data, prior scale, Bayes
#' factor, etc.; see Value). If it is desired to store or extract solely the
#' Bayes factor, the user can do this with \code{\link{get_bf}}, by setting the
#' S4 object as an argument (see Examples).
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
#'   \code{sd_y} \strong{OR} \code{ci_margin} should be defined (see Details).
#' @param sd_y A numeric vector of length one, specifying the standard deviation
#'   of the dependent variable in the experimental group. Only \code{sd_x} and
#'   \code{sd_y} \strong{OR} \code{ci_margin} should be defined (see Details).
#' @param ci_margin A numeric vector of length one, specifying the margin of the
#'   confidence interval (i.e., the width of the confidence interval divided by
#'   2) of the difference on the dependent variable between the control and
#'   experimental groups. Only \code{sd_x} and \code{sd_y} \strong{OR}
#'   \code{ci_margin} should be defined (see Details).
#' @param prior_scale A numeric vector of length one, specifying the scale of
#'   the prior distribution (see Details). The default value is \eqn{1 /
#'   \sqrt{2}}.
#' @param alternative A character vector of length one, specifying the direction
#'   of the alternative hypothesis. 'greater' (the default) corresponds to mu_y
#'   > mu_x, two.sided' reflects mu_y == mu_x, and 'less' refers to mu_y < mu_x
#'   (see Details).
#'
#' @return An S4 object of class 'baymedrSuperiority' is returned. Contained are
#'   a description of the model and the resulting Bayes factor: \itemize{ \item
#'   test: The type of analysis \item hypotheses: A statement of the hypotheses
#'   \itemize{ \item h0: The null hypothesis \item h1: The alternative
#'   hypothesis} \item data: A description of the data \itemize{ \item type: The
#'   type of data ('raw' when arguments \code{x} and \code{y} are used or
#'   'summary' when arguments \code{n_x}, \code{n_y}, \code{mean_x},
#'   \code{mean_y}, \code{sd_x}, and \code{sd_y} (or \code{ci_margin} instead of
#'   \code{sd_x} and \code{sd_y}) are used) \item ...: values for the arguments
#'   used, depending on 'raw' or 'summary'} \item prior_scale: The width of the
#'   Cauchy prior distribution \item bf: The resulting Bayes factor } A summary
#'   of the model is shown by printing the object.
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
#' ## The one-sided alternative:
#'
#' # Assign model to variable.
#' super_sum_one_ex1 <- super_bf(n_x = 201,
#'                               n_y = 203,
#'                               mean_x = 68.1,
#'                               mean_y = 63.6,
#'                               ci_margin = (15.5 - (-6.5)) / 2,
#'                               alternative = "less")
#'
#' # Extract Bayes factor from model.
#' get_bf(super_sum_one_ex1)
#'
#' # ----------
#'
#' ## super_bf using summary statistics with data from Skjerven et al. (2013).
#' ## EXAMPLE 1
#' ## The two-sided alternative:
#'
#' # Assign model to variable.
#' super_sum_two_ex1 <- super_bf(n_x = 201,
#'                               n_y = 203,
#'                               mean_x = 68.1,
#'                               mean_y = 63.6,
#'                               ci_margin = (15.5 - (-6.5)) / 2,
#'                               alternative = "two.sided")
#'
#' # Extract Bayes factor from model.
#' get_bf(super_sum_two_ex1)
#'
#' # ----------
#' # ----------
#'
#' ## super_bf using summary statistics with data from Skjerven et al. (2013).
#' ## EXAMPLE 2
#' ## The one-sided alternative:
#'
#' # Assign model to variable.
#' super_sum_one_ex2 <- super_bf(n_x = 200,
#'                               n_y = 204,
#'                               mean_x = 47.6,
#'                               mean_y = 61.3,
#'                               ci_margin = (24.4 - 2.9) / 2,
#'                               alternative = "less")
#'
#' # Extract Bayes factor from model.
#' get_bf(super_sum_one_ex2)
#'
#' # ----------
#'
#' ## super_bf using summary statistics with data from Skjerven et al. (2013).
#' ## EXAMPLE 2
#' ## The two-sided alternative:
#'
#' # Assign model to variable.
#' super_sum_two_ex2 <- super_bf(n_x = 200,
#'                               n_y = 204,
#'                               mean_x = 47.6,
#'                               mean_y = 61.3,
#'                               ci_margin = (24.4 - 2.9) / 2,
#'                               alternative = "two.sided")
#'
#' # Extract Bayes factor from model.
#' get_bf(super_sum_two_ex2)
super_bf <- function(x = NULL,
                     y = NULL,
                     n_x = NULL,
                     n_y = NULL,
                     mean_x = NULL,
                     mean_y = NULL,
                     sd_x = NULL,
                     sd_y = NULL,
                     ci_margin = NULL,
                     prior_scale = 1 / sqrt(2),
                     alternative = "greater") {
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
  if (!is.character(alternative) || length(alternative) > 1) {
    abort("'alternative' must be a single character value.")
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
  res <- bf10_t(t = t_stat,
                n1 = n_x,
                n2 = n_y,
                ind_samples = TRUE,
                prior_loc = 0,
                prior_scale = prior_scale,
                prior_df = 1)
  if (str_detect(alternative,
                 "greater")) {
    bf <- res[[3]]
    h1 <- "mu_y > mu_x"
  } else if (str_detect(alternative,
                        "two.sided")) {
    bf <- res[[1]]
    h1 <- "mu_y != mu_x"
  } else if (str_detect(alternative,
                        "less")) {
    bf <- res[[2]]
    h1 <- "mu_y < mu_x"
  } else {
    abort("'alternative' must be one of 'greater', 'two.sided', or 'less'.")
  }
  test <- "Superiority analysis"
  h0 <- "mu_y == mu_x"
  hypotheses <- list(h0 = h0,
                     h1 = h1)
  baymedrSuperiority(test = test,
                     data = data,
                     hypotheses = hypotheses,
                     prior_scale = prior_scale,
                     bf = bf)
}
