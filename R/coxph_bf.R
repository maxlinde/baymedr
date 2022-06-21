#' Bayes factor for Cox proportional hazards regression
#'
#' \code{\link{coxph_bf}} computes a Bayes factor for Cox proportional hazards
#' regression models with one dichotomous independent variable.
#'
#' The Cox proportional hazards model has the following hypotheses: The null
#' hypothesis (i.e., H0) states that the population hazard ratio between the
#' experimental (e.g., a new medication) and the control group (e.g., a placebo
#' or an already existing medication) is equal to 1 (i.e., beta = 0). The
#' alternative hypothesis can be two-sided or one-sided (either negative or
#' positive).
#'
#' Since the main goal of \code{\link{coxph_bf}} is to establish that the
#' hazard ratio is not equal to 1, the resulting Bayes factor quantifies
#' evidence in favor of the alternative hypothesis. For a two-sided alternative
#' hypothesis, we have BF10; for a negative one-sided alternative hypothesis, we
#' have BF-0; and for a positive one-sided alternative hypothesis, we have BF+0.
#' Evidence for the null hypothesis can easily be calculated by taking the
#' reciprocal of the original Bayes factor (i.e., BF01 = 1 / BF10).
#' Quantification of evidence in favor of the null hypothesis is logically sound
#' and legitimate within the Bayesian framework (see e.g., van Ravenzwaaij et
#' al., 2019).
#'
#' For the calculation of the Bayes factor, a Normal prior density is chosen for
#' beta under the alternative hypothesis. The arguments \code{prior_mean} and
#' \code{prior_sd} specify the mean and standard deviation of the Normal prior,
#' respectively. By adjusting the Normal prior, different ranges of expected
#' effect sizes can be emphasized. The default is a Normal prior with a mean of
#' 0 and a standard deviation of 1.
#'
#' Note that at the moment the model specifications are limited. That is, it is
#' only possible to have a single dichotomous independent variable. Further, at
#' the moment only a Normal prior is supported. Lastly, only the Efron partial
#' likelihood and not the many other options are supported.
#'
#' \code{\link{coxph_bf}} creates an S4 object of class
#' \linkS4class{baymedrCoxProportionalHazards}, which has multiple slots/entries
#' (e.g., prior, Bayes factor, etc.; see Value). If it is desired to store or
#' extract solely the Bayes factor, the user can do this with
#' \code{\link{get_bf}}, by setting the S4 object as an argument (see Examples).
#'
#' @param data A data.frame or a list resulting from calling
#'   \code{coxph_data_sim}. In the former case, the first column represents the
#'   survival/censoring times, the second column indicates whether an event
#'   (e.g., death) has happened (1) or not (0), and the third column represents
#'   the dummy-coded independent variable, where the control group is coded with
#'   0 and the experimental group with 1.
#' @param null_value The value of the point null hypothesis for the beta
#'   coefficient. The default is a null value of 0.
#' @param alternative A string specifying whether the alternative hypothesis is
#'   two-sided ("two.sided"; the default) or one-sided ("one.sided").
#' @param direction A string specifying the direction of the one-sided
#'   alternative hypothesis. This is ignored if
#'   \code{alternative = "two.sided"}. Possible options are "low" and "high".
#' @param prior_mean Mean of the Normal prior for the beta parameter. The
#'   default is a mean of 0.
#' @param prior_sd Standard deviation of the Normal prior for the beta
#'   parameter. The default is a standard deviation of 1.
#' @param save_samples A logical value indicating whether the posterior samples
#'   should be saved (TRUE) or not (FALSE; the default).
#' @param ... Arguments passed to \code{\link[rstan]{sampling}} (e.g. iter,
#'   chains). Be aware that \code{\link{coxph_bf}} uses default values that are not the
#'   default in \code{\link[rstan]{sampling}}. Specifically,
#'   \code{\link{coxph_bf}} uses \code{chains = 5}, \code{warmup = 1000}, and
#'   \code{iter = ceiling(50000 / chains + warmup)}.
#'
#' @return An S4 object of class \linkS4class{baymedrCoxProportionalHazards} is
#'   returned. Contained are a description of the model and the resulting Bayes
#'   factor: \itemize{ \item test: The type of analysis \item hypotheses: A
#'   statement of the hypotheses \itemize{ \item h0: The null hypothesis
#'   \item h1: The alternative hypothesis} \item prior: The parameters of the
#'   Normal prior on beta \item bf: The resulting Bayes factor \item samples:
#'   The \linkS4class{stanfit} object containing the posterior samples } A
#'   summary of the model is shown by printing the object.
#'
#' @export
#' @import bridgesampling rstan stringr
#'
#' @references van Ravenzwaaij, D., Monden, R., Tendeiro, J. N., & Ioannidis, J. P. A.
#'   (2019). Bayes factors for superiority, non-inferiority, and equivalence
#'   designs. \emph{BMC Medical Research Methodology}, \emph{19}(1), 71.
#'
#' @examples
#' # Load aml dataset from the survival R package.
#' data <- survival::aml
#' data$x <- ifelse(test = data$x == "Maintained",
#'                  yes = 0,
#'                  no = 1)
#' names(data) <- c("time", "event", "group")
#'
#' # Assign model to variable.
#' coxph_mod <- coxph_bf(data = data,
#'                       null_value = 0,
#'                       alternative = "one.sided",
#'                       direction = 1,
#'                       prior_mean = 0,
#'                       prior_sd = 1,
#'                       save_samples = TRUE)
#'
#' # Extract Bayes factor from variable.
#' get_bf(coxph_mod)
coxph_bf <- function(data,
                     null_value = 0,
                     alternative = "two.sided",
                     direction = NULL,
                     prior_mean = 0,
                     prior_sd = 1,
                     save_samples = FALSE,
                     ...) {
  if (!inherits(x = data,
                what = c("data.frame", "list"))) {
    stop(str_c("'data' must be either a single data.frame or a list resulting ",
               "from coxph_data_sim()."),
         call. = FALSE)
  }
  if (length(null_value) != 1 || !is.numeric(null_value)) {
    stop("'null_value' must be a single numeric value.",
         call. = FALSE)
  }
  if (length(alternative) != 1 ||
      !(alternative %in% c("one.sided", "two.sided"))) {
    stop("'alternative' must be one of one.sided or two.sided.",
         call. = FALSE)
  }
  if (alternative == "two.sided") {
    if (!is.null(direction)) {
      stop("When 'alternative' is two.sided, 'direction' must be NULL.",
           call. = FALSE)
    }
  } else {
    if (length(direction) != 1 ||
        !(direction %in% c(-1, 1))) {
      stop("When 'alternative' is one.sided, 'direction' must be -1 or 1.",
           call. = FALSE)
    }
  }
  if (length(prior_mean) != 1 || !is.numeric(prior_mean) ||
      length(prior_sd) != 1 || !is.numeric(prior_sd) || prior_sd < 0) {
    stop(str_c("'prior_mean' and 'prior_sd' must be single numeric values. ",
               "'prior_sd' must be non-negative."),
         call. = FALSE)
  }
  if (length(save_samples) != 1 || !is.logical(save_samples)) {
    stop("'save_samples' must be a single logical value.",
         call. = FALSE)
  }
  stan_args <- match.call(expand.dots = FALSE)$...
  if (is.null(stan_args)) {
    stan_args <- list()
  }
  if (is.null(stan_args$chains)) {
    stan_args$chains <- 5
  }
  if (is.null(stan_args$warmup)) {
    stan_args$warmup <- 1000
  }
  if (is.null(stan_args$iter)) {
    stan_args$iter <- ceiling(50000 / stan_args$chains + stan_args$warmup)
  }
  if (is.null(stan_args$cores)) {
    stan_args$cores <- 1
  }
  test <- "Cox proportional hazards analysis"
  h0 <- paste0("beta == ", null_value)
  if (alternative == "two.sided") {
    h1 <- paste0("beta != ", null_value)
  } else {
    if (direction == "low") {
      h1 <- paste0("beta < ", null_value)
    } else {
      h1 <- paste0("beta > ", null_value)
    }
  }
  hypotheses <- list(h0 = h0,
                     h1 = h1)
  prior <- list(mean = prior_mean,
                sd = prior_sd)
  if (inherits(x = data,
               what = "data.frame")) {
    time <- data[[1]]
    event <- data[[2]]
    group <- data[[3]]
    if (!is.numeric(time) || any(time < 0, na.rm = TRUE)) {
      stop("The first column in 'data' must be a non-negative numeric vector.",
           call. = FALSE)
    }
    if (!is.numeric(event) || !all(event %in% c(0:1, NA))) {
      stop(str_c("The second column in 'data' must be a numeric vector ",
                 "containing only the values 0 and 1."),
           call. = FALSE)
    }
    if (!is.numeric(group) || !all(group %in% c(0:1, NA))) {
      stop(str_c("The third column in 'data' must be a numeric vector ",
                 "containing only the values 0 and 1."),
           call. = FALSE)
    }
    if (any(is.na(data))) {
      stop("'data' must not contain any missing values.",
           call. = FALSE)
    }
    log_lik <- likelihood(time = time,
                          event = event,
                          group = group,
                          null_value = null_value,
                          log = TRUE)
    post <- do.call(what = posterior,
                    args = c(list(time = time,
                                  event = event,
                                  group = group,
                                  null_value = null_value,
                                  alternative = alternative,
                                  direction = direction,
                                  prior_mean = prior_mean,
                                  prior_sd = prior_sd),
                             stan_args))
    log_marg_lik <- marginal_likelihood(object = post,
                                        cores = stan_args$cores,
                                        log = TRUE)
    log_bf10 <- log_marg_lik - log_lik
    if (save_samples) {
      samples <- post
    }
    if (!save_samples) {
      new(Class = "baymedrCoxProportionalHazards",
          test = test,
          hypotheses = hypotheses,
          prior = prior,
          bf = exp(log_bf10))
    } else {
      new(Class = "baymedrCoxProportionalHazardsSamples",
          test = test,
          hypotheses = hypotheses,
          prior = prior,
          bf = exp(log_bf10),
          samples = samples)
    }
  } else {
  # if (inherits(x = data,
  #              what = "list")) {
    n_elem <- length(data)
    for (i in 1:n_elem) {
      tmp_data <- data[[i]][["data"]]
      time <- tmp_data[[1]]
      event <- tmp_data[[2]]
      group <- tmp_data[[3]]
      if (!is.numeric(time) || any(time < 0, na.rm = TRUE)) {
        stop("The first column in 'data' must be a non-negative numeric vector.",
             call. = FALSE)
      }
      if (!is.numeric(event) || !all(event %in% c(0:1, NA))) {
        stop(str_c("The second column in 'data' must be a numeric vector ",
                   "containing only the values 0 and 1."),
             call. = FALSE)
      }
      if (!is.numeric(group) || !all(group %in% c(0:1, NA))) {
        stop(str_c("The third column in 'data' must be a numeric vector ",
                   "containing only the values 0 and 1."),
             call. = FALSE)
      }
      if (any(is.na(data))) {
        stop("'data' must not contain any missing values.",
             call. = FALSE)
      }
    }
    log_bf10 <- vector(mode = "numeric",
                       length = n_elem)
    post <- list()
    for (i in 1:n_elem) {
      tmp_data <- data[[i]][["data"]]
      time <- tmp_data[[1]]
      event <- tmp_data[[2]]
      group <- tmp_data[[3]]
      log_lik <- likelihood(time = time,
                            event = event,
                            group = group,
                            null_value = null_value,
                            log = TRUE)
      post[[i]] <- do.call(what = posterior,
                           args = c(list(time = time,
                                         event = event,
                                         group = group,
                                         null_value = null_value,
                                         alternative = alternative,
                                         direction = direction,
                                         prior_mean = prior_mean,
                                         prior_sd = prior_sd),
                                    stan_args))
      log_marg_lik <- marginal_likelihood(object = post[[i]],
                                          cores = stan_args$cores,
                                          log = TRUE)
      log_bf10[i] <- log_marg_lik - log_lik
    }
    if (save_samples) {
      samples <- post
    }
    if (!save_samples) {
      new(Class = "baymedrCoxProportionalHazardsMulti",
          test = test,
          hypotheses = hypotheses,
          prior = prior,
          bf = exp(log_bf10))
    } else {
      new(Class = "baymedrCoxProportionalHazardsSamplesMulti",
          test = test,
          hypotheses = hypotheses,
          prior = prior,
          bf = exp(log_bf10),
          samples = samples)
    }
  }
}
