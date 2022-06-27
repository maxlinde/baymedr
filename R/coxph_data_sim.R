#' Simulate data for Cox proportional hazards regression
#'
#' \code{\link{coxph_data_sim}} simulates data for Cox proportional hazards
#' regression models with one dichotomous independent variable based on summary
#' statistics.
#'
#' Particle swarm optimization, as implemented by \code{link[pso]{psoptim}} is
#' used to simulate a dataset that matches certain summary statistics. The
#' algorithm uses as many parameters as there cases in the dataset that is to be
#' simulated. Therefore, using \code{\link{coxph_data_sim}} becomes more
#' time-consuming the larger the sample size.
#'
#' The relevant summary statistics that are used in the optimization process
#' are: \itemize{ \item km_med_c: \itemize{ \item Kaplan-Meier median survival
#' time of the control condition. \item Lower boundary of the x% confidence
#' interval of the Kaplan-Meier median survival time of the control condition.
#' \item Upper boundary of the x% confidence interval of the Kaplan-Meier median
#' survival time of the experimental condition. } \item km_med_e: \itemize{
#' \item Kaplan-Meier median survival time of the experimental condition. \item
#' Lower boundary of the x% confidence interval of the Kaplan-Meier median
#' survival time of the experimental condition. \item Upper boundary of the x%
#' confidence interval of the Kaplan-Meier median survival time of the
#' experimental condition. } \item cox_hr \itemize{ \item Hazard ratio
#' between the experimental and control conditions based on a Cox proportional
#' hazards regression model. \item Lower boundary of the x% confidence interval
#' of the hazard ratio between the experimental and control conditions based on
#' a Cox proportional hazarads regression model. \item Upper boundary of the x%
#' confidence interval of the hazard ratio between the experimental and control
#' conditions based on a Cox proportional hazards regression model. } }
#'
#' \code{\link{coxph_data_sim}} creates a list with as many elements as
#' specified by the argument \code{n_data}. Each element consists of a list that
#' entails the resulting simulated data and the optimization results of the data
#' simulation process.
#'
#' @param n_data The number of datasets to be simulated. The default is 1.
#' @param ns_c Sample size of the control condition.
#' @param ns_e Sample size of the experimental condition.
#' @param ne_c Number of events (e.g., death) in the control condition.
#' @param ne_e Number of events (e.g., death) in the experimental condition.
#' @param km_med_c A numeric vector of length 3, indicating the Kaplan-Meier
#'   median survival time of the control condition, the lower boundary of the x%
#'   confidence interval of the Kaplan-Meier median survival time of the control
#'   condition, and the upper boundary of the x% confidence interval of the
#'   Kaplan-Meier median survival time of the control condition, respectively.
#'   Insert NA for those information that are not available.
#' @param km_med_e A numeric vector of length 3, indicating the Kaplan-Meier
#'   median survival time of the experimental condition, the lower boundary of
#'   the x% confidence interval of the Kaplan-Meier median survival time of the
#'   experimental condition, and the upper boundary of the x% confidence
#'   interval of the Kaplan-Meier median survival time of the experimental
#'   condition, respectively. Insert NA for those information that are not
#'   available.
#' @param km_med_ci_level Confidence level of the x% confidence intervals of the
#'   Kaplan-Meier median survival times. The default is 0.95.
#' @param cox_hr A numeric vector of length 3, indicating the hazard ratio
#'   between the experimental and control conditions based on a Cox proportional
#'   hazards regression model, the lower boundary of the x% confidence interval
#'   of the hazard ratio between the experimental and control conditions based
#'   on a Cox proportional hazards regression model, and the upper boundary of
#'   the x% confidence interval of the hazard ratio between the experimental and
#'   control conditions based on a Cox proportional hazards regression model,
#'   respectively. Insert NA for those information that are not available.
#' @param cox_hr_ci_level Confidence level of the x% confidence interval of the
#'   hazard ratio between the experimental and control conditions based on a Cox
#'   proportional hazards regression model. The default is 0.95.
#' @param max_t The maximum allowed survival/censoring time. The default is a
#'   heuristic that uses 2 times the maximum value in \code{km_med_c} and
#'   \code{km_med_e}.
#' @param w A numeric vector of length 9, indicating how summary statistics
#'   should be weighted in the optimization process. The relevant summary
#'   statistics consist of \code{km_med_c}, \code{km_med_e}, and \code{cox_hr},
#'   respectively. The default is \code{c(2, 1, 1, 2, 1, 1, 6, 3, 3)}.
#' @param cores The number of cores to be used in the data simulation process.
#'   The default is 1. Note that it is only useful to use more than 1 core if
#'   more than 1 dataset is simulated; ideally, \code{n_data} should be a
#'   multiple of \code{cores}.
#' @param ... Arguments passed to the \code{control} argument of
#'   \code{\link[pso]{psoptim}} (e.g. maxit, maxit.stagnate). Be aware that
#'   \code{\link{coxph_data_sim}} uses default values that are not the
#'   default in \code{\link[pso]{psoptim}}. Specifically,
#'   \code{\link{coxph_data_sim}} uses \code{maxit = 5000}, and
#'   \code{maxit.stagnate = ceiling(maxit / 5)}.
#'
#' @return A list of length \code{n_data} is returned. Each element of that list
#'   contains one simulated dataset and information about the optimization
#'   process: \itemize{ \item data: A data.frame containing the following
#'   columns: \itemize{ \item time: Survival/censoring times. \item event:
#'   Indication of whether an event happened (1) or not (0). \item group:
#'   Indication of whether case belongs to control condition (0) or experimental
#'   condition (1). } \item optim: Results of particle swarm optimization. See
#'   the Value section in \code{\link[pso]{psoptim}} }.
#'
#' @export
#' @import doParallel foreach parallel pso stringr survival
#'
#' @references
#'   Harrell, F. R. (2015). Regression modeling strategies: Withapplications to
#'   linear models, logistic regression, and survival analysis (2nd ed.).
#'   Springer.
#'
#'   Kennedy, J., & Eberhart, R. (1995). Particle swarm optimization.
#'   \emph{Proceedings of ICNN'95 - International Conference on Neural
#'   Networks}, \emph{4}, 1942-1948.
#'
#'   Shi, Y., & Eberhart, R. (1998). A modified particle swarm optimizer.
#'   \emph{1998 IEEE International Conference on Evolutionary Computation
#'   Proceedings. IEEE World Congress on Computational Intelligence}, 69-73.
#'
#' @examples
#' # Pretend we extracted the following summary statistics from an article.
#' ns_c <- 20
#' ns_e <- 56
#' ne_c <- 18
#' ne_e <- 40
#' km_med_c <- c(22, 15, 40)
#' km_med_e <- c(130, 78, 185)
#' cox_hr <- c(0.433, 0.242, 0.774)
#' km_med_ci_level <- 0.9
#' cox_hr_ci_level <- 0.95
#'
#' # Unfortunately, the precise study duration (i.e., maximum possible
#' # survival/censoring time) is not provided in the article. Therefore, we use
#' # a heuristic to define the maximum possible survival time for the simulated
#' # dataset. Note that this heuristic is the default.
#' max_t <- 2 * max(km_med_c, km_med_e, na.rm = TRUE)
#'
#' # We want to simulate 5 datasets. We do not need a very precise match of the
#' # summary statistics to the real summary statistics. Therefore, for
#' # demonstration purposes we only use 1/100 of the default number of
#' # optimization iterations (i.e., (1 / 100) * 5000).
#' sim_data <- coxph_data_sim(n_data = 5,
#'                            ns_c = ns_c,
#'                            ns_e = ns_e,
#'                            ne_c = ne_c,
#'                            ne_e = ne_e,
#'                            km_med_c = km_med_c,
#'                            km_med_e = km_med_e,
#'                            km_med_ci_level = km_med_ci_level,
#'                            cox_hr = cox_hr,
#'                            cox_hr_ci_level = cox_hr_ci_level,
#'                            max_t = 2 * max(km_med_c, km_med_e, na.rm = TRUE),
#'                            maxit = 50)
coxph_data_sim <- function(n_data = 1,
                           ns_c,
                           ns_e,
                           ne_c,
                           ne_e,
                           km_med_c,
                           km_med_e,
                           km_med_ci_level = 0.95,
                           cox_hr,
                           cox_hr_ci_level = 0.95,
                           max_t = 2 * max(km_med_c, km_med_e, na.rm = TRUE),
                           w = c(2, 1, 1,
                                 2, 1, 1,
                                 6, 3, 3),
                           cores = 1,
                           ...) {
  if (length(n_data) != 1 || n_data < 1) {
    stop("'n_data' must be a single positive integer.",
         call. = FALSE)
  }
  if (length(ns_c) != 1 || ns_c < 1 || length(ns_e) != 1 || ns_e < 1 ||
      length(ne_c) != 1 || ne_c < 1 || length(ne_e) != 1 || ne_e < 1) {
    stop("'ns_c', 'ns_e', 'ne_c', and 'ne_e' must be single positive integers.",
         call. = FALSE)
  }
  if (length(km_med_c) != 3 || !is.numeric(km_med_c) ||
      ifelse(test = all(!is.na(km_med_c[c(1, 2)])),
             yes = km_med_c[2] >= km_med_c[1],
             no = FALSE) ||
      ifelse(test = all(!is.na(km_med_c[c(1, 3)])),
             yes = km_med_c[3] <= km_med_c[1],
             no = FALSE) ||
      length(km_med_e) != 3 || !is.numeric(km_med_e) ||
      ifelse(test = all(!is.na(km_med_e[c(1, 2)])),
             yes = km_med_e[2] >= km_med_e[1],
             no = FALSE) ||
      ifelse(test = all(!is.na(km_med_e[c(1, 3)])),
             yes = km_med_e[3] <= km_med_e[1],
             no = FALSE) ||
      length(cox_hr) != 3 || !is.numeric(cox_hr) ||
      ifelse(test = all(!is.na(cox_hr[c(1, 2)])),
             yes = cox_hr[2] >= cox_hr[1],
             no = FALSE) ||
      ifelse(test = all(!is.na(cox_hr[c(1, 3)])),
             yes = cox_hr[3] <= cox_hr[1],
             no = FALSE)) {
    stop(str_c("'km_med_c', 'km_med_e', and 'cox_hr' must be numeric vectors ",
               "of length 3 containing only positive value. The second entry ",
               "must be lower than the first entry and the third entry must ",
               "higher than the first entry."),
         call. = FALSE)
  }
  if (km_med_ci_level <= 0 || km_med_ci_level >= 1 ||
      cox_hr_ci_level <= 0 || cox_hr_ci_level >= 1) {
    stop("'km_med_ci_level' and 'cox_hr_ci_level' must be between 0 and 1.",
         call. = FALSE)
  }
  if (length(max_t) != 1 || !is.numeric(max_t) || max_t <= 0 ||
      max_t < max(km_med_c, km_med_e, na.rm = TRUE)) {
    stop(str_c("'max_t' must be a single positive number that is higher than ",
               "the maximum of 'km_med_c' and 'km_med_e'."),
         call. = FALSE)
  }
  if (length(w) != 9 || !is.numeric(w) || any(w < 0)) {
    stop(str_c("'w' must be a numeric vector of length 9 containing only ",
               "positive numbers."),
         call. = FALSE)
  }
  if (length(cores) != 1 || cores < 1) {
    stop("'cores' must be a single positive integer.",
         call. = FALSE)
  }
  pso_args <- match.call(expand.dots = FALSE)$...
  if (is.null(pso_args)) {
    pso_args <- list()
  }
  if (is.null(pso_args$maxit)) {
    pso_args$maxit <- 5000
  }
  if (is.null(pso_args$maxit.stagnate)) {
    pso_args$maxit.stagnate <- ceiling(pso_args$maxit / 5)
  }
  if (cores == 1) {
    res <-
      foreach(
        i = 1:n_data,
        .packages = c("pso", "survival")
      ) %do% {
        time <- runif(n = ns_c + ns_e,
                      min = 1,
                      max = max_t)
        event <- c(rep(c(0, 1), c(ns_c - ne_c, ne_c)),
                   rep(c(0, 1), c(ns_e - ne_e, ne_e)))
        group <- rep(c(0, 1), c(ns_c, ns_e))
        c <- do.call(what = psoptim,
                     args = list(par = rep(NA, length(time)),
                                 fn = loss,
                                 km_med_c = km_med_c,
                                 km_med_e = km_med_e,
                                 cox_hr = cox_hr,
                                 time = time,
                                 event = event,
                                 group = group,
                                 km_med_ci_level = km_med_ci_level,
                                 cox_hr_ci_level = cox_hr_ci_level,
                                 w = w,
                                 lower = log(1 / time),
                                 upper = log(max_t / time),
                                 control = pso_args))
        time <- time * exp(c$par)
        list(data = data.frame(time = time,
                               event = event,
                               group = group),
             optim = c)
      }
    nc <- nchar(n_data)
    names(res) <- paste0("data_", formatC(x = 1:n_data,
                                          width = nc,
                                          flag = "0"))
    res
  } else {
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    res <-
      foreach(
        i = 1:n_data,
        .packages = c("pso", "survival")
      ) %dopar% {
        time <- runif(n = ns_c + ns_e,
                      min = 1,
                      max = max_t)
        event <- c(rep(c(0, 1), c(ns_c - ne_c, ne_c)),
                   rep(c(0, 1), c(ns_e - ne_e, ne_e)))
        group <- rep(c(0, 1), c(ns_c, ns_e))
        c <- do.call(what = psoptim,
                     args = list(par = rep(NA, length(time)),
                                 fn = loss,
                                 km_med_c = km_med_c,
                                 km_med_e = km_med_e,
                                 cox_hr = cox_hr,
                                 time = time,
                                 event = event,
                                 group = group,
                                 km_med_ci_level = km_med_ci_level,
                                 cox_hr_ci_level = cox_hr_ci_level,
                                 w = w,
                                 lower = log(1 / time),
                                 upper = log(max_t / time),
                                 control = pso_args))
        time <- time * exp(c$par)
        list(data = data.frame(time = time,
                               event = event,
                               group = group),
             optim = c)
      }
    stopCluster(cl)
    nc <- nchar(n_data)
    names(res) <- paste0("data_", formatC(x = 1:n_data,
                                          width = nc,
                                          flag = "0"))
    res
  }
}
