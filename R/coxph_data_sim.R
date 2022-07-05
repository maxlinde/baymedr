#' Simulate data for Cox proportional hazards regression
#'
#' \code{\link{coxph_data_sim}} simulates data for Cox proportional hazards
#' regression models with one dichotomous independent variable based on summary
#' statistics.
#'
#' Particle swarm optimization, as implemented by \code{\link[pso]{psoptim}} is
#' used to simulate one or multiple datasets that match certain summary
#' statistics. The algorithm uses as many parameters as there cases in the
#' dataset that is to be simulated. Therefore, using
#' \code{\link{coxph_data_sim}} becomes more time-consuming the larger the
#' sample size.
#'
#' The relevant summary statistics that are used in the optimization process
#' are: \itemize{ \item cox_hr \itemize{ \item Hazard ratio
#' between the experimental and control conditions based on a Cox proportional
#' hazards regression model. \item Lower boundary of the x% confidence interval
#' of the hazard ratio between the experimental and control conditions based on
#' a Cox proportional hazards regression model. \item Upper boundary of the x%
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
#' @param cox_hr A numeric vector of length 3, indicating the hazard ratio
#'   between the experimental and control conditions based on a Cox proportional
#'   hazards regression model, the lower boundary of the x% confidence interval
#'   of the hazard ratio between the experimental and control conditions based
#'   on a Cox proportional hazards regression model, and the upper boundary of
#'   the x% confidence interval of the hazard ratio between the experimental and
#'   control conditions based on a Cox proportional hazards regression model,
#'   respectively. The hazard ratio must be provided. The confidence interval
#'   boundaries are optional; if missing they should be given as NA.
#' @param cox_hr_ci_level Confidence level of the x% confidence interval of the
#'   hazard ratio between the experimental and control conditions based on a Cox
#'   proportional hazards regression model. The default is 0.95.
#' @param max_t The maximum allowed survival/censoring time. The default is 100.
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
#' @seealso \code{\link{coxph_bf}} and \code{\link[pso]{psoptim}}.
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
#' cox_hr <- c(0.433, 0.242, 0.774)
#' cox_hr_ci_level <- 0.95
#'
#' # We want to simulate 3 datasets. We do not need a very precise match of the
#' # summary statistics to the real summary statistics. Therefore, for
#' # demonstration purposes we only use 1/200 of the default number of
#' # optimization iterations (i.e., (1 / 200) * 5000).
#' sim_data <- coxph_data_sim(n_data = 3,
#'                            ns_c = ns_c,
#'                            ns_e = ns_e,
#'                            ne_c = ne_c,
#'                            ne_e = ne_e,
#'                            cox_hr = cox_hr,
#'                            cox_hr_ci_level = cox_hr_ci_level,
#'                            max_t = 100,
#'                            maxit = 25)
coxph_data_sim <- function(n_data = 1,
                           ns_c,
                           ns_e,
                           ne_c,
                           ne_e,
                           cox_hr,
                           cox_hr_ci_level = 0.95,
                           max_t = 100,
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
  if (length(cox_hr) != 3 || !is.numeric(cox_hr) || is.na(cox_hr[1]) ||
      !all(cox_hr > 0, na.rm = TRUE) ||
      ifelse(test = !is.na(cox_hr[2]),
             yes = cox_hr[2] >= cox_hr[1],
             no = FALSE) ||
      ifelse(test = !is.na(cox_hr[3]),
             yes = cox_hr[3] <= cox_hr[1],
             no = FALSE)) {
    stop(str_c("'cox_hr' must be a numeric vector of length 3 containing only ",
    "positive values. The second entry must be lower than the first entry and ",
    "the third entry must be higher than the first entry."),
         call. = FALSE)
  }
  if (cox_hr_ci_level <= 0 || cox_hr_ci_level >= 1) {
    stop("'cox_hr_ci_level' must be between 0 and 1.",
         call. = FALSE)
  }
  if (length(max_t) != 1 || !is.numeric(max_t) || max_t <= 0) {
    stop("'max_t' must be a single positive number.",
         call. = FALSE)
  }
  if (length(cores) != 1 || cores < 1) {
    stop("'cores' must be a single positive integer.",
         call. = FALSE)
  }
  pso_args <- as.list(match.call(expand.dots = FALSE)$...)
  pso_args <- lapply(X = pso_args,
                     FUN = eval)
  if (is.null(pso_args$maxit)) {
    pso_args$maxit <- 5000
  }
  if (is.null(pso_args$maxit.stagnate)) {
    pso_args$maxit.stagnate <- ceiling(pso_args$maxit / 5)
  }
  if (cores == 1) {
    message(str_c("Running in serial.\n",
                  "Computations might take some time."))
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
                                 cox_hr = cox_hr,
                                 time = time,
                                 event = event,
                                 group = group,
                                 cox_hr_ci_level = cox_hr_ci_level,
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
    if (!getDoParRegistered()) {
      message(str_c("Running in parallel using new parallel backend.\n",
                    "Computations might take some time."))
      cl <- makeCluster(cores)
      registerDoParallel(cl)
    } else {
      message(str_c("Running in parallel using existing parallel backend.\n",
                    "Computations might take some time."))
    }
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
                                 cox_hr = cox_hr,
                                 time = time,
                                 event = event,
                                 group = group,
                                 cox_hr_ci_level = cox_hr_ci_level,
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
  }
}
