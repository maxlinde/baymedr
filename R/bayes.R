#' @import bridgesampling rstan
likelihood <- function(time,
                       event,
                       group,
                       null_value = 0,
                       log = TRUE) {
  surv <- time[event == 1]
  surv_ord_uniq <- unique(sort(surv))
  res_i <- 0
  for (i in 1:length(surv_ord_uniq)) {
    d_set <- which(time == surv_ord_uniq[i] & event == 1)
    d <- length(d_set)
    tmp_1 <- sum(group[d_set] * null_value)
    res_j <- 0
    for (j in 1:d) {
      tmp_2a <- sum(exp(group[time >= surv_ord_uniq[i]] * null_value))
      tmp_2b <- (j - 1) / d
      tmp_2c <- sum(exp(group[d_set] * null_value))
      res_j <- res_j + log(tmp_2a - tmp_2b * tmp_2c)
    }
    res_i <- res_i + tmp_1 - res_j
  }
  ifelse(test = log,
         yes = res_i,
         no = exp(res_i))
}

posterior <- function(time,
                      event,
                      group,
                      null_value = 0,
                      alternative = "two.sided",
                      direction = NULL,
                      prior_mean = 0,
                      prior_sd = 1,
                      ...) {
  stan_data <- list(n_time = length(time),
                    time = time,
                    event = event,
                    group = group,
                    n_surv = length(unique(time[event == 1])),
                    surv_ord_uniq = unique(sort(time[event == 1])),
                    prior_mean = prior_mean,
                    prior_sd = prior_sd,
                    null_hypothesis = null_value,
                    alternative = ifelse(test = alternative == "two.sided",
                                         yes = 0,
                                         no = 1),
                    direction = ifelse(test = is.null(direction),
                                       yes = 0,
                                       no = ifelse(test = direction == "low",
                                                   yes = -1,
                                                   no = 1)))
  sampling(object = stanmodels$coxph_bf,
           data = stan_data,
           ...)
}

marginal_likelihood <- function(object,
                                cores = 1,
                                log = TRUE) {
  log_marg_lik <- bridge_sampler(samples = object,
                                 cores = cores,
                                 use_neff = FALSE,
                                 silent = TRUE)$logml
  ifelse(test = log,
         yes = log_marg_lik,
         no = exp(log_marg_lik))
}
