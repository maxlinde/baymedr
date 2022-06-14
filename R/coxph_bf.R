#' @export
#' @import bridgesampling
coxph_bf <- function(time,
                     event,
                     group,
                     null_value = 0,
                     alternative = "two.sided",
                     direction = NULL,
                     prior_mean = 0,
                     prior_sd = 1,
                     log = TRUE,
                     chains = 4,
                     iter = 21000,
                     warmup = 1000,
                     thin = 1,
                     cores = 1,
                     save_samples = FALSE) {
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
  stan_fit <- sampling(object = stanmodels$coxph_bf,
                       data = stan_data,
                       chains = chains,
                       iter = iter,
                       warmup = warmup,
                       thin = thin,
                       cores = cores,
                       refresh = 0,
                       control = list(adapt_delta = 0.99))
  log_marg_lik <- bridge_sampler(samples = stan_fit,
                                 cores = cores,
                                 use_neff = FALSE,
                                 silent = TRUE)$logml
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
  log_lik <- res_i
  log_bf10 <- log_marg_lik - log_lik
  if (save_samples) {
    samples <- stan_fit
  } else {
    samples <- NA
  }
  list(bf10 = list(value = ifelse(test = log,
                                  yes = log_bf10,
                                  no = exp(log_bf10)),
                   h0 = null_value,
                   log = log),
       samples = samples)
}
