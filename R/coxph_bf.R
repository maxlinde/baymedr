#' @export
#' @import bridgesampling rstan
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
  log_lik <- likelihood(time = time,
                        event = event,
                        group = group,
                        null_value = null_value,
                        log = TRUE)
  post <- posterior(time = time,
                    event = event,
                    group = group,
                    null_value = null_value,
                    alternative = alternative,
                    direction = direction,
                    prior_mean = prior_mean,
                    prior_sd = prior_sd,
                    chains = chains,
                    iter = iter,
                    warmup = warmup,
                    thin = thin,
                    cores = cores)
  log_marg_lik <- marginal_likelihood(object = post,
                                      cores = cores,
                                      log = TRUE)
  log_bf10 <- log_marg_lik - log_lik
  if (save_samples) {
    samples <- post
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
