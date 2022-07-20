log_likelihood <- function(beta,
                           time,
                           event,
                           group) {
  y <- time
  c <- event
  x <- group
  t <- unique(sort(y[c == 1]))
  ll <- 0
  for (j in 1:(length(t))) {
    d_set <- which(y == t[j] & c == 1)
    d <- length(d_set)
    e_set <- which(y >= t[j])
    a <- x[d_set]
    f <- a * beta
    v <- x[e_set]
    q <- v * beta
    tmp_1 <- sum(a) * beta
    tmp_2 <- 0
    if (any(q > 0) | all(q < 0)) {
      z <- max(v) * -beta
    } else {
      z <- 0
    }
    for (i in 1:d) {
      tmp_2a <- sum(exp(q + z))
      tmp_2b <- (i - 1) / d
      tmp_2c <- sum(exp(f + z))
      tmp_2 <- tmp_2 + log(tmp_2a - tmp_2b * tmp_2c)
    }
    ll <- ll + tmp_1 - (tmp_2 - d * z)
  }
  ll
}
log_likelihood <- Vectorize(FUN = log_likelihood,
                            vectorize.args = "beta")

bf10 <- function(beta = 0,
                 time,
                 event,
                 group,
                 alternative = "two.sided",
                 direction = NULL,
                 prior_mean = 0,
                 prior_sd = 1) {
  log_lik <- log_likelihood(beta = beta,
                            time = time,
                            event = event,
                            group = group)
  if (alternative == "two.sided") {
    m_lower <- -100
    m_upper <- 100
    i_lower <- -Inf
    i_upper <- Inf
    prior_adj <- log(1)
  } else if (alternative == "one.sided" & direction == "low") {
    m_lower <- -100
    m_upper <- beta
    i_lower <- -Inf
    i_upper <- beta
    prior_adj <- pnorm(q = beta,
                       mean = prior_mean,
                       sd = prior_sd,
                       lower.tail = TRUE,
                       log.p = TRUE)
  } else if (alternative == "one.sided" & direction == "high") {
    m_lower <- beta
    m_upper <- 100
    i_lower <- beta
    i_upper <- Inf
    prior_adj <- pnorm(q = beta,
                       mean = prior_mean,
                       sd = prior_sd,
                       lower.tail = FALSE,
                       log.p = TRUE)
  } else {
    stop("Incorrect arguments for 'alternative' and 'direction' used.",
         call. = FALSE)
  }
  m <- optim(par = beta,
             fn = function(x) {
               log_likelihood(beta = x,
                              time = time,
                              event = event,
                              group = group) + dnorm(x = x,
                                                     mean = prior_mean,
                                                     sd = prior_sd,
                                                     log = TRUE) - prior_adj
             },
             lower = m_lower,
             upper = m_upper,
             method = "Brent",
             control = list(fnscale = -1))$value
  i <- integrate(f = function(x) {
    exp(log_likelihood(beta = x,
                       time = time,
                       event = event,
                       group = group) + dnorm(x = x,
                                              mean = prior_mean,
                                              sd = prior_sd,
                                              log = TRUE) - prior_adj - m)
  },
  lower = i_lower,
  upper = i_upper)$value
  r <- m + log(i)
  exp(r - log_lik)
}
