#' @import stats
integrand_t <- function(delta,
                        t,
                        n,
                        nu,
                        mu_delta,
                        gamma,
                        kappa) {
  suppressWarnings(
    dt(
      x = t,
      df = nu,
      ncp = sqrt(n) * delta) * 1 / gamma * dt(
        (delta - mu_delta) / gamma,
        df = kappa))
}
