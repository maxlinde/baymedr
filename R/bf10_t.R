#' @import stats
bf10_t <- function(t,
                   n1,
                   n2 = NULL,
                   ind_samples = FALSE,
                   prior_loc,
                   prior_scale,
                   prior_df,
                   rel_tol = .Machine$double.eps^0.25) {
  neff <- ifelse(ind_samples,
                 n1 * n2 / (n1 + n2),
                 n1)
  nu <- ifelse(ind_samples,
               n1 + n2 - 2,
               n1 - 1)
  mu_delta <- prior_loc
  gamma <- prior_scale
  kappa <- prior_df
  numerator <- integrate(integrand_t,
                         lower = -Inf,
                         upper = Inf,
                         t = t,
                         n = neff,
                         nu = nu,
                         mu_delta = mu_delta,
                         gamma = gamma,
                         kappa = kappa,
                         rel.tol = rel_tol)$value
  denominator <- dt(x = t,
                    df = nu)
  bf10 <- numerator / denominator
  prior_area_smaller_0 <- pt(q = -mu_delta / gamma,
                             df = kappa)
  post_area_smaller_0 <- cdf_t(x = 0,
                               t = t,
                               n1 = n1,
                               n2 = n2,
                               ind_samples = ind_samples,
                               prior_loc = prior_loc,
                               prior_scale = prior_scale,
                               prior_df = prior_df,
                               rel_tol = rel_tol)
  bf_min1 <- post_area_smaller_0 / prior_area_smaller_0
  bf_plus1 <- (1 - post_area_smaller_0) / (1 - prior_area_smaller_0)
  bf_min0 <- bf_min1 * bf10
  bf_plus0 <- bf_plus1 * bf10
  return(list(
    bf_10 = bf10,
    bf_plus0 = bf_plus0,
    bf_min0 = bf_min0
    ))
}
