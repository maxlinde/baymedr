quantile_t <- function(q,
                       t,
                       n1,
                       n2 = NULL,
                       ind_samples = FALSE,
                       prior_loc,
                       prior_scale,
                       prior_df,
                       tol = 0.0001,
                       max_iter = 100) {
  # compute quantiles via Newton-Raphson method
  x_cur <- Inf
  # get reasonable starting value
  delta <- seq(from = -2,
               to = 2,
               length.out = 400)
  dens <- posterior_t(delta,
                      t = t,
                      n1 = n1,
                      n2 = n2,
                      ind_samples = ind_samples,
                      prior_loc = prior_loc,
                      prior_scale = prior_scale,
                      prior_df = prior_df)
  x_new <- delta[which.max(x = dens)]
  i <- 1
  while (abs(x = x_cur - x_new) > tol && i < max_iter) {
    x_cur <- x_new
    x_new <- x_cur - (cdf_t(x_cur,
                            t = t,
                            n1 = n1,
                            n2 = n2,
                            ind_samples = ind_samples,
                            prior_loc = prior_loc,
                            prior_scale = prior_scale,
                            prior_df = prior_df) - q) /
      posterior_t(x_cur,
                  t = t,
                  n1 = n1,
                  n2 = n2,
                  ind_samples = ind_samples,
                  prior_loc = prior_loc,
                  prior_scale = prior_scale,
                  prior_df = prior_df)
    i <- i + 1
  }
  return(x_new)
}
