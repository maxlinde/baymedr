ci_median_t <- function(t,
                        n1,
                        n2 = NULL,
                        ind_samples = FALSE,
                        prior_loc,
                        prior_scale,
                        prior_df,
                        ci = 0.95,
                        type = "two-sided",
                        tol = 0.0001,
                        max_iter = 100) {
  lower <- (1 - ci) / 2
  upper <- ci + (1 - ci) / 2
  med <- 0.5
  post_area_smaller0 <- cdf_t(x = 0,
                              t = t,
                              n1 = n1,
                              n2 = n2,
                              ind_samples = ind_samples,
                              prior_loc = prior_loc,
                              prior_scale = prior_scale,
                              prior_df = prior_df)
  if (type == "plus-sided") {
    lower <- post_area_smaller0 + (1 - post_area_smaller0) * lower
    upper <- post_area_smaller0 + (1 - post_area_smaller0) * upper
    med <- post_area_smaller0 + (1 - post_area_smaller0) * med
  } else if (type == "min-sided") {
    lower <- post_area_smaller0 * lower
    upper <- post_area_smaller0 * upper
    med <- post_area_smaller0 * med
  }
  ci_lower <- quantile_t(lower,
                         t = t,
                         n1 = n1,
                         n2 = n2,
                         ind_samples = ind_samples,
                         prior_loc = prior_loc,
                         prior_scale = prior_scale,
                         prior_df = prior_df)
  ci_upper <- quantile_t(upper,
                         t = t,
                         n1 = n1,
                         n2 = n2,
                         ind_samples = ind_samples,
                         prior_loc = prior_loc,
                         prior_scale = prior_scale,
                         prior_df = prior_df)
  mdn <- quantile_t(med,
                    t = t,
                    n1 = n1,
                    n2 = n2,
                    ind_samples = ind_samples,
                    prior_loc = prior_loc,
                    prior_scale = prior_scale,
                    prior_df = prior_df)
  return(list(ci_lower = ci_lower,
              mdn = mdn,
              ci_upper = ci_upper))
}
