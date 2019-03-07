#' @import stats
cdf_t <- function(x,
                  t,
                  n1,
                  n2 = NULL,
                  ind_samples = FALSE,
                  prior_loc,
                  prior_scale,
                  prior_df) {
  integrate(f = posterior_t,
            lower = -Inf,
            upper = x,
            t = t,
            n1 = n1,
            n2 = n2,
            ind_samples = ind_samples,
            prior_loc = prior_loc,
            prior_scale = prior_scale,
            prior_df = prior_df)$value
}
