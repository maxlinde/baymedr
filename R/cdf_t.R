#' @import rlang stats stringr
cdf_t <- function(x,
                  t,
                  n1,
                  n2 = NULL,
                  ind_samples = FALSE,
                  prior_loc,
                  prior_scale,
                  prior_df,
                  rel_tol = .Machine$double.eps^0.25) {
  out <- integrate(posterior_t,
                   lower = -Inf,
                   upper = x,
                   t = t,
                   n1 = n1,
                   n2 = n2,
                   ind_samples = ind_samples,
                   prior_loc = prior_loc,
                   prior_scale = prior_scale,
                   prior_df = prior_df,
                   rel.tol = rel_tol)$value
  if (out > 1) {
    out <- 1
    warn(str_c(
      "Numerical integration yields a CDF value slightly larger than 1. ",
      "The CDF value has been replaced by 1."
    ))
  }
  out
}
