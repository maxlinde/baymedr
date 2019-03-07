super_bf <- function(formula,
                     x,
                     y,
                     data) {
  n_x <- length(x)
  n_y <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  sd_x <- sd(x)
  sd_y <- sd(y)
  sd_pooled <- sqrt(((n_x - 1) * sd_x ^ 2 + (n_y - 1) * sd_y ^ 2) /
                      (n_x + n_y - 2))
  se <- sd_pooled * sqrt(1 / n_x + 1 / n_y)
  t_stat <- (mean_x - mean_y) / se
  res = bf10_t(t = t_stat,
               n1 = n_x,
               n2 = n_y,
               ind_samples = TRUE,
               prior_loc = 0,
               prior_scale = 1 / sqrt(2),
               prior_df = 1)
  bf_sup1 = res[[3]]
  bf_sup2 = res[[1]]
  names(bf_sup1) = "BFsup1"
  names(bf_sup2) = "BFsup2"
}
