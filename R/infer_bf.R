infer_bf <- function(x = NULL,
                     y = NULL,
                     n_x = NULL,
                     n_y = NULL,
                     mean_x = NULL,
                     mean_y = NULL,
                     sd_x = NULL,
                     sd_y = NULL,
                     ni_margin = NULL,
                     prior_scale = 1 / sqrt(2)) {
  if (any(!is.null(x),
          !is.null(y)) && any(!is.null(n_x),
                              !is.null(n_y),
                              !is.null(mean_x),
                              !is.null(mean_y),
                              !is.null(sd_x),
                              !is.null(sd_y))) {
    abort("Only 'x', 'y', and 'ni_margin' OR 'n_x', 'n_y', 'mean_x', 'mean_y',
          'sd_x', 'sd_y', and 'ni_margin' must be defined.")
  }
  if (any(!is.null(x),
          !is.null(y),
          !is.null(ni_margin))) {
    if (any(is.null(x),
            is.null(y),
            is.null(ni_margin))) {
      abort("All 'x', 'y', and 'ni_margin' must be defined.")
    }
  }
  if (any(!is.null(n_x),
          !is.null(n_y),
          !is.null(mean_x),
          !is.null(mean_y),
          !is.null(ni_margin))) {
    if (any(is.null(n_x),
            is.null(n_y),
            is.null(mean_x),
            is.null(mean_y),
            is.null(ni_margin))) {
      abort("All 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', 'sd_y', and
            'ni_margin' must be defined.")
    }
  }
  if (!is.null(x) && !is.null(y)) {
    if (any(is.na(x)) || any(is.na(y))) {
      abort("'x' and 'y' must not contain missing values.")
    }
    if (any(is.infinite(x)) || any(is.infinite(y))) {
      abort("'x' and 'y' must not contain infinite values.")
    }
    if (!is.numeric(x) || !is.numeric(y)) {
      abort("'x' and 'y' must be numeric vectors.")
    }
    n_x <- length(x)
    n_y <- length(y)
    mean_x <- mean(x)
    mean_y <- mean(y)
    sd_x <- sd(x)
    sd_y <- sd(y)
  }
  sd_pooled <- sqrt(((n_x - 1) * sd_x ^ 2 + (n_y - 1) * sd_y ^ 2) /
                      (n_x + n_y - 2))
  se <- sd_pooled * sqrt(1 / n_x + 1 / n_y)
  cohen_d <- ni_margin / sd_pooled
  t_stat <- (mean_x - mean_y - ni_margin) / se
  res <- bf10_t(t = t_stat,
                n1 = n_x,
                n2 = n_y,
                ind_samples = TRUE,
                prior_loc = cohen_d,
                prior_scale = prior_scale,
                prior_df = 1)
  ni_bf <- res[[3]] * (1 / res[[2]])
  names(ni_bf) <- "non-inferiority BF"
  cat("  Mean group 2 at least as high as (mean group 1 - ni_margin) BF =",
      round(x = ni_bf,
            digits = 2),
      "\n")
  return(ni_bf)
}
