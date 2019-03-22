equiv_bf = function(x = NULL,
                    y = NULL,
                    n_x = NULL,
                    n_y = NULL,
                    mean_x = NULL,
                    mean_y = NULL,
                    sd_x = NULL,
                    sd_y = NULL,
                    ci_margin = NULL,
                    interval = 0,
                    prior_scale = 1 / sqrt(2)) {
  if (any(!is.null(x),
          !is.null(y)) && any(!is.null(n_x),
                              !is.null(n_y),
                              !is.null(mean_x),
                              !is.null(mean_y),
                              !is.null(sd_x),
                              !is.null(sd_y),
                              !is.null(ci_margin))) {
    abort("Only 'x', and 'y' OR 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', and
          'sd_y' (or 'ci_margin' instead of 'sd_x' and 'sd_y') must be
          defined.")
  }
  if (xor(!is.null(x),
          !is.null(y))) {
    abort("Both 'x' and 'y' must be defined.")
  }
  if (any(!is.null(n_x),
          !is.null(n_y),
          !is.null(mean_x),
          !is.null(mean_y))) {
    if (any(is.null(n_x),
            is.null(n_y),
            is.null(mean_x),
            is.null(mean_y)) ||
        ((is.null(sd_x) || is.null(sd_y)) && is.null(ci_margin))) {
      abort("All 'n_x', 'n_y', 'mean_x', 'mean_y', 'sd_x', and 'sd_y' (or
            'ci_margin' instead of 'sd_x' and 'sd_y') must be defined.")
    }
    if (!xor(!is.null(sd_x) && !is.null(sd_y),
             !is.null(ci_margin))) {
      abort("Only 'sd_x' and 'sd_y' OR 'ci_margin' must be defined.")
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
  if (!is.null(sd_x) && !is.null(sd_y)) {
    sd_pooled <- sqrt(((n_x - 1) * sd_x ^ 2 + (n_y - 1) * sd_y ^ 2) /
                        (n_x + n_y - 2))
    se <- sd_pooled * sqrt(1 / n_x + 1 / n_y)
  } else {
    se <- ci_margin / qt(p = 0.975,
                         df = n_x + n_y - 2)
  }
  t_stat = (mean_x - mean_y) / se
  if (interval == 0) {
    res = bf10_t(t = t_stat,
                 n1 = n_x,
                 n2 = n_y,
                 ind_samples = TRUE,
                 prior_loc = 0,
                 prior_scale = prior_scale,
                 prior_df = 1)
    bf_eq = 1 / res[[1]]
    names(bf_eq) = "BF equivalence"
    cat("  Two-sided M1 equal to M2;   BF =",
        round(bf_eq,
              2),
        "\n")
    return(bf_eq)
  } else {
    post_dens = cdf_t(x = interval,
                      t = t_stat,
                      n1 = n_x,
                      n2 = n_y,
                      ind_samples = TRUE,
                      prior_loc = 0,
                      prior_scale = prior_scale,
                      prior_df = 1) - cdf_t(x = -interval,
                                            t = t_stat,
                                            n1 = n_x,
                                            n2 = n_y,
                                            ind_samples = TRUE,
                                            prior_loc = 0,
                                            prior_scale = prior_scale,
                                            prior_df = 1)
    prior_dens = pcauchy(q = interval,
                         scale = prior_scale - pcauchy(q = -interval,
                                                       scale = prior_scale))
    interval_bf = (post_dens / prior_dens) /
      ((1 - post_dens) / (1 - prior_dens))
    names(interval_bf) = "BF interval"
    cat("  Interval equivalence between M1 and M2;   BF =",
        round(interval_bf,
              2),
        "\n")
    return(interval_bf)
  }
}
