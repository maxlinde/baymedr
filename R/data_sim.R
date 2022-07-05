#' @import rms survival
# This function calculates the hazard ratio and the corresponding x% confidence
# interval for a Cox proportional hazards regression model that is fit using the
# Efron likelihood (cf. Harrell, 2015, p. 477, eq. 20.7).
cox_hr_ci <- function(time,
                      event,
                      group,
                      ci_level = 0.95) {
  mod <- coxph(formula = Surv(time, event) ~ group,
               method = "efron")
  hr_ci <- summary(mod)$conf.int[c(1, 3, 4)]
  names(hr_ci) <- c("hr", "ci_lower", "ci_upper")
  hr_ci
}

# This is the loss function that we want to minimize for the data generation
# process. The loss is based on the hazard ratio and the corresponding
# confidence interval. Using these summary statistics, the loss is defined as
# the log of the weighted and root mean squared deviation between the actual
# summary statistics (i.e., those we want to reach) and the summary statistics
# of the simulated data. It is possible to only provide the hazard ratio and not
# the confidence interval boundaries.
loss <- function(par,
                 cox_hr,
                 time,
                 event,
                 group,
                 cox_hr_ci_level = 0.95) {
  time <- time * exp(par)
  sim_cox_hr <- cox_hr_ci(time = time,
                          event = event,
                          group = group,
                          ci_level = cox_hr_ci_level)
  act <- log(cox_hr)
  sim <- sim_cox_hr
  if (is.na(act[1]) || is.na(sim[1])) {
    stop("The hazard ratio must be provided.",
         call. = FALSE)
  }
  idx <- !is.na(act) & !is.na(sim)
  act <- act[idx]
  sim <- sim[idx]
  w <- c(2, 1, 1)
  w <- w[idx]
  w <- w / sum(w)
  # Mean squared of the weighted errors.
  ms_weighted_error <- crossprod((sim - act) * w) / length(act)
  log(ms_weighted_error)
}
