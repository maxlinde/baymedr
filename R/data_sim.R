#' @import rms survival
# This function calculates the Kaplan-Meier median survival time and the
# corresponding x% confidence interval.
km_med_ci <- function(time,
                      event,
                      ci_level = 0.95) {
  mod <- survfit(formula = Surv(time, event) ~ 1,
                 conf.int = ci_level)
  mod_summary <- summary(mod)$table
  med_ci <- mod_summary[grep(pattern = "(median|CL)",
                             x = names(mod_summary))]
  names(med_ci) <- c("median", "ci_lower", "ci_upper")
  med_ci
}

# This function calculates the hazard ratio and the corresponding x% confidence
# interval for a Cox proportional hazards regression model that is fit using the
# Efron likelihood (cf. Harrell, 2015, p. 477, eq. 20.7).
cox_hr_ci <- function(time,
                      event,
                      group,
                      ci_level = 0.95) {
  mod <- cph(formula = Surv(time, event) ~ group,
               method = "efron",
               eps = 1e-4)
  hr <- exp(coef(object = mod))
  hr_ci <- exp(confint(object = mod,
                       level = ci_level))
  out <- c(hr, hr_ci)
  names(out) <- c("hr", "ci_lower", "ci_upper")
  out
}

# This is the loss function that we want to minimize for the data generation
# process. The loss is based on the following nine summary statistics:
#     -Kaplan-Meier median survival time for the control group (and x%
#      confidence interval),
#     -Kaplan-Meier median survival time for the experimental group (and x%
#      confidence interval),
#     -Hazard ratio of a Cox proportional hazards regression model that is fit
#      with the Efron likelihood (cf. Harrell, 2015, p. 477, eq. 20.7) (and x%
#      confidence interval).
# Using these nine summary statistics, the loss is defined as the log of the
# weighted and root mean squared deviation between the actual summary
# statistics (i.e., those we want to reach) and the summary statistics of the
# simulated data. The nine summary statistics can be weighted as desired. Some
# weights can even be set to 0. Furthermore, it is possible to not specify some
# summary statistics at all.
loss <- function(par,
                 km_med_c,
                 km_med_e,
                 cox_hr,
                 time,
                 event,
                 group,
                 km_med_ci_level = 0.95,
                 cox_hr_ci_level = 0.95,
                 # Weights, where the first and second rows are the weights for
                 # the Kaplan-Meier median survival times (and x% confidence
                 # intervals) for the control and experimental conditions,
                 # respectively, and the third row are the weights for the
                 # hazard ratio (and x% confidence interval) for the Cox
                 # proportional hazards regression model.
                 w = c(2, 1, 1,
                       2, 1, 1,
                       6, 3, 3)) {
  time <- time * exp(par)
  c_idx <- which(group == 0)
  e_idx <- which(group == 1)
  sim_km_med_c <- km_med_ci(time = time[c_idx],
                            event = event[c_idx],
                            ci_level = km_med_ci_level)
  sim_km_med_e <- km_med_ci(time = time[e_idx],
                            event = event[e_idx],
                            ci_level = km_med_ci_level)
  sim_cox_hr <- cox_hr_ci(time = time,
                          event = event,
                          group = group,
                          ci_level = cox_hr_ci_level)
  # Actual Kaplan-Meier median survival time for control condition (and x%
  # confidence interval), actual Kaplan-Meier median survival time for
  # experimental condition (and x% confidence interval), and actual hazard
  # ratio of Cox proportional hazards regression model (and x% confidence
  # interval).
  act <- c(km_med_c, km_med_e, cox_hr)
  # Simulated Kaplan-Meier median survival time for control condition (and x%
  # confidence interval), simulated Kaplan-Meier median survival time for
  # experimental condition (and x% confidence interval), and simulated hazard
  # ratio of Cox model (and x% confidence interval).
  sim <- c(sim_km_med_c, sim_km_med_e, sim_cox_hr)
  # Sometimes some values are missing or cannot be estimated. In that case,
  # only use the summary statistics that are available.
  idx <- !is.na(act) & !is.na(sim)
  if (sum(idx) == 0) {
    stop("There are only missing summary statistics.")
  }
  act <- act[idx]
  sim <- sim[idx]
  w <- w[idx]
  w <- w / sum(w)
  # Sometimes the weights for the non-missing summary statistics sum to 0. In
  # that case the loss function cannot be optimized.
  if (sum(w) == 0) {
    stop("The weights for the non-missing summary statistics sum to 0")
  }
  # Mean squared of the scaled and weighted errors.
  ms_scaled_weighted_error <- crossprod((sim - act) / act * w) / length(act)
  log(ms_scaled_weighted_error)
}
