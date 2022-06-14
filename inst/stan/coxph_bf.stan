
functions {

  real cph(real beta, vector time, vector event, vector group, vector surv_ord_uniq) {

    int r;
    int d;
    real tmp_1;
    real tmp_2a;
    real tmp_2b;
    real tmp_2c;
    real res_i = 0;
    real res_j;

    for (i in 1:num_elements(surv_ord_uniq)) {
      r = 0;
      d = 0;
      tmp_1 = 0;
      tmp_2a = 0;
      tmp_2c = 0;
      for (z in 1:num_elements(time)) {
        if (time[z] >= surv_ord_uniq[i]) {
          r += 1;
          tmp_2a += exp(group[z] * beta);
        }
        if (time[z] == surv_ord_uniq[i] && event[z] == 1) {
          d += 1;
          tmp_1 += group[z] * beta;
          tmp_2c += exp(group[z] * beta);
        }
      }
      res_j = 0;
      for (j in 1:d) {
        tmp_2b = ((j - 1.0) / d);
        res_j += log(tmp_2a - tmp_2b * tmp_2c);
      }
      res_i += tmp_1 - res_j;
    }

    return(res_i);

  }

}

data {
  int<lower=1> n_time;
  vector<lower=0>[n_time] time;
  vector[n_time] event;
  vector[n_time] group;
  int<lower=1> n_surv;
  vector[n_surv] surv_ord_uniq;
  real prior_mean;
  real<lower=0> prior_sd;
  real null_hypothesis;
  int<lower=0,upper=1> alternative;
  int<lower=-1,upper=1> direction;
}

parameters {
  real<lower=(alternative == 1 && direction == 1 ? null_hypothesis : negative_infinity()),upper=(alternative == 1 && direction == -1 ? null_hypothesis : positive_infinity())> beta;
}

model {
  if (alternative == 0) {
    target += normal_lpdf(beta | prior_mean, prior_sd);
    target += cph(beta, time, event, group, surv_ord_uniq);
  } else {
    if (direction == -1) {
      target += normal_lpdf(beta | prior_mean, prior_sd) - normal_lcdf(null_hypothesis | prior_mean, prior_sd);
      target += cph(beta, time, event, group, surv_ord_uniq);
    }
    if (direction == 1) {
      target += normal_lpdf(beta | prior_mean, prior_sd) - normal_lccdf(null_hypothesis | prior_mean, prior_sd);
      target += cph(beta, time, event, group, surv_ord_uniq);
    }
  }
}
