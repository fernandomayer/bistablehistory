//
// This Stan program defines the base model for cumulative
// history computation without random and fixed factor (apart
// from history).
// It supports for distribution families:
//   1. Gamma.
//   2. Log Normal.
//   3. Exponentially modulated Gaussian.
//   4. Normal.
// Allows to use either fixed or sample level signal for
// mixed/transition phases and history mixture.

functions {
  #include /functions/common_functions.stan
}
data{
  #include /data/common_data.stan

  // which family do we fit?
  int<lower=1, upper=4> family;             // see transformed data for family codes
}
transformed data {
  #include /transformed_data/family_codes.stan

  // number of parameters that are modeled via a linear regression
  // 1 for normal and lognormal: mu
  // 2 for gamma: shape and scale
  // 2 for exgaussian: mu and lambda
  int<lower=1> lm_terms_N = ((family == normal_family) || (family == lognormal_family)) ? 1 : 2;

  // variance, defined for all but gamma
  int<lower=0, upper=1> sigmaN = family != gamma_family ? 1 : 0;
}
parameters{
  // non-zero length only if we need to fit it
  real <lower=0> normalized_tau[fit_norm_tau ? 1 : 0];
  real <lower=0, upper=1> mixed_state[fit_mixed_state ? 1 : 0];
  real <lower=0, upper=1> history_mixture[fit_history_mix ? 1 : 0];

  // parameters for linear regression
  real a[lm_terms_N];
  real bHistory[lm_terms_N];

  // variance
  real<lower=0> sigma[sigmaN];
}
transformed parameters{
  #include /transformed_parameters/history_computation.stan
}
model{
  // normalized tau for exponential accumulation / decay
  if (fit_norm_tau) normalized_tau ~ gamma(norm_tau_prior[1], 1/norm_tau_prior[2]);

  // activation during the mixed state / transition
  if (fit_mixed_state) mixed_state ~ beta_proportion(mixed_state_prior_mu, mixed_state_prior_kappa);

  // history mixing propotion
  if (fixed_history_mix) history_mixture ~ beta_proportion(history_mix_prior_mu, history_mix_prior_kappa);

  // linear regression parameters
  a ~ normal(0, 10);
  bHistory ~ normal(0, 1);

  // variance
  if (family != gamma_family) sigma ~ exponential(1);


  // predicting duration family-based
  if (family == gamma_family){
    // assuming gamma
    for(iClear in 1:clearN){
      clear_duration[iClear] ~ gamma(exp(a[1] + bHistory[1] * history_mix[iClear]),
                                     1/exp(a[2] + bHistory[2] * history_mix[iClear]));
    }
  }
  else if (family == lognormal_family){
    // assuming lognormal distribution
    for(iClear in 1:clearN){
      clear_duration[iClear] ~ lognormal(exp(a[1] + bHistory[1] * history_mix[iClear]), sigma[1]);
    }
  }
  else if (family == exgauss_family){
    // assuming exponentially  modulated normal distribution
    for(iClear in 1:clearN){
      clear_duration[iClear] ~ exp_mod_normal(a[1] + bHistory[1] * history_mix[iClear],
                                              sigma[1],
                                              exp(a[2] + bHistory[2] * history_mix[iClear]));
    }
  } else if (family == normal_family){
    // assuming normal distribution
    for(iClear in 1:clearN){
      clear_duration[iClear] ~ normal(a[1] + bHistory[1] * history_mix[iClear], sigma[1]);
    }
  }

}
generated quantities{
  vector[clearN] log_lik;
  vector[clearN] predicted_duration;

  if (family == gamma_family){
    // assuming gamma
    real shape;
    real scale;

    for(iClear in 1:clearN){
      shape = exp(a[1] + bHistory[1] * history_mix[iClear]);
      scale = exp(a[2] + bHistory[2] * history_mix[iClear]);
      predicted_duration[iClear] = shape * scale;
      log_lik[iClear] = gamma_lpdf(clear_duration[iClear] | shape, 1/scale);
    }
  }
  else if (family == lognormal_family){
    real lognormal_mu;
    // assuming normal distribution
    for (iClear in 1:clearN){
      lognormal_mu = exp(a[1] + bHistory[1] * history_mix[iClear]);
      predicted_duration[iClear] = exp(lognormal_mu + sigma[1] * sigma[1] / 2);
      log_lik[iClear] = lognormal_lpdf(clear_duration[iClear] | lognormal_mu, sigma[1]);
    }
  }
  else if (family == exgauss_family){
    // assuming exponentially modulated gaussian
    real mu;
    real lambda;

    for(iClear in 1:clearN){
      mu = a[1] + bHistory[1] * history_mix[iClear];
      lambda = exp(a[2] + bHistory[2] * history_mix[iClear]);
      predicted_duration[iClear] = mu + 1/lambda;
      log_lik[iClear] = exp_mod_normal_lpdf(clear_duration[iClear] | mu, sigma[1], lambda);
    }
  }
  else if (family == normal_family){
    // assuming normal distribution
    for (iClear in 1:clearN){
      predicted_duration[iClear] = a[1] + bHistory[1] * history_mix[iClear];
      log_lik[iClear] = normal_lpdf(clear_duration[iClear] | predicted_duration[iClear], sigma[1]);
    }
  }
}
