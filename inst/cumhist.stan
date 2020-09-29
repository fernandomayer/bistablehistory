//
// This Stan program defines the base model for cumulative
// history computation without random and fixed factor.
// It supports for distribution families:
//   1. Normal.
//   2. Gamma.
//   3. Log Normal.
//   4. Exponentially modulated Gaussian.
// Allows to use either fixed or sample level signal for
// mixed/transition phases and history mixture.

#include common_functions.stan

data{
  // Overall time-series
  int<lower=1> rowsN; // Number of rows in the COMPLETE multi-timeseries table including mixed phase
  real duration[rowsN]; // Duration of a dominance/transition phase
  int state[rowsN]; // Index of a dominance state, 1 and 2 code for two competing clear states, 3 - transition/mixed.
  int log_history[rowsN]; // Whether history value must used to predict duration or ignored (first/last )
  int timeseries_start[rowsN]; // 1 marks a beginning of the new time-series (run/block/etc.)
  real timeseries_TMean[rowsN]; // Mean dominance phase duration for both CLEAR percepts. Used to scale time-constant.

  // A shorter clear-states only time-series
  int clearN; // number of rows in the clear-states only time-series
  real clear_duration[clearN]; // Durations of clear percepts only.

  // history starting values, range [0..1]
  real history_starting_values[2];

  // mixed state
  int<lower=0, upper=1> fit_mixed_state; // whether to sample mixed state
  real<lower=0, upper=1> fixed_mixed_state; // a fixed option, no sampling
  real<lower=0, upper=1> mixed_state_prior_mu; // mu (mean) for beta proportion
  real<lower=0> mixed_state_prior_kappa; // kappa (precision) for beta proportion

  // history-mixing proportion, used as history_mix * history_same - (1-history_mix) * history_different
  int<lower=0, upper=1> fit_history_mix; // whether to sample history mixing proportion
  real<lower=0, upper=1> fixed_history_mix; // fixed proportion of history mixing
  real<lower=0, upper=1> history_mix_prior_mu; // mu (mean) for beta proportion
  real<lower=0> history_mix_prior_kappa; // kappa (precision) for beta proportion

  // which family do we fit?
  int<lower=1, upper=4> family;             // see transformed data for family codes

  // gamma prior for normalized tau
  real<lower=0> norm_tau_prior[2];

}
transformed data {
  // internal code for families
  int normal_family = 1;
  int gamma_family = 2;
  int lognormal_family = 3;
  int exgauss_family = 4;

  // number of parameters that are modeled via a linear regression
  // 1 for normal and lognormal: mu
  // 2 for gamma: shape [1] and scale [2]
  // 2 for exgaussian: mu [1] and lambda [2]
  int<lower=1> lm_terms_N = ((family == normal_family) || (family == lognormal_family)) ? 1 : 2;

  // variance, defined for all but gamma
  int<lower=0, upper=1> sigmaN = family != gamma_family ? 1 : 0;
}
parameters{
  // cumulative history constant
  real <lower=0> normalized_tau;

  // non-zero length only if we need to fit it
  real <lower=0, upper=1> mixed_state[fit_mixed_state ? 1 : 0];
  real <lower=0, upper=1> history_mixture[fit_history_mix ? 1 : 0];

  // parameters for linear regression
  real a[lm_terms_N];
  real bHistory[lm_terms_N];

  // variance
  real<lower=0> sigma[sigmaN];
}
transformed parameters{
  real history_same[clearN];
  real history_other[clearN];
  real history_mix[clearN];
  {
    real history_mixing_prop = fit_history_mix ? history_mixture[1] : fixed_history_mix;
    matrix[2, 3] level = state_to_signal_levels(fit_mixed_state ? mixed_state[1] : fixed_mixed_state);
    real current_history[2];
    real tau;

    int iH = 1; // counter for clear states

    // looping over time series computing history
    for(iT in 1:rowsN){
      // new time-series, recompute absolute tau and reset history state
      if (timeseries_start[iT]){
        tau = timeseries_TMean[iT] * normalized_tau;
        current_history = history_starting_values;
      }

      // recording history for clear state
      if (log_history[iT] == 1){
        history_same[iH] = current_history[state[iT]];
        history_other[iH] = current_history[3-state[iT]];
        history_mix[iH] = history_mixing_prop * history_same[iH] + (1 - history_mixing_prop) * history_other[iH];
        iH = iH + 1;
      }

      // computing history for the next episode
      for(iState in 1:2){
        current_history[iState] = compute_history(current_history[iState], level[iState, state[iT]], duration[iT], tau);
      }
    }
  }
}
model{
  // normalized tau for exponential accumulation / decay
  normalized_tau ~ gamma(norm_tau_prior[1], 1/norm_tau_prior[2]);

  // activation during the mixed state / transition
  if (fit_mixed_state) mixed_state ~ beta_proportion(mixed_state_prior_mu, mixed_state_prior_kappa);

  // history mixing propotion
  if (fixed_history_mix) history_mixture ~ beta_proportion(history_mix_prior_mu, history_mix_prior_kappa);

  // linear regression parameters
  a ~ normal(0, 10);
  bHistory ~ normal(0, 1);

  // variance
  if (family != gamma_family) sigma ~ exponential(1);

  // -------------- predicting duration, family-specific --------------
  if (family == normal_family){
    // assuming normal distribution
    for(iClear in 1:clearN){
      clear_duration[iClear] ~ normal(a[1] + bHistory[1] * history_mix[iClear], sigma[1]);
    }
  }
  else if (family == gamma_family){
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
  }
}
// for each family we generate
// * log_lik: a log Likelihood, later used to compute loo/waic information criterion
// * predicted_duration: prediction for expected (mean) duration, computation is family-specific
generated quantities{
  vector[clearN] log_lik;
  vector[clearN] predicted_duration;

  if (family == normal_family){
    // assuming normal distribution
    for (iClear in 1:clearN){
      predicted_duration[iClear] = a[1] + bHistory[1] * history_mix[iClear];
      log_lik[iClear] = normal_lpdf(clear_duration[iClear] | predicted_duration[iClear], sigma[1]);
    }
  }
  else if (family == gamma_family){
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
}
