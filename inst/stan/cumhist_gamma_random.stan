// gamma
//
// This Stan program defines the base model with GAMMA distribution
// family for cumulative history computation without random and fixed
// factor (apart from history).
//
// Allows to use either fixed or sample for normalized tau,
// level signal for mixed/transition phases and history mixture.

functions {
#include /functions/common.stan
}
data{
#include /data/common.stan
#include /data/random_effects.stan
}
transformed data{
  // Number of linear models we fit,
  // one for each distribution parameter.
  int lmN = 2; 
}
parameters{
#include /parameters/common.stan
#include /parameters/common_random.stan

  // parameters for linear regression
  real a[lmN];
  real bHistory[lmN];

  // parameters for hierarchical part
  real <lower=0> bHistory_sigma[lmN];
  vector[random_levelsN] bHistory_random[lmN];
}
transformed parameters{
#include /transformed_parameters/history_computation_random.stan
}
model{
#include /model/common_random.stan

  // linear regression parameters
  a ~ normal(0, 10);
  bHistory ~ normal(0, 1);

  // hierarchical part for linear regression
  bHistory_sigma ~ exponential(1);
  for(iLM in 1:lmN) bHistory_random[iLM] ~ normal(bHistory[iLM], bHistory_sigma[iLM]);

  // predicting duration family-based
  for(iClear in 1:clearN){
    clear_duration[iClear] ~ gamma(exp(a[1] + bHistory_random[1][clear_random_effect[iClear]] * history_mix[iClear]),
                                   1/exp(a[2] + bHistory_random[2][clear_random_effect[iClear]] * history_mix[iClear]));
  }
}
generated quantities{
  vector[clearN] log_lik;
  vector[clearN] predicted_duration;

  // assuming gamma
  real shape;
  real scale;

  for(iClear in 1:clearN){
    shape = exp(a[1] + bHistory_random[1][clear_random_effect[iClear]] * history_mix[iClear]);
    scale = exp(a[2] + bHistory_random[2][clear_random_effect[iClear]] * history_mix[iClear]);
    predicted_duration[iClear] = shape * scale;
    log_lik[iClear] = gamma_lpdf(clear_duration[iClear] | shape, 1/scale);
  }
}
