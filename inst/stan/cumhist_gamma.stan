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
}
parameters{
#include /parameters/common.stan

  // parameters for linear regression
  real a[2];
  real bHistory[2];
}
transformed parameters{
#include /transformed_parameters/history_computation.stan
}
model{
#include /model/common.stan

  // linear regression parameters
  a ~ normal(0, 10);
  bHistory ~ normal(0, 1);

  // predicting duration family-based
  for(iClear in 1:clearN){
    clear_duration[iClear] ~ gamma(exp(a[1] + bHistory[1] * history_mix[iClear]),
                                   1/exp(a[2] + bHistory[2] * history_mix[iClear]));
  }
}
generated quantities{
  vector[clearN] log_lik;
  vector[clearN] predicted_duration;

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
