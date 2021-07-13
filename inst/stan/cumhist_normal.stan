// normal
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
  real a;
  real bHistory;
  real<lower=0> sigma;
}
transformed parameters{
#include /transformed_parameters/history_computation.stan
}
model{
#include /model/common.stan

  // linear regression parameters
  a ~ normal(0, 10);
  bHistory ~ normal(0, 1);
  sigma ~ exponential(1);

  // predicting duration family-based
    for(iClear in 1:clearN){
      clear_duration[iClear] ~ normal(a + bHistory * history_mix[iClear], sigma);
    }
}
generated quantities{
  vector[clearN] log_lik;
  vector[clearN] predicted_duration;

    for (iClear in 1:clearN){
      predicted_duration[iClear] = a + bHistory * history_mix[iClear];
      log_lik[iClear] = normal_lpdf(clear_duration[iClear] | predicted_duration[iClear], sigma);
    }
}
