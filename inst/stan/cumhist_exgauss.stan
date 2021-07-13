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
      clear_duration[iClear] ~ exp_mod_normal(a[1] + bHistory[1] * history_mix[iClear],
                                              sigma,
                                              exp(a[2] + bHistory[2] * history_mix[iClear]));
    }
}
generated quantities{
  vector[clearN] log_lik;
  vector[clearN] predicted_duration;

    real mu;
    real lambda;

    for(iClear in 1:clearN){
      mu = a[1] + bHistory[1] * history_mix[iClear];
      lambda = exp(a[2] + bHistory[2] * history_mix[iClear]);
      predicted_duration[iClear] = mu + 1/lambda;
      log_lik[iClear] = exp_mod_normal_lpdf(clear_duration[iClear] | mu, sigma, lambda);
    }
}
