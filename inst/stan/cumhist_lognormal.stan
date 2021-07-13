// lognormal
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
      clear_duration[iClear] ~ lognormal(exp(a + bHistory * history_mix[iClear]), sigma);
    }
}
generated quantities{
  vector[clearN] log_lik;
  vector[clearN] predicted_duration;

    real lognormal_mu;
    for (iClear in 1:clearN){
      lognormal_mu = exp(a + bHistory * history_mix[iClear]);
      predicted_duration[iClear] = exp(lognormal_mu + sigma * sigma / 2);
      log_lik[iClear] = lognormal_lpdf(clear_duration[iClear] | lognormal_mu, sigma);
    }
}
