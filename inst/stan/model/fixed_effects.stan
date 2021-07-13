// Same weakly regularizing normal prior for all fixed effects
for(iLM in 1:lmN){
    bFixed[iLM] ~ normal(0, 1);
}
