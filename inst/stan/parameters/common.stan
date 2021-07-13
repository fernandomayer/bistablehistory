// Common parameters for NON-HIERACHICAL models
real <lower=0> normalized_tau[fit_norm_tau ? 1 : 0];
real <lower=0, upper=1> mixed_state[fit_mixed_state ? 1 : 0];
real <lower=0, upper=1> history_mixture[fit_history_mix ? 1 : 0];