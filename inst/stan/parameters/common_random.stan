// additional parameters when common parameters are modeled
// hierarchically
real <lower=0> normalized_tau_sigma[fit_norm_tau ? 1 : 0];
real <lower=0> mixed_state_invkappa[fit_mixed_state ? 1 : 0];
real <lower=0> history_mixture_invkappa[fit_history_mix ? 1 : 0];

real <lower=0> normalized_tau_random[fit_norm_tau ? random_levelsN : 0];
real <lower=0, upper=1> mixed_state_random[fit_mixed_state ? 1 : 0];
real <lower=0, upper=1> history_mixture_random[fit_history_mix ? 1 : 0];