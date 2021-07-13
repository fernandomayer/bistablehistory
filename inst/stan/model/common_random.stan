// Common prior for HIERARCHICAL models

// normalized tau for exponential accumulation / decay
if (fit_norm_tau){
    normalized_tau_sigma ~ exponential(1);
    normalized_tau_random ~ normal(normalized_tau[1], normalized_tau_sigma[1]);
}

// activation during the mixed state / transition
if (fit_mixed_state){
    mixed_state_invkappa ~ exponential(1);
    mixed_state_random ~ beta_proportion(mixed_state[1], 1/mixed_state_invkappa[1]);
}

// history mixing propotion
if (fixed_history_mix){
    history_mixture_invkappa ~ exponential(1);
    history_mixture_random ~ beta_proportion(history_mixture[1], 1/history_mixture_invkappa[1]);
}