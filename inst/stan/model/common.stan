// Common prior for NON-HIERARCHICAL models

// normalized tau for exponential accumulation / decay
if (fit_norm_tau) normalized_tau ~ gamma(norm_tau_prior[1], 1/norm_tau_prior[2]);

// activation during the mixed state / transition
if (fit_mixed_state) mixed_state ~ beta_proportion(mixed_state_prior_mu, mixed_state_prior_kappa);

// history mixing propotion
if (fixed_history_mix) history_mixture ~ beta_proportion(history_mix_prior_mu, history_mix_prior_kappa);