// Data part common to all models

// Overall time-series
int<lower=1> rowsN; // Number of rows in the COMPLETE multi-timeseries table including mixed phase
real duration[rowsN]; // Duration of a dominance/transition phase
int state[rowsN]; // Index of a dominance state, 1 and 2 code for two competing clear states, 3 - transition/mixed.
int is_used[rowsN]; // Whether history value must used to predict duration or ignored (first/last )
int run_start[rowsN]; // 1 marks a beginning of the new time-series (run/block/etc.)
real session_tmean[rowsN]; // Mean dominance phase duration for both CLEAR percepts. Used to scale time-constant.

// A shorter clear-states only time-series
int clearN; // number of rows in the clear-states only time-series
real clear_duration[clearN]; // Durations of clear percepts only.

// history starting values, range [0..1]
real history_starting_values[2];

// gamma prior for normalized tau
int<lower=0, upper=1> fit_norm_tau; // whether to sample tau
real<lower=0> fixed_norm_tau; // a fixed option, no sampling
real<lower=0> norm_tau_prior[2]; // shape and RATE for gamma distribution

// mixed state
int<lower=0, upper=1> fit_mixed_state; // whether to sample mixed state
real<lower=0, upper=1> fixed_mixed_state; // a fixed option, no sampling
real<lower=0, upper=1> mixed_state_prior_mu; // mu (mean) for beta proportion
real<lower=0> mixed_state_prior_kappa; // kappa (precision) for beta proportion

// history-mixing proportion, used as history_mix * history_same - (1-history_mix) * history_different
int<lower=0, upper=1> fit_history_mix; // whether to sample history mixing proportion
real<lower=0, upper=1> fixed_history_mix; // fixed proportion of history mixing
real<lower=0, upper=1> history_mix_prior_mu; // mu (mean) for beta proportion
real<lower=0> history_mix_prior_kappa; // kappa (precision) for beta proportion
