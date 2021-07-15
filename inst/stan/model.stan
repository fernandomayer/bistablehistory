data{
    // --- Likelihood choice ---
    int<lower=1, upper=6> likelihood; 
    // 1 - Normal with linear model for the mean
    // 2 - Gamma with linear model for shape and rate
    // 3 - Gamma with linear model for the mean
    // 4 - Gamma with linear model for shape only
    // 5 - Gamma with linear model for rate only
    // 6 - Log normal with linear model for the mean

    // --- Complete time-series ---
    int<lower=1> rowsN;   // Number of rows in the COMPLETE multi-timeseries table including mixed phase.
    real duration[rowsN]; // Duration of a dominance/transition phase
    int state[rowsN];     // Index of a dominance state, 1 and 2 code for two competing clear states, 3 - transition/mixed.
    int is_used[rowsN];   // Whether history value must used to predict duration or ignored (mixed phases, warm-up period, last, etc.)
    int run_start[rowsN]; // 1 marks a beginning of the new time-series (run/block/etc.)
    real tmean[rowsN];    // Mean dominance phase duration for both CLEAR percepts. Used to scale time-constant.

    int<lower=1> randomN;          // Number of levels for random effects
    int<lower=1, upper=randomN> random[rowsN];    // Index of a random effect cluster (participant, display, etc.)

    // --- A shorter clear-states only time-series ---
    int clearN;                  // Number of rows in the clear-states only time-series
    real clear_duration[clearN]; // Durations of clear percepts only.

    // --- Cumulative history parameters
    real<lower=0, upper=1> history_starting_values[2]; // Starting values for cumulative history at the beginning of the run

    int<lower=1, upper=4> tau_option; // 1 - constant provided by user, 2 - fit single tau for all, 3 - independent taus, 4 - pooled (multilevel) taus
    real<lower=0> fixed_tau;          // a fixed option (tau_option == 1)
    real<lower=0> tau_prior[2];       // for tau_option == 2 and tau_option == 3: first two values, shape and RATE (not scale!) for gamma distribution
    real<lower=0> tau_prior_pop[4];   // for tau_option == 4 population-level lognormal priors for shape and rate parameters of gamma distribution

    // mixed state
    int<lower=1, upper=4> mixed_state_option; // 1 - constant provided by user, 2 - fit single tau for all, 3 - independent taus, 4 - pooled (multilevel) taus
    real<lower=0, upper=1> fixed_mixed_state; // a fixed option (tau_option == 1)
    real<lower=0> mixed_state_prior[2];       // for tau_option == 2 and tau_option == 3 : mu and kappa for beta proportion distribution
    real<lower=0> mixed_state_prior_pop[3];   // for tau_option == 4 :mu and kappa for population-level mu (beta proportion) and tau for exponential distribution for population-level kappa

    // history-mixing proportion, used as history_mix * history_same - (1-history_mix) * history_different
    int<lower=1, upper=4> history_mix_option; // 1 - constant provided by user, 2 - fit single tau for all, 3 - independent taus, 4 - pooled (multilevel) taus
    real<lower=0, upper=1> fixed_history_mix; // fixed proportion of history mixing (tau_option == 1)
    real<lower=0> history_mix_prior[2];       // for tau_option == 2 and tau_option == 3 : mu and kappa for beta proportion distribution
    real<lower=0> history_mix_prior_pop[3];   // for tau_option == 4 :mu and kappa for population-level mu (beta proportion) and tau for exponential distribution for population-level kappa
}
transformed data {
    // Constants for likelihood index
    int lNormal = 1;
    int lGammaBoth = 2;
    int lGammaMean = 3;
    int lGammaShape = 4;
    int lGammaRate = 5;
    int lLogNormal = 6;

    // Constants for sampling options for cumulative history parameters
    int oConstant = 1;
    int oSingle = 2;
    int oIndependent = 3;
    int oPooled = 4;

    // --- Dimensionality of vectors, 0 means not used. ---
    // Normalized tau
    int tau_mu_size;
    int tau_sigma_size;
    int tau_rnd_size;
    { // computing sizes for tau parameters
        if (tau_option == oConstant) {
            tau_mu_size = 0;
            tau_sigma_size = 0;
            tau_rnd_size = 0;
        }
        else if (tau_option == oSingle) {
            tau_mu_size = 1;
            tau_sigma_size = 0;
            tau_rnd_size = 0;
        }
        else if (tau_option == oIndependent) {
            tau_mu_size = 0;
            tau_sigma_size = 0;
            tau_rnd_size = randomN;
        }
        else {
            tau_mu_size = 1;
            tau_sigma_size = 1;
            tau_rnd_size = randomN;
        }
    }

    // Mixed state
    int mixed_state_mu_size;
    int mixed_state_sigma_size;
    int mixed_state_rnd_size;
    { // computing size for mixed state parameters
        if (mixed_state_option == oConstant) {
            mixed_state_mu_size = 0;
            mixed_state_sigma_size = 0;
            mixed_state_rnd_size = 0;
        }
        else if (mixed_state_option == oSingle) {
            mixed_state_mu_size = 1;
            mixed_state_sigma_size = 0;
            mixed_state_rnd_size = 0;
        }
        else if (mixed_state_option == oIndependent) {
            mixed_state_mu_size = 0;
            mixed_state_sigma_size = 0;
            mixed_state_rnd_size = randomN;
        }
        else {
            mixed_state_mu_size = 1;
            mixed_state_sigma_size = 1;
            mixed_state_rnd_size = randomN;
        }
    }

    // History mixture
    int history_mix_mu_size;
    int history_mix_sigma_size;
    int history_mix_rnd_size;
    { // computing size for history mixture parameters
        if (history_mix_option == oConstant) {
            history_mix_mu_size = 0;
            history_mix_sigma_size = 0;
            history_mix_rnd_size = 0;
        }
        else if (history_mix_option == oSingle) {
            history_mix_mu_size = 1;
            history_mix_sigma_size = 0;
            history_mix_rnd_size = 0;
        }
        else if (history_mix_option == oIndependent) {
            history_mix_mu_size = 0;
            history_mix_sigma_size = 0;
            history_mix_rnd_size = randomN;
        }
        else {
            history_mix_mu_size = 1;
            history_mix_sigma_size = 1;
            history_mix_rnd_size = randomN;
        }
    }

}
parameters {
    // tau
    vector[tau_mu_size] tau_mu; // population level mean
    vector[tau_sigma_size] tau_sigma; // population level variance
    vector[tau_rnd_size] tau_rnd; // individuals

    // mixed state
    vector[mixed_state_mu_size] mixed_state_mu; // population level mean
    vector[mixed_state_sigma_size] mixed_state_sigma; // population level variance
    vector[mixed_state_rnd_size] mixed_state_rnd; // individuals

    // history mixture
    vector[history_mix_mu_size] history_mix_mu; // population level mean
    vector[history_mix_sigma_size] history_mix_sigma; // population level variance
    vector[history_mix_rnd_size] history_mix_rnd; // individuals
}
transformed parameters {
   { // Cumulative history parameters
       // tau
       vector[randomN] tau_ind;
       if (tau_option == oConstant) {
           tau_ind = rep_vector(fixed_tau, randomN);
       }
       else if (tau_option == oSingle) {
           tau_ind = rep_vector(exp(tau_mu[1]), randomN);
       }
       else if (tau_option == oIndependent) {
           tau_ind = exp(tau_rnd);
       }
       else {
           // pooled: mean + variance * z-score
           tau_ind = exp(tau_mu[1] + tau_sigma[1] * tau_rnd);
       }

       // Mixed state
       vector[randomN] mixed_state_ind;
       if (mixed_state_option == oConstant) {
           mixed_state_ind = rep_vector(fixed_mixed_state, randomN);
       }
       else if (mixed_state_option == oSingle) {
           mixed_state_ind = rep_vector(inv_logit(mixed_state_mu[1]), randomN);
       }
       else if (mixed_state_option == oIndependent) {
           mixed_state_ind = inv_logit(mixed_state_rnd);
       }
       else {
           // pooled: mean + variance * z-score
           mixed_state_ind = inv_logit(mixed_state_mu[1] + mixed_state_sigma[1] * mixed_state_rnd);
       }

       // History mixture
       vector[randomN] history_mix_ind;
       if (history_mix_option == oConstant) {
           history_mix_ind = rep_vector(fixed_history_mix, randomN);
       }
       else if (history_mix_option == oSingle) {
           history_mix_ind = rep_vector(inv_logit(history_mix_mu[1]), randomN);
       }
       else if (history_mix_option == oIndependent) {
           history_mix_ind = inv_logit(history_mix_rnd);
       }
       else {
           // pooled: mean + variance * z-score
           history_mix_ind = inv_logit(history_mix_mu[1] + history_mix_sigma[1] * history_mix_rnd);
       }
   }
}
model {
    // tau
    if (tau_option == oSingle) {
        tau_mu ~ normal(log(1), 0.75);
    }
    else if (tau_option == oIndependent) {
        tau_rnd ~ normal(log(1), 0.75);
    }
    else if (tau_option == oPooled) {
        tau_mu ~ normal(log(1), 0.75);
        tau_sigma ~ exponential(1);
        tau_rnd ~ normal(0, 1);
    }

    // Mixed state
    if (mixed_state_option == oSingle) {
        mixed_state_mu ~ normal(0, 1);
    }
    else if (mixed_state_option == oIndependent) {
        mixed_state_rnd ~ normal(0, 1);
    }
    else if (mixed_state_option == oPooled) {
        mixed_state_mu ~ normal(0, 1);
        mixed_state_sigma ~ exponential(1);
        mixed_state_rnd ~ normal(0, 1);
    }

    // History mixture
    if (history_mix_option == oSingle) {
        history_mix_mu ~ normal(0, 1);
    }
    else if (history_mix_option == oIndependent) {
        history_mix_rnd ~ normal(0, 1);
    }
    else if (history_mix_option == oPooled) {
        history_mix_mu ~ normal(0, 1);
        history_mix_sigma ~ exponential(1);
        history_mix_rnd ~ normal(0, 1);
    }
}
