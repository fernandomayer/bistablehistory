functions {
   vector expand_history_param_to_individuals(int option, real fixed_value, vector mu, vector sigma, vector rnd, int randomN, int link_function){
        // Uses option and parameters to compute value for each individual.
        //
        // Parameters:
        // -----------
        // int option : 1 (constant), 2 (single value), 3 (random independent), 4 (random pooled)
        // real fixed_value : value for option == 1 (oConstant)
        // vector mu : population mean, applicable for option == 2 or option == 4
        // vector sigma : population variance, applicable for option == 4
        // vector rnd : either individual means (option == 3) or individual z-scores (option == 4)
        // int randomN : number of random clusters
        // int link_function : 1 (log), 2 (logit)
        //
        // Returns:
        // -----------
        // vector[randomN] : value for each individual.

       vector[randomN] ind;
       if (option == 1) { // constant
           ind = rep_vector(fixed_value, randomN);
           return ind;
       }
       else if (option == 2) { // single
           ind = rep_vector(mu[1], randomN);
       }
       else if (option == 3) { // random independent
           ind = rnd;
       }
       else {
           // pooled: mean + variance * z-score
           ind = mu[1] + sigma[1] * rnd;
       }

       // using link function
       if (link_function == 1) { //log
           return exp(ind);
       }
       else {
           return inv_logit(ind);
       }
   }
}
data{
    // --- Family choice ---
    int<lower=1, upper=6> family; 
    // 1 - Gamma, linear model for both shape and rate
    // 2 - Log normal with linear model for the mean
    // 3 - Exponentially modulated normal with linear model for the mean and rate
    // 4 - Normal with linear model for the mean

    // --- Complete time-series ---
    int<lower=1> rowsN;   // Number of rows in the COMPLETE multi-timeseries table including mixed phase.
    real duration[rowsN]; // Duration of a dominance/transition phase
    int state[rowsN];     // Index of a dominance state, 1 and 2 code for two competing clear states, 3 - transition/mixed.
    int is_used[rowsN];   // Whether history value must used to predict duration or ignored (mixed phases, warm-up period, last, etc.)
    int run_start[rowsN]; // 1 marks a beginning of the new time-series (run/block/etc.)
    real session_tmean[rowsN];    // Mean dominance phase duration for both CLEAR percepts. Used to scale time-constant.

    // --- A shorter clear-states only time-series ---
    int clearN;                  // Number of rows in the clear-states only time-series
    real clear_duration[clearN]; // Durations of clear percepts only.

    // --- Random effects ---
    int<lower=1> randomN;                          // Number of levels for random effects
    int<lower=1, upper=randomN> random[rowsN];    // Index of a random effect cluster (participant, display, etc.)
    int<lower=1, upper=randomN> random_clear[clearN];    // Index of a random effect cluster (participant, display, etc.)

    // --- Cumulative history parameters
    real<lower=0, upper=1> history_starting_values[2]; // Starting values for cumulative history at the beginning of the run

    // time constant
    int<lower=1, upper=4> tau_option;  // 1 - constant provided by user, 2 - fit single tau for all, 3 - independent taus, 4 - pooled (multilevel) taus
    real<lower=0> fixed_tau;           // a fixed option (tau_option == 1)
    int tau_mu_size;                   // dimensionality, 1 - sampled, 0 - unused
    int tau_sigma_size;                // dimensionality, 1 - sampled, 0 - unused 
    int tau_rnd_size;                  // dimensionality, randomN - sampled, 0 - unused
    real tau_prior[2];                 // prior

    // Mixed state
    int<lower=1, upper=4> mixed_state_option; // 1 - constant provided by user, 2 - fit single tau for all, 3 - independent taus, 4 - pooled (multilevel) taus
    real<lower=0, upper=1> fixed_mixed_state; // a fixed option (tau_option == 1)
    int mixed_state_mu_size;                  // dimensionality, 1 - sampled, 0 - unused
    int mixed_state_sigma_size;               // dimensionality, 1 - sampled, 0 - unused
    int mixed_state_rnd_size;                 // dimensionality, randomN - sampled, 0 - unused
    real mixed_state_prior[2];                // prior

    // History-mixing proportion, used as history_mix * history_same - (1-history_mix) * history_different
    int<lower=1, upper=4> history_mix_option; // 1 - constant provided by user, 2 - fit single tau for all, 3 - independent taus, 4 - pooled (multilevel) taus
    real<lower=0, upper=1> fixed_history_mix; // fixed proportion of history mixing (tau_option == 1)
    int history_mix_mu_size;                  // dimensionality, 1 - sampled, 0 - unused
    int history_mix_sigma_size;               // dimensionality, 1 - sampled, 0 - unused
    int history_mix_rnd_size;                 // dimensionality, randomN - sampled, 0 - unused
    real history_mix_prior[2];                // prior

    // --- Linear model ---
    int<lower=1> lmN; // number of linear models, i.e., distribution parameters that are modelled
    int<lower=0, upper=1> varianceN; // number of variance parameters. Effectively, whether variance is sampled at all(1) or not (0)

    real a_prior[lmN, 2];

    // --- Fixed effects
    // int<lower=1, upper=2> fixed_option; // 1 - fit single population-level value, 2 - pooled (multilevel) effects
    // int fixedN;                         // number of fixed effect terms
    // matrix[clearN, fixedN] fixed;       // values supplied
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

    // Options for fitting fixed effects
    int fSingle = 1;
    int fPooled = 2;

    // Link function codes
    int lfLog = 1;
    int lfLogit = 2;
}
parameters {
    // --- History ---
    // tau
    vector[tau_mu_size] tau_mu;       // population-level mean
    vector[tau_sigma_size] tau_sigma; // population-level variance
    vector[tau_rnd_size] tau_rnd;     // individuals

    // mixed state
    vector[mixed_state_mu_size] mixed_state_mu;       // population-level mean
    vector[mixed_state_sigma_size] mixed_state_sigma; // population-level variance
    vector[mixed_state_rnd_size] mixed_state_rnd;     // individuals

    // History mixture
    vector[history_mix_mu_size] history_mix_mu; // population-level mean
    vector[history_mix_sigma_size] history_mix_sigma; // population-level variance
    vector[history_mix_rnd_size] history_mix_rnd; // individuals

    // --- Linear model ---
    // Independent intercept for each random cluster
    vector[randomN] a[lmN];

    // history terms for linear model
    // vector[history_term_size] bHistory; // population-level mean
    // vector<lower=0>[(randomN > 1) && (fixed_option == fPooled) ? paramsN : 0] bHistory_sigma; // population-level variance (only if there is more than one individual)
    // vector[randomN] bHistory_rnd[(randomN > 1) && (fixed_option == fPooled) ? paramsN : 0]; // individuals
}
transformed parameters {
   {   // Cumulative history parameters
       vector[randomN] tau_ind = expand_history_param_to_individuals(tau_option, fixed_tau, tau_mu, tau_sigma, tau_rnd, randomN, lfLog);
       vector[randomN] mixed_state_ind = expand_history_param_to_individuals(mixed_state_option, fixed_mixed_state, mixed_state_mu, mixed_state_sigma, mixed_state_rnd, randomN, lfLogit);
       vector[randomN] history_mix_ind = expand_history_param_to_individuals(history_mix_option, fixed_history_mix, history_mix_mu, history_mix_sigma, history_mix_rnd, randomN, lfLogit);
   }
}
model {
    {// tau
        if (tau_option == oSingle) {
            tau_mu ~ normal(tau_prior[1], tau_prior[2]);
        }
        else if (tau_option == oIndependent) {
            tau_rnd ~ normal(tau_prior[1], tau_prior[2]);
        }
        else if (tau_option == oPooled) {
            tau_mu ~ normal(tau_prior[1], tau_prior[2]);
            tau_sigma ~ exponential(1);
            tau_rnd ~ normal(0, 1);
        }
    }
    { // Mixed state
        if (mixed_state_option == oSingle) {
            mixed_state_mu ~ normal(mixed_state_prior[1], mixed_state_prior[2]);
        }
        else if (mixed_state_option == oIndependent) {
            mixed_state_rnd ~ normal(mixed_state_prior[1], mixed_state_prior[2]);
        }
        else if (mixed_state_option == oPooled) {
            mixed_state_mu ~ normal(mixed_state_prior[1], mixed_state_prior[2]);
            mixed_state_sigma ~ exponential(1);
            mixed_state_rnd ~ normal(0, 1);
        }
    }
    { // History mixture
        if (history_mix_option == oSingle) {
            history_mix_mu ~ normal(history_mix_prior[1], history_mix_prior[2]);
        }
        else if (history_mix_option == oIndependent) {
            history_mix_rnd ~ normal(history_mix_prior[1], history_mix_prior[2]);
        }
        else if (history_mix_option == oPooled) {
            history_mix_mu ~ normal(history_mix_prior[1], history_mix_prior[2]);
            history_mix_sigma ~ exponential(1);
            history_mix_rnd ~ normal(0, 1);
        }
    }

    // intercepts
    for(iLM in 1:lmN) a[iLM] ~ normal(a_prior[iLM, 1], a_prior[iLM, 2]);

    // if (randomN > 1) {
    //     bHistory_sigma ~ exponential(1);
    //     for(i in 1:history_term_size) bHistory_rnd[i] ~ normal(0, 1);
    // }
    
    //     // history term
    // bHistory ~ normal(0, 1);
    // if (randomN > 1) {
    //     bHistory_sigma ~ exponential(1);
    //     for(i in 1:history_term_size) bHistory_rnd[i] ~ normal(0, 1);
    // }

    // linear model
    {
        for(iClear in 1:clearN){
            clear_duration[iClear] ~ gamma(exp(a[1, random_clear[iClear]]),
                                           exp(a[2, random_clear[iClear]]));
    }
  }
}
// generated quantities{
//   vector[clearN] log_lik;
//   vector[clearN] predicted_duration;

//   // assuming gamma
//   real shape;
//   real scale;

//   for(iClear in 1:clearN){
//     shape = exp(a[1]);
//     scale = exp(a[2]);
//     predicted_duration[iClear] = shape * scale;
//     log_lik[iClear] = gamma_lpdf(clear_duration[iClear] | shape, 1 / scale);
//   }
// }
