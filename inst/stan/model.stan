data{
    // --- Family choice ---
    int<lower=1, upper=6> family; 
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

    int<lower=1, upper=4> tau_option;  // 1 - constant provided by user, 2 - fit single tau for all, 3 - independent taus, 4 - pooled (multilevel) taus
    real<lower=0> fixed_tau;           // a fixed option (tau_option == 1)
    int tau_mu_size;                   // dimensionality, 1 - sampled, 0 - unused
    int tau_sigma_size;                // dimensionality, 1 - sampled, 0 - unused 
    int tau_rnd_size;                  // dimensionality, randomN - sampled, 0 - unused

    // mixed state
    int<lower=1, upper=4> mixed_state_option; // 1 - constant provided by user, 2 - fit single tau for all, 3 - independent taus, 4 - pooled (multilevel) taus
    real<lower=0, upper=1> fixed_mixed_state; // a fixed option (tau_option == 1)
    int mixed_state_mu_size;                  // dimensionality, 1 - sampled, 0 - unused
    int mixed_state_sigma_size;               // dimensionality, 1 - sampled, 0 - unused
    int mixed_state_rnd_size;                 // dimensionality, randomN - sampled, 0 - unused

    // history-mixing proportion, used as history_mix * history_same - (1-history_mix) * history_different
    int<lower=1, upper=4> history_mix_option; // 1 - constant provided by user, 2 - fit single tau for all, 3 - independent taus, 4 - pooled (multilevel) taus
    real<lower=0, upper=1> fixed_history_mix; // fixed proportion of history mixing (tau_option == 1)
    int history_mix_mu_size;                  // dimensionality, 1 - sampled, 0 - unused
    int history_mix_sigma_size;               // dimensionality, 1 - sampled, 0 - unused
    int history_mix_rnd_size;                 // dimensionality, randomN - sampled, 0 - unused

    // --- Fixed effects
    int<lower=1, upper=2> fixed_option; // 1 - fit single population-level value, 2 - pooled (multilevel) effects
    int fixedN;                         // number of fixed effect terms
    matrix[clearN, fixedN] fixed;       // values supplied
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

    // --- Family-specific number of parameters ---
    int paramsN = 2; // number of likelihood parameters to be fitted
    { // dimensions for gamma distribution parameters
        if (family == lGammaBoth) {
            paramsN = 2;
        }
        else  {
            paramsN = 1;
        }
    }

    // --- Dimensions for the intercept term ---
    int a_size;
    int a_sigma_size;
    int a_rnd_size;
    if (randomN == 1) {
        // single random factor -> single slope
        a_size = paramsN;
        a_sigma_size = 0;
        a_rnd_size = 0;
    }
    else if (fixed_option == fSingle) {
        // independent intercepts, rm ANOVA style
        a_size = 0;
        a_sigma_size = 0;
        a_rnd_size = paramsN;
    }
    else if (fixed_option == fPooled) {
        // pooled intercepts
        a_size = paramsN;
        a_sigma_size = paramsN;
        a_rnd_size = paramsN;
    }

    // --- Priors ---
    row_vector[2] priorGammaBothShape = [log(3), 5];
    row_vector[2] priorGammaBothScale = [log(3), 5];
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

    // history mixture
    vector[history_mix_mu_size] history_mix_mu; // population-level mean
    vector[history_mix_sigma_size] history_mix_sigma; // population-level variance
    vector[history_mix_rnd_size] history_mix_rnd; // individuals

    // intercept parameter for the linear model
    vector[a_size] a;                       // population-level mean
    vector<lower=0>[a_sigma_size] a_sigma;  // population-level variance (only if there is more than one individual)
    vector[randomN] a_rnd[a_rnd_size];      // individuals

    // history terms for linear model
    // vector[history_term_size] bHistory; // population-level mean
    // vector<lower=0>[(randomN > 1) && (fixed_option == fPooled) ? paramsN : 0] bHistory_sigma; // population-level variance (only if there is more than one individual)
    // vector[randomN] bHistory_rnd[(randomN > 1) && (fixed_option == fPooled) ? paramsN : 0]; // individuals
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

    // intercepts
    if (a_size > 0) { // we need to sample global intercept 
        if (family == lGammaBoth){
            a[1] ~ normal(priorGammaBothShape[1], priorGammaBothShape[2]);
            a[2] ~ normal(priorGammaBothScale[1], priorGammaBothScale[2]);
        }
    }
    if (a_sigma_size > 0) { // we need to sample variance of intercepts
        a_sigma ~ exponential(1);
    }
    if (a_rnd_size > 0) {
        if (fixed_option == fSingle){
            // independent intercepts
            if (family == lGammaBoth){
                a_rnd[1] ~ normal(priorGammaBothShape[1], priorGammaBothShape[2]);
                a_rnd[2] ~ normal(priorGammaBothScale[1], priorGammaBothScale[2]);
            }
        }
        else {
            // pooled intercepts : z-scores
            for(i in 1:paramsN) a_rnd[i] ~ normal(0, 1);
        }
    }
    
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
        // individual intercepts
        real a_ind[paramsN, randomN];
        for(iP in 1:paramsN){
            for(iR in 1:randomN){
                if (randomN == 1){
                    // single intercept
                    a_ind[iP, iR] = a[iP];
                }
                else if (fixed_option == fSingle){
                    // independent intercepts
                    a_ind[iP, iR] = a_rnd[iP][iR];
                }
                else {
                    // pooled intercepts
                    a_ind[iP, iR] = a[iP] + a_sigma[iP] * a_rnd[iP][iR];
                }
            }
        }

        for(iClear in 1:clearN){
            clear_duration[iClear] ~ gamma(exp(a_ind[1, random_clear[iClear]]),
                                           1 / exp(a_ind[2, random_clear[iClear]]));
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
