// History computation common to all models

real history_mix[clearN];
  {
    matrix[2, 3] level;
    real current_history[2];
    real tau;

    // initializing parameters to a sample value (can be overwritten with sampled values below)
    real norm_tau[random_levelsN];
    real mixed_states[random_levelsN];
    real history_mixing_prop[random_levelsN];

    // counter for clear states
    int iH = 1;

    // initializing fitted parameters
    if (fit_norm_tau) {
      norm_tau = normalized_tau_random;
    }
    else {
     // rep_vector()!
      for(iR in 1:random_levelsN) norm_tau[iR] = fixed_norm_tau;
    }

    if (fit_mixed_state){
      mixed_states = mixed_state_random;
    }
    else {
     // rep_vector()!
      for(iR in 1:random_levelsN) mixed_states[iR] = fixed_mixed_state;
    }

    if (fit_history_mix) {
      history_mixing_prop = history_mixture_random;
    }
    else {
     // rep_vector()!
      for(iR in 1:random_levelsN) history_mixing_prop[iR] = fixed_history_mix;
    }


    // looping over time series computing history
    for(iT in 1:rowsN){
      // new time-series, recompute absolute tau and reset history state
      if (run_start[iT]){
        tau = session_tmean[iT] * norm_tau[random_effect[iT]];
        current_history = history_starting_values;
        level = state_to_signal_levels(mixed_states[random_effect[iT]]);
      }

      // recording history
      if (is_used[iT] == 1){
        history_mix[iH] = history_mixing_prop[random_effect[iT]] * current_history[state[iT]] +
                          (1 - history_mixing_prop[random_effect[iT]]) * current_history[3-state[iT]];
        iH = iH + 1;
      }

      // computing history for the next episode
      for(iState in 1:2){
        current_history[iState] = compute_history(current_history[iState], level[iState, state[iT]], duration[iT], tau);
      }
    }
  }
