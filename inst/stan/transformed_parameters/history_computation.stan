// History computation common to all models

real history_mix[clearN];
  {
    real norm_tau = fit_norm_tau ? normalized_tau[1] : fixed_norm_tau;
    real history_mixing_prop = fit_history_mix ? history_mixture[1] : fixed_history_mix;
    matrix[2, 3] level = state_to_signal_levels(fit_mixed_state ? mixed_state[1] : fixed_mixed_state);
    real current_history[2];
    real tau;

    // counter for clear states
    int iH = 1;

    // looping over time series computing history
    for(iT in 1:rowsN){
      // new time-series, recompute absolute tau and reset history state
      if (run_start[iT]){
        tau = session_tmean[iT] * norm_tau;
        current_history = history_starting_values;
      }

      // recording history
      if (is_used[iT] == 1){
        history_mix[iH] = history_mixing_prop * current_history[state[iT]] +
                          (1 - history_mixing_prop) * current_history[3-state[iT]];
        iH = iH + 1;
      }

      // computing history for the next episode
      for(iState in 1:2){
        current_history[iState] = compute_history(current_history[iState], level[iState, state[iT]], duration[iT], tau);
      }
    }
  }
