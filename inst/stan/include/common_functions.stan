/* Common functions for all models */

functions {
  /**
  * Creates signal level matrix based on target state (row) and
  *   dominant state (column). Uses 1 (maximum) if states match,
  *   0 (minimum) if an opposite clear state is dominant, 
  *   mixed_value, if it is a transition.
  * @param mixed_level. Signal level that corresponds to mixed 
  *   perception / transition. Real, range [0..1].
  * @return A matrix[2, 3] with row for target state and column
  *   for the dominant state.
  */
  matrix state_to_signal_levels(real mixed_level){
    matrix [2, 3] signal_level;
  
    signal_level[1, 1] = 1;
    signal_level[1, 2] = 0;
    signal_level[1, 3] = mixed_level;
    signal_level[2, 1] = 0;
    signal_level[2, 2] = 1;
    signal_level[2, 3] = mixed_level;

    return signal_level;
  }
  
  /**
  * Computes next state of history assuming negative exponential growth/decay
  *   with scale time constant (tau) from current level (history) to the 
  *   signal level (signal) following the next perceptual state duration
  *   (duration).
  * @param history Current value of history. Real, range [0..1].
  * @param signal Signal level to which history grows/decays. Real, 
  *   range [0..1].
  * @param duration Duration of the signal. Real, >0.
  * @param tau Time constant (scale) of the negative exponential. Real, >0.
  * @return Real value for history state after the signal.
  */
  real compute_history(real history, real signal, real duration, real tau){
      if (history == signal){
        // special case, history is already at the final reachable level
        return history;
      }
      else if (history > signal) {
        // decreasing history to the signal level
          real  starting_x = -tau * log((history - signal) / (1-signal));
          return (1 - signal) * exp(-(duration + starting_x) / tau) + signal;
      }
      else {
        // increasing history to the signal level
        real starting_x = -tau * log((signal - history)/signal);
        return signal * (1- exp(-(duration + starting_x) / tau));
      }
  }
}