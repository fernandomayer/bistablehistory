#' Fits cumulative history for bistable perceptual rivalry displays.
#'
#' Fits a generalized linear model using cumulative history and
#' specified fixed effects.
#'
#'
#' @param data A table with time-series.
#' @param state String, the name of the column that specifies
#' perceptual state. The column type should be a factor with
#' two or three levels (the third level is assumed to correspond to a
#' transition/mixed phase) or should be convertible to a two level
#' factor (as it would be impossible to infer the identity of transition/
#' mixed phase).
#' @param duration String, name of the column with duration of individual
#' perceptual dominance phases. Optional, you can specify \code{onset}
#' instead.
#' @param onset String, name of the column with onsets of the perceptual
#' dominance states. Optional, used to compute duration of the dominance
#' phases, if these are not provided explicitly via \code{duration}
#' parameter.
#' @param random_effect String, name of the column that identifies random effect,
#' e.g. individual participants, stimuli for a single participant, etc.
#' If omitted, no random effect is assumed. If specified and
#' there is more than one level (participant, stimulus, etc.), it is used
#' in a hierarchical model.
#' @param fixed_effects String or vector of strings. Name of column(s)
#' with values to be used for fitting an additional fixed effect(s). E.g.,
#' contrast in binocular rivalry, rotation speed for kinectic-depth effect,
#' etc.
#' @param session String, name of the column that identifies unique
#' experimental session for which a mean dominance phase duration will
#' be computed (see \code{norm_tau} parameter). Code assumes that session
#' IDs are different within a participant but can be the same between them.
#' If omitted, a single mean dominance duration based on the entire time series
#' is used.
#' @param run String, name of the column that identifies unique runs/blocks.
#' If omitted, the data is assumed to belong to a single time series. Code
#' assumes that run IDs are different within an experimental session but
#' can be the same between the session. E.g. session A, runs 1, 2, 3.. and
#' session B, runs 1, 2, 3 but not session A, runs 1, 2, 1.
#' @param tau Time constant of exponential growth/decay
#' normalized to the mean duration of clear percepts within each \code{session}.
#' Can be 1) a single positive number (>0) that is used for all participants and runs,
#' 2) \code{NULL} (default) -  a _single_ value will be fitted for all participants and runs,
#' 3) \code{"random"} - an independent tau is fitted for each random cluster,
#' 4) \code{"1|random"}- a tau for a random cluster
#' is sampled from a population distribution, i.e., pooled parameter values via
#' a multilevel model.
#' @param mixed_state Specifies an activation level during
#' transition/mixed phases (state #3, see \code{state}). Either a single
#' number (range 0..1) that will be used as a fixed level or a vector
#' of two numbers \code{c(mu, kappa)} that specifies, correspondingly, mean
#' (range 0..1) and precision (>0) of beta proportion distribution, it
#' should be sampled from. Defaults to a fixed value of \code{0.5}.
#' @param history_mix Specifies how cumulative history levels for
#' the same and opposite perceptual states are combined when used to
#' predict dominance phase durations, so that \code{history = history_mix
#' * history_same + (1 - history_mix) * history_opposite}. Either a single
#' number (range 0..1) that will be used as a fixed level or a vector
#' of two numbers \code{c(mu, kappa)} that specifies, correspondingly, mean
#' (range 0..1) and precision (>0) of beta proportion distribution, it
#' should be sampled from. Defaults to a fixed value of \code{0}
#' (predictions are made using cumulative history for the opposite state).
#' @param history_init Initial value for cumulative history computation. Either
#' a numeric scalar in 0..1 range or a vector of two numbers in 0..1 range.
#' In the latter case, two histories will start at different levels.
#' @param family String, distribution used to fit duration of perceptual dominance
#' phases. Options include \code{"gamma"} (default), \code{"lognormal"}, and \code{"normal"}.
#' @param chains Number of chains for sampling.
#' @param cores Number of CPU cores to use for sampling. If omitted, All cores are used.
#' @param ... Additional arguments passed to [rstan::sampling()][rstan::sampling] function.
#'
#' @return  An object of class [cumhist][cumhist-class()]
#'
#' @importFrom future availableCores
#' @importFrom rstan sampling
#' @export
#'
#' @examples
#' \donttest{
#' data(br_singleblock)
#' gamma_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' }
fit_cumhist <- function(data,
                        state,
                        duration=NULL,
                        onset=NULL,
                        random_effect=NULL,
                        session=NULL,
                        run=NULL,
                        fixed_effects=NULL,
                        tau=NULL,
                        mixed_state=0.5,
                        history_mix=0.2,
                        history_init=0,
                        family="gamma",
                        chains=1,
                        cores=NULL,
                        ...){

  # ----------------------------- Input checks -----------------------------
  cumhist <- list(data = as.list(bistablehistory::preprocess_data(data, state, duration, onset, random_effect, session, run)))
  class(cumhist) <- "cumhist"
  cumhist$state <- state
  cumhist$duration <- duration
  cumhist$onset <- onset
  cumhist$random_effect <- random_effect
  cumhist$session <- session
  cumhist$run <- run
  cumhist$fixed_effects <- fixed_effects
  cumhist$tau <- tau
  cumhist$mixed_state <- mixed_state
  cumhist$history_mix <- history_mix
  cumhist$history_init <- history_init
  cumhist$Call <- match.call()

  ## --- 1. Prepare clean data ---
  cumhist$data$clear_duration <- cumhist$data$duration[cumhist$data$is_used]
  cumhist$data$irandom_clear <-cumhist$data$irandom[cumhist$data$is_used]

  ## --- 2. Counts ---
  cumhist$data$rowsN <- length(cumhist$data$duration)
  cumhist$data$randomN <- length(unique(cumhist$data$random))
  cumhist$data$clearN <- length(cumhist$data$clear_duration)

  ## --- 3. Check fixed effects
  if (is.null(fixed_effects)) {
    cumhist$data$fixedN <- 0
    cumhist$data$fixed_clear <- matrix(0, nrow=cumhist$data$clearN, ncol=1)
  }
  else {
    # Checking that all columns are valid
    for(current_fixed in fixed_effects){
      if (!current_fixed %in% colnames(data)) stop(sprintf("Column '%s' for fixed effect variable is not in the table", current_fixed))
      if (sum(is.na(data[[current_fixed]]))>0) stop(sprintf("Column '%s' contains NAs", current_fixed))
      if (!is.numeric(data[[current_fixed]])) stop(sprintf("Column '%s' is not numeric", current_fixed))
    }

    cumhist$data$fixedN <- length(fixed_effects)
    cumhist$data$fixed_clear <- as.matrix(data[cumhist$data$is_used, fixed_effects])
  }

  ## --- 4. History parameters ---
  cumhist$data <- c(cumhist$data,
                    bistablehistory::evaluate_history_option("tau", tau, cumhist$data$randomN, Inf),
                    bistablehistory::evaluate_history_option("mixed_state", mixed_state, cumhist$data$randomN, 1),
                    bistablehistory::evaluate_history_option("history_mix", history_mix, cumhist$data$randomN, 1),
                    list("tau_prior"=c(log(1), 0.75),
                         "mixed_state_prior"=c(0, 1),
                         "history_mix_prior"=c(0, 1)))

  # --- 5. Check history_init
  cumhist$data$history_starting_values <- bistablehistory::evaluate_history_init(history_init)

  ## --- 6. Family
  supported_families <- c("gamma"=1, "lognormal"=2, "normal"=3)
  if (!family %in% names(supported_families)) stop(sprintf("Unsupported distribution family '%s'", family))
  lmN <- c("gamma"=2, "lognormal"=1, "exgauss"=2, "normal"=1)
  varianceN <- c("gamma"=0, "lognormal"=1, "normal"=1)
  a_prior <- list("gamma" = matrix(c(log(3), 5, log(3), 5), nrow=2, byrow = TRUE),
                  "lognormal" = matrix(c(log(3), 5), nrow=1, byrow = TRUE),
                  "normal" = matrix(c(3, 5), nrow=1, byrow = TRUE))

  cumhist$family <- family
  cumhist$data$family <- supported_families[family]
  cumhist$data$lmN <- lmN[family]
  cumhist$data$varianceN <- varianceN[family]
  cumhist$data$a_prior <- a_prior[[family]]

  # ----------------------------- Sampling -----------------------------
  # deciding on number of cores
  if (is.null(cores)) cores = future::availableCores()

  if (chains > 0) {
    cumhist$stanfit <- rstan::sampling(stanmodels$historylm,
                                      data=cumhist$data,
                                      chains=chains,
                                      cores=cores,
                                      ...)
  }
  else {
    message("Zero chain specified, skipping sampling.")
  }

  cumhist
}
