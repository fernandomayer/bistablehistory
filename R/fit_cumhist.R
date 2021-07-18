# Main package interface function that computes
# cumulative history for bistable perceptual rivalry displays.

#' Computes cumulative history for bistable perceptual
#' rivalry displays.
#'
#' @param data A table with all time-series.
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
#' number (range [0..1]) that will be used as a fixed level or a vector
#' of two numbers \code{c(mu, kappa)} that specifies, correspondingly, mean
#' (range [0..1]) and precision (>0) of beta proportion distribution, it
#' should be sampled from. Defaults to a fixed value of \code{0.5}.
#' @param history_mix Specifies how cumulative history levels for
#' the same and opposite perceptual states are combined when used to
#' predict dominance phase durations, so that \code{history = history_mix
#' * history_same + (1 - history_mix) * history_opposite}. Either a single
#' number (range [0..1]) that will be used as a fixed level or a vector
#' of two numbers \code{c(mu, kappa)} that specifies, correspondingly, mean
#' (range [0..1]) and precision (>0) of beta proportion distribution, it
#' should be sampled from. Defaults to a fixed value of \code{0}
#' (predictions are made using cumulative history for the opposite state).
#' @param history_init Initial value for cumulative history computation. Either
#' a numeric scalar in [0..1] range or a vector of two numbers in [0..1] range.
#' In the latter case, two histories will start at different levels.
#' @param family String, distribution used to fit duration of perceptual dominance
#' phases. Options include \code{"gamma"} (default), \code{"lognormal"}, \code{"exgauss"}
#' (exponentially modulated Gaussian), and \code{"normal"}.
#' @param chains Number of chains for sampling.
#' @param cores Number of CPU cores to use for sampling. If omitted, All cores are used.
#' @param ... Additional arguments passed to [rstan::sampling()][rstan::sampling] function.
#'
#' @return
#' @export
#'
#' @examples
#' data(br_singleblock)
#' gamma_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
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
                        history_mix=0.5,
                        history_init=0,
                        family="gamma",
                        chains=4,
                        cores=NULL,
                        ...){

  # ----------------------------- Input checks -----------------------------

  ## --- 1. Check if data is a data.frame (or something that is convertible to it, e.g. a tibble) ---
  if (!is.data.frame(data)) stop("data parameter is not a data.frame")

  ## --- 2. Check the state column. ---
  # Must be
  # * a factor with two or three levels, or
  # * column with no more than two unique values
  if (!state %in% colnames(data)) stop(sprintf("Column '%s' for state variable is not in the table", state))
  if (sum(is.na(data[[state]]))>0) stop("State column contains NAs")
  if (is.factor(data[[state]])){
    if (length(levels(data[[state]])) < 2) stop("Too few levels for state column, should be either 2 or 3")
    if (length(levels(data[[state]])) > 3) stop("Too many levels for state column, should be either 2 or 3")
  }
  else {
    if (length(unique(data[[state]])) != 2) stop(sprintf("If state column is not a factor, it must have just two unique values, not %s", length(unique(data[[state]]))))

    # converting state column to a factor
    data[[state]] <- as.factor(data[[state]])
  }
  cumhist <- list()
  attr(cumhist, "class") <- "cumhist"

  cumhist$data <- tibble::tibble(state = as.integer(data[[state]]))


  ## --- 3. Check that either duration or onset a supplied ---
  if (is.null(duration) && is.null(onset)) stop("Either duration or onset time must be specified")
  if (is.null(duration)) { # we must infer duration of individual phases from onsets
    # check that column with this name exists
    if (!onset %in% colnames(data)) stop(sprintf("Column '%s' for onset variable is not in the table", onset))

    # NAs (can't handle those for the onset)
    if (sum(is.na(data[[onset]]))>0) stop("Onset column contains NAs")

    # check that is has a numeric type
    if (!is.numeric(data[[onset]])) stop("Onset column must be numeric")

    # computing duration, the values at the end of the runs will be trivially wrong
    # (negative) but this will be corrected once we have full participant/session/run data
    cumhist$data$duration <- c(diff(data[[onset]]), NA)
  }
  else { # duration can be used directly
    # check that column with this name exists
    if (!duration %in% colnames(data)) stop(sprintf("Column '%s' for duration variable is not in the table", duration))

    # check that is has a numeric type
    if (!is.numeric(data[[duration]])) stop("Duration column must be numeric")
    cumhist$data$duration <- data[[duration]]
  }

  ## --- 4. Check whether we have information about random_effect ---
  if (is.null(random_effect)) {
    # A single experimental session
    cumhist$data$random <- as.integer(1)
  }
  else {
    # check that column with this name exists
    if (!random_effect %in% colnames(data)) stop(sprintf("Column '%s' for random effect variable is not in the table", random_effect))
    if (sum(is.na(data[[random_effect]]))>0) stop("Random effect column contains NAs")
    cumhist$data$random <- as.integer(as.factor(data[[random_effect]]))
  }

  ## --- 5. Check whether we have information about experimental sessions ---
  if (is.null(session)) {
    # A single experimental session
    cumhist$data$session <- as.integer(1)
  }
  else {
    # check that column with this name exists
    if (!session %in% colnames(data)) stop(sprintf("Column '%s' for session variable is not in the table", session))
    if (sum(is.na(data[[session]]))>0) stop("Session column contains NAs")
    cumhist$data$session <- as.integer(as.factor(data[[session]]))
  }

  ## --- 6. Check whether we have information about individual runs ---
  if (is.null(run)) {
    # A single run
    cumhist$data$run <- as.integer(1)
  }
  else {
    # check that column with this name exists
    if (!run %in% colnames(data)) stop(sprintf("Column '%s' for run variable is not in the table", run))
    if (sum(is.na(data[[run]]))>0) stop("Run column contains NAs")
    cumhist$data$run <- as.integer(as.factor(data[[run]]))
  }

  ## --- 12. Check fixed effects
  if (is.null(fixed_effects)) {
    fit_fixed <- NULL
  }
  else {
    # Checking that all columns are valid
    for(current_fixed in fixed_effects){
      if (!current_fixed %in% colnames(data)) stop(sprintf("Column '%s' for fixed effect variable is not in the table", current_fixed))
      if (sum(is.na(data[[current_fixed]]))>0) stop(sprintf("Column '%s' contains NAs", current_fixed))
      if (!is.numeric(data[[current_fixed]])) stop(sprintf("Column '%s' is not numeric", current_fixed))
    }

    fit_fixed <- "fixed"
  }

  ## --- 7. Time series preprocessing (session_tmean, which durations are used for fitting, service flags)
  cumhist$data <-
    cumhist$data %>%

    # making sure last duration is 0 (as we won't be using it for fitting  anyhow)
    dplyr::group_by(random, session, run) %>%
    dplyr::mutate(duration = ifelse(dplyr::row_number()==dplyr::n(), 0, duration)) %>%

    # computing average clear percept duration for each experimental session
    dplyr::group_by(random, session) %>%
    dplyr::mutate(session_tmean = mean(duration[state<3], na.rm=TRUE)) %>%

    # marking out percept that will be used to fit history
    # To this end, we are ignoring
    # * any transition/mixed states (state==3)
    dplyr::ungroup() %>%
    dplyr::mutate(is_used = state != 3) %>%

    # * first durations for each state (as they had no chance to be properly history dependent)
    dplyr::group_by(random, session, run, state) %>%
    dplyr::mutate(is_used = ifelse(dplyr::row_number()==1, FALSE, is_used)) %>%

    # * last duration in each block (as it is not used for predictions)
    dplyr::group_by(random, session, run) %>%
    dplyr::mutate(is_used = ifelse(dplyr::row_number()==dplyr::n(), FALSE, is_used)) %>%

    # replacing any NAs with zeros
    dplyr::mutate(duration = tidyr::replace_na(duration, 0)) %>%

    # add time series start flag (first element for each run)
    dplyr::group_by(random, session, run) %>%
    dplyr::mutate(run_start  = ifelse(dplyr::row_number()==1, 1, 0))   %>%

    # turn data into the list
    dplyr::ungroup() %>%
    as.list()


  ## --- 8. Second durations check ---
  # Check that after all cleaning up used durations are strictly non-negative positive and
  # that durations used for history fitting are strictly positive (only the is_used once, so, ignoring
  # transitions and last durations)
  if (sum(cumhist$data$duration < 0) > 0) stop("Table contains negative durations.")
  if (sum(cumhist$data$duration[cumhist$data$is_used] == 0) > 0) stop("Table contains zero durations for clear states. Check original duration column for 0 and NAs or, perhaps, you forgot to specify participant/session/run variable(s)?")


  ## --- 9. Store number of random clusters ---
  cumhist$data$randomN <- length(unique(cumhist$data$random))

  ## --- 10. History parameters ---
  cumhist$data <- c(cumhist$data,
                    bistablehistory::evaluate_history_option("tau", tau, cumhist$data$randomN, Inf),
                    bistablehistory::evaluate_history_option("mixed_state", mixed_state, cumhist$data$randomN, 1),
                    bistablehistory::evaluate_history_option("history_mix", history_mix, cumhist$data$randomN, 1),
                    list("tau_prior"=c(log(1), 0.75),
                         "mixed_state_prior"=c(0, 1),
                         "history_mix_prior"=c(0, 1)))



  # --- 11. Check history_init
  cumhist$data$history_starting_values <- bistablehistory::evaluate_history_init(history_init)

  ## --- 12. Check selected distribution
  supported_families <- c("gamma"=1, "lognormal"=2, "exgauss"=3, "normal"=4)
  if (!family %in% names(supported_families)) stop(sprintf("Unsupported distribution family '%s'", family))



  # ----------------------------- Sampling -----------------------------
  # deciding on number of cores
  # if (is.null(cores)) cores = future::availableCores()
  #
  #   cumhist$fit <- rstan::sampling(stanmodels[[stanmodel_name]],
  #                                data=cumhist$data,
  #                                chains=chains,
  #                                cores=cores,
  #                                ...)

  cumhist
}


