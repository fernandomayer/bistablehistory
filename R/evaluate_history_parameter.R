#' Evaluates whether and how to fit a cumulative history parameter.
#'
#' Evaluation is based on the \code{param_value}.
#' 1. A single positive number (>0) that is used for all participants and runs.
#' 2. \code{NULL} (default) -  a _single_ value will be fitted for all participants
#' and runs, also applied if \code{randomN == 1}.
#' 3. \code{"random"} - an independent value is fitted for each random cluster.
#' 4. \code{"1|random"}- a value for a random cluster is sampled from a population
#' distribution, i.e., pooled parameter values via a multilevel model.
#'
#' @param param_name Name of the parameter.
#' @param param_value Value from the \code{\link{fit_cumhist}} function call.
#' @param randomN Number of levels for the random variable.
#' @param upperLimit Upper limit for a valid \code{param_value}.
#'
#' @return a list with \code{<param_name>_option} and \code{fixed_<param_name>}.
#' @export
#' @keywords internal
#'
#' @examples
#' evaluate_history_option("tau", 1, 1, Inf)
evaluate_history_option <- function(param_name, param_value, randomN, upperLimit){
  param_list <- list()
  if (is.null(param_value)) {
    param_list[[paste0(param_name, "_option")]] <- 2L # fit single value
    param_list[[paste0("fixed_", param_name)]] <- 0 # not-used
  }
  else if (is.numeric(param_value) && is.atomic(param_value) && length(param_value) == 1) {
    if (param_value < 0) stop(sprintf("%s must be non-negative", param_name))
    if (param_value > upperLimit) stop(sprintf("%s must be less or equal to %g", param_name, upperLimit))

    param_list[[paste0(param_name, "_option")]] <- 1L # constant
    param_list[[paste0("fixed_", param_name)]] <- param_value
  }
  else if (param_value == "random") {
    if (randomN == 1) {
      param_list[[paste0(param_name, "_option")]] <- 1L # fit single value for a single random cluster
    }
    else {
      param_list[[paste0(param_name, "_option")]] <- 3L # independent random
    }
    param_list[[paste0("fixed_", param_name)]] <- 0 # not-used
  }
  else if (param_value == "1|random") {
    if (randomN == 1) {
      param_list[[paste0(param_name, "_option")]] <- 1L # fit single value for a single random cluster
    }
    else {
      param_list[[paste0(param_name, "_option")]] <- 4L # pooled random
    }
    param_list[[paste0("fixed_", param_name)]] <- 0 # not-used
  }
  else {
    stop(sprintf("Unknown option for %s", param_name))
  }

  param_list
}


#' Evaluates validity of initial history values.
#'
#' Checkes number and range of values. If a scalar is passed, uses same value
#' for both states.
#'
#' @param history_init Either a single value or a pair of values within [0..1] range.
#'
#' @return A vector of two values
#' @export
#' @keywords internal
#'
#' @examples
#' evaluate_history_init(0.5)
evaluate_history_init <- function(history_init){
  if (!(is.atomic(history_init) && is.numeric(history_init))){
    stop("history_init parameter must be numeric")
  }
  if (length(history_init) < 1 || length(history_init) > 2) {
    stop("history_init must be a scalar or a pair of values")
  }
  if (any(history_init < 0) || any(history_init > 1)) {
    stop("history_init values must be within 0..1 range")
  }
  if (length(history_init) == 1){
    return(c(history_init, history_init))
  }

  history_init
}
