#' Evaluates values for a fixed history parameter
#'
#' Expects either a single value within a valid range or
#' \code{randomN} values.
#'
#' @param param_name Name of the parameter.
#' @param param_value A single value or \code{randomN} numeric values.
#' @param randomN Number of levels for the random variable.
#' @param upperLimit Upper limit for a valid \code{param_value}.
#'
#' @return A numeric vector \code{randomN} long.
#'
#' @export
#' @keywords internal
#'
#' @examples
#' check_fixed_history_parameter("tau", 1, 10, Inf)
check_fixed_history_parameter <- function(param_name, param_value, randomN, upperLimit){
  if (!is.numeric(param_value)) stop(sprintf("%s must be numeric", param_name))
  if (!is.atomic(param_value)) stop(sprintf("%s must be an atomic vector", param_name))
  if (length(param_value) != 1 && length(param_value) != randomN) {
    stop(sprintf("Number ofvalues for %s must be either 1 or %d (number of random clusters)", param_name, randomN))
  }
  if (any(param_value < 0)) stop(sprintf("%s must be non-negative", param_name))
  if (any(param_value > upperLimit)) stop(sprintf("%s must be less or equal to %g", param_name, upperLimit))

  if (length(param_value) != randomN) param_value <- rep(param_value, randomN)
  param_value
}


#' Checks for validity of values for use as normal distribution parameters.
#'
#' Should a pair of numeric values, second value should be non-zero.
#' Stops execution with an error.
#'
#' @param values Parameters for normal distribution.
#' @param parameter Name of the parameter for which the prior is defined.
#'
#' @return Logical TRUE, if none of the tests fail
#' @export
#' @keywords internal
#'
#' @examples
#' check_normal_prior(c(0, 1), "tau")
check_normal_prior <- function(values, parameter){
  if (!is.numeric(values)) stop(sprintf("prior parameters for %s is not a numeric", parameter))
  if (length(values) != 2) stop(sprintf("prior parameters for %s should be a pair of values", parameter))
  if (values[2] <= 0) stop(sprintf("variance for %s should be strictly positive", parameter))
  TRUE
}

