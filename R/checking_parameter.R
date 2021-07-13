#' Checks validity of a fixed value or parameters
#' for a Gamma prior distribution. Raises error, if one of
#' the checks fail.
#'
#' @param param Either a single value or a vector of two parameters for Gamma distribution
#' @param param_name Name of the parameter for error message
#'
#' @return
#' @export
#'
#' @examples
#' check_fixed_or_gamma_prior(1.0, "norm_tau")
#' check_fixed_or_gamma_prior(c(5, 2), "norm_tau")
#' check_fixed_or_gamma_prior(-1, "norm_tau")
check_fixed_or_gamma_prior <- function(param, param_name) {
  if (!is.numeric(param)) stop(sprintf("%s parameter must be numeric scalar or two element vector", param_name))
  if (length(param) > 2) stop(sprintf("%s parameter must be numeric scalar or two element vector", param_name))
  if (length(param) == 1) {
    # fixed value
    if (param <= 0) stop(sprintf("Fixed %s parameter must be strictly positive", param_name))
    if (!is.finite(param)) stop(sprintf("Fixed %s parameter must be a finite strictly positive number", param_name))
  }
  else {
    # prior
    if (sum(param <= 0) > 0) stop(sprintf("Parameters for %s Gamma prior distribution must be strictly positive", param_name))
    if (sum(!is.finite(param))>0) stop(sprintf("Parameters for %s Gamma prior distribution must be finite strictly positive numbers", param_name))
  }
}

#' Checks validity of a fixed value or parameters
#' for a Beta proportion prior distribution. Raises error,
#' if one of the checks fail.
#'
#' @param param Either a single value or a vector of two parameters for Beta proportion distribution
#' @param param_name Name of the parameter for error message
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
#' check_fixed_or_beta_prior(0.5, "mixed_state")
#' check_fixed_or_beta_prior(c(0.5, 1), "mixed_state")
#' check_fixed_or_beta_prior(-1, "mixed_state")
check_fixed_or_beta_prior <- function(param, param_name) {
  if (!is.numeric(param)) stop(sprintf("%s parameter must be numeric scalar or two element vector", param_name))
  if (length(param) > 2) stop(sprintf("%s parameter must be numeric scalar or two element vector", param_name))
  if (length(param) == 1) {
    # fixed value
    if (param < 0 || param >1) stop(sprintf("Fixed %s parameter must be in [0..1] range", param_name))
    if (!is.finite(param)) stop(sprintf("Fixed %s parameter must be a finite", param_name))
  }
  else {
    # prior
    if (param[1] < 0 || param[1] > 1) stop(sprintf("First value for %s Beta proportion prior distribution (mu) must be in range [0..1]", param_name))
    if (param[2] <= 0 ) stop(sprintf("Second value for %s Beta proportion prior distribution (kappa) must be positive", param_name))
    if (sum(!is.finite(param))>0) stop(sprintf("Parameters for %s Beta proportion prior distribution must be finite", param_name))
  }
}

#' Checks valid range for initial history values. Returns a vector of two,
#' thus replicates a scalar to be used for both initial values
#'
#' @param param Either a single value or a vector of two number in [0..1] range
#' @param param_name Name of the parameter for error message
#'
#' @return A vector of two, scalar value is repeated.
#' @export
#' @keywords internal
#'
#' @examples
check_history_init <- function(param, param_name){
  if (!is.numeric(param)) stop(sprintf("%s parameter must be numeric scalar or two element vector", param_name))
  if (length(param) > 2) stop(sprintf("%s parameter must be numeric scalar or two element vector", param_name))
  if (sum(param<0 | param>1) > 0) stop(sprintf("%s parameter must be in [0..1] range", param_name))
  if (length(param) == 1)
    param <- c(param, param)
  param
}
