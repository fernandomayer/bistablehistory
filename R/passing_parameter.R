#' Packages gamma parameter either as fixed or as sampled from priors
#'
#' @param param A scalar (fixed value) or two number vector (prior for sampling)
#' @param param_name String, parameter name
#'
#' @return List with "fit_" + param_name, "fixed_"+param_name, and param_name+"_prior"
#' properties
#' @export
#' @keywords internal
#'
#' @examples
#' pass_gamma(1, "norm_tau")
pass_gamma <- function(param, param_name){
  param_list <- list()
  if (length(param) == 1) {
    # fixed
    param_list[[stringr::str_c("fit_", param_name)]] <- 0
    param_list[[stringr::str_c("fixed_", param_name)]] <- param
    param_list[[stringr::str_c(param_name, "_prior")]] <- c(5, 0.2) # unused but must be passed
  }
  else {
    # sampled
    param_list[[stringr::str_c("fit_", param_name)]] <- 1
    param_list[[stringr::str_c("fixed_", param_name)]] <- 1 # unused but must be passed
    param_list[[stringr::str_c(param_name, "_prior")]] <- param
  }

  param_list
}

#' Packages beta proportion parameter either as fixed or as sampled from priors
#'
#' @param param A scalar (fixed value) or two number vector (prior for sampling)
#' @param param_name String, parameter name
#'
#' @return List with "fit_" + param_name, "fixed_"+param_name,
#' param_name+"_prior_mu", and param_name+"_prior_kappa"
#' properties
#' @export
#' @keywords internal
#'
#' @examples
#' pass_beta_prop(0.5, "mixed_state")
pass_beta_prop <- function(param, param_name){
  param_list <- list()
  if (length(param) == 1) {
    # fixed
    param_list[[stringr::str_c("fit_", param_name)]] <- 0
    param_list[[stringr::str_c("fixed_", param_name)]] <- param
    param_list[[stringr::str_c(param_name, "_prior_mu")]] <- 0.5 # unused but must be passed
    param_list[[stringr::str_c(param_name, "_prior_kappa")]] <- 2 # unused but must be passed
  }
  else {
    # sampled
    param_list[[stringr::str_c("fit_", param_name)]] <- 1
    param_list[[stringr::str_c("fixed_", param_name)]] <- 0.5 # unused but must be passed
    param_list[[stringr::str_c(param_name, "_prior_mu")]] <- param[1]
    param_list[[stringr::str_c(param_name, "_prior_kappa")]] <- param[2]
  }

  param_list
}
