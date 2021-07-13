#' Computes an efficient approximate leave-one-out
#' cross-validation via loo library. It can be used
#' for a model comparison via loo::loo_compare() function.
#'
#' @param cumhist_fit A fitted cumhist object
#'
#' @return A named list, see loo::loo() for details.
#' @export
#'
#' @examples
#' data(br_singleblock)
#' gamma_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' loo_gamma <- loo(gamma_fit)
#' lognormal_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration", family="lognormal")
#' loo_lognormal <- loo(lognormal_fit)
#' loo::loo_compare(loo_gamma, loo_lognormal)

loo.cumhist <- function(cumhist_fit) {
  log_lik <- loo::extract_log_lik(cumhist_fit$fit, "log_lik", merge_chains = FALSE)

  if (length(dimnames(as.array(cumhist_fit$fit))$chains) == 1){
    # single chaing, no relative_eff
    return( loo::loo(log_lik, cores = future::availableCores()) )
  }


  # we have more than one chain, can compute relative_eff
  r_eff <- loo::relative_eff(exp(log_lik), cores=future::availableCores())
  loo::loo(log_lik, r_eff = r_eff, cores = future::availableCores())
}
