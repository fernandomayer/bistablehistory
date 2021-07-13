#' Computes widely applicable information criterion
#' (WAIC).
#'
#' @description Computes widely applicable information criterion
#' via [loo][loo-package] library. It can be used for a model comparison via
#' [loo::loo_compare()][loo::loo_compare] function.
#'
#' @param cumhist_fit A fitted cumhist object
#'
#' @return A named list, see [loo::waic()] for details.
#' @export
#'
#' @examples
#' data(br_singleblock)
#' gamma_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' waic_gamma <- waic(gamma_fit)
#' lognormal_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration", family="lognormal")
#' waic_lognormal <- waic(lognormal_fit)
#' loo::loo_compare(waic_gamma, waic_lognormal)

waic.cumhist <- function(cumhist_fit) {
  log_lik <- loo::extract_log_lik(cumhist_fit$fit, "log_lik", merge_chains = FALSE)
  loo::waic(log_lik)
}
