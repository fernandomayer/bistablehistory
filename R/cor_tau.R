cor_tau <- function(object, method) { UseMethod("cor_tau") }


#' Computes correlation between observed and
#' predicted dominance phase duration for each tau.
#'
#' @param cumhist_fit A fitted cumhist object.
#' @param method String, correlation coefficient to
#' be computed. Could be \code{"pearson"},
#' \code{"kendall"}, or \code{"spearman"}. Defaults to
#' \code{"pearson"}.
#'
#' @return A data.frame with columns \code{normalized_tau}
#' and \code{R}
#' @export
#'
#' @examples
#' data(br_singleblock)
#' gamma_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' R <- cor_tau(gamma_fit)
#' plot(R$tau, R$r)
#'
cor_tau.cumhist <- function(cumhist_fit, method="pearson"){
  predictions <- rstan::extract(cumhist_fit$fit, pars="predicted_duration")$predicted_duration

  data.frame(tau = normalized_tau <- rstan::extract(cumhist_fit$fit, pars="normalized_tau[1]")[["normalized_tau[1]"]],
             r = apply(predictions, 1, FUN=cor, y=cumhist_fit$data$clear_duration, method=method))
}
