#' Posterior interval plots for key parameters. Uses bayesplot::mcmc_intervals
#' routine and supplies better (family-matching) labels for plotted parameters.
#'
#' @param cumhist_fit A fitted cumhist object.
#'
#' @return
#' @export
#'
#' @examples
#' data(br_singleblock)
#' gamma_fit <- bistablehistory::fit_cumhist(br_singleblock, state="State", duration="Duration")
#' plot(gamma_fit)
plot.cumhist <- function(cumhist_fit, ...){
  if (cumhist_fit$family == "gamma"){
    common_pars <- c("normalized_tau[1]", "a[1]", "a[2]", "bHistory[1]", "bHistory[2]")
    common_names <- c("normalized_tau", "Intercept(shape)", "Intercept(scale)", "History(shape)", "History(scale)")

    gamma_param <- c("shape", "scale")
    if (cumhist_fit$fixedN > 0){
      fixed_names <- c()
      fixed_pars <- c()
      for(iFixed in 1:cumhist_fit$fixedN){
        for(iGammaParam in 1:2){
          fixed_pars <- c(fixed_pars, sprintf("bFixed[%d,%d]", iGammaParam, iFixed))
          fixed_names <- c(fixed_names, sprintf("%s(%s)", cumhist_fit$fixed_names[iFixed], gamma_param[iGammaParam]))
        }
      }
    }
    else {
      fixed_names <- NULL
      fixed_pars <- NULL
    }

    bayesplot::mcmc_intervals(as.matrix(cumhist_fit$fit, pars = c(common_pars, fixed_pars))) +
      ggplot2::scale_y_discrete(labels = c(common_names, fixed_names))
  }
  else if (cumhist_fit$family %in% c("normal", "lognormal")){
    bayesplot::mcmc_intervals(as.matrix(cumhist_fit$fit, pars = c("normalized_tau[1]", "a", "bHistory", "sigma"))) +
      ggplot2::scale_y_discrete(labels = c("normalized_tau", "a", "bHistory", "sigma"))
  }
  else if (cumhist_fit$family == "exgauss"){
    bayesplot::mcmc_intervals(as.matrix(cumhist_fit$fit, pars = c("normalized_tau[1]", "a[1]", "a[2]", "bHistory[1]", "bHistory[2]", "sigma"))) +
      ggplot2::scale_y_discrete(labels = c("normalized_tau", "a_mu", "a_lambda", "bHistory_mu", "bHistory_lambda", "sigma"))

  }
  else {
    stop(sprintf("Unsupported family '%s'", cumhist_fit$family))
  }
}



