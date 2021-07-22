#' Computes R-squared using LOO-residuals and Bayesian bootstrap.
#'
#' For detail refer to:
#' Andrew Gelman, Ben Goodrich, Jonah Gabry, and Aki Vehtari (2018).
#' R-squared for Bayesian regression models. The American Statistician,
#' doi:[10.1080/00031305.2018.1549100](https://doi.org/10.1080/00031305.2018.1549100) and
#' [https://avehtari.github.io/bayes_R2/bayes_R2.html](https://avehtari.github.io/bayes_R2/bayes_R2.html).
#'
#' @name loo_R2
#'
#' @param object An object of class [cumhist][cumhist-class()]
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to 89% credible interval.
#' @param ... Unused.
#'
#' @return vector of values or a data.frame with summary
#' @export
#'
#' @examples
#' #' \donttest{
#' br_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' loo_R2(br_fit)
#' }
loo_R2.cumhist <- function(object, summary=TRUE, probs=c(0.055, 0.945), ...){
  # extracting outcome, predicted outcome, and its log likelihood
  y <- object$data$clear_duration
  ypred <- predicted <- predict(object, summary=FALSE)
  ll <- rstan::extract(object$stanfit, pars="log_lik")$log_lik

  # magic (see references in the description)
  M <- length(object$stanfit@sim$n_save)
  N <- object$stanfit@sim$n_save[[1]] - object$stanfit@sim$warmup
  r_eff <- loo::relative_eff(exp(ll), chain_id = rep(1:M, each = N))
  psis_object <- loo::psis(log_ratios = -ll, r_eff = r_eff)
  ypredloo <- loo::E_loo(ypred, psis_object, log_ratios = -ll)$value
  eloo <- ypredloo-y
  n <- length(y)
  rd <- bayesboot::rudirichlet(4000, n)
  vary <- (rowSums(sweep(rd, 2, y^2, FUN = "*")) -
             rowSums(sweep(rd, 2, y, FUN = "*"))^2)*(n/(n-1))
  vareloo <- (rowSums(sweep(rd, 2, eloo^2, FUN = "*")) -
                rowSums(sweep(rd, 2, eloo, FUN = "*")^2))*(n/(n-1))
  looR2 <- 1-vareloo/vary
  looR2[looR2 < -1] <- -1
  looR2[looR2 > 1] <- 1

  if (!summary) return(looR2)

  # average
  r2_mean <- apply(as.matrix(looR2), MARGIN=2, FUN=mean)
  if (is.null(probs)) return(r2_mean)

  # full summary
  tibble::tibble(R2 = r2_mean) %>%
    dplyr::bind_cols(tibble::as_tibble(t(apply(as.matrix(looR2), MARGIN=2, FUN=quantile, probs=probs))))
}

#' @export
#' @keywords internal
loo_R2 <- function(object, ...) { UseMethod("loo_R2") }
