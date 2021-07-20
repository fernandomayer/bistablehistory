#' Computes R-squared using Bayesian R-squared approach.
#' For detail refer to:
#' Andrew Gelman, Ben Goodrich, Jonah Gabry, and Aki Vehtari (2018).
#' R-squared for Bayesian regression models. The American Statistician,
#' doi:10.1080/00031305.2018.1549100.
#'
#' @name R2
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
#' R2(br_fit)
#' }
R2.cumhist <- function(object, summary=TRUE, probs=c(0.055, 0.945), ...){
  predicted <- predict(object, summary=FALSE)

  # compute residuals
  e <- -1 * sweep(predicted, 2, object$data$clear_duration)

  # compute variances
  var_pred <- apply(predicted, 1, stats::var)
  var_e <- apply(e, 1, stats::var)

  # compute R2
  r2s <- var_pred / (var_pred + var_e)

  # returning
  if (!summary) {
    r2s
  }

  r2_mean <- apply(as.matrix(r2s), MARGIN=2, FUN=mean)
  if (is.null(probs)) return(r2_mean)

  # full summary
  tibble::tibble(R2 = r2_mean) %>%
    dplyr::bind_cols(tibble::as_tibble(t(apply(as.matrix(r2s), MARGIN=2, FUN=quantile, probs=probs))))
}

#' @export
#' @keywords internal
R2 <- function(object, ...) { UseMethod("R2") }
