#' Extract the fixed-effects estimates
#'
#' Extracts models fixed-effect terms for every modeled distribution parameter.
#'
#' @param object An object of class [cumhist][cumhist-class()]
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to 89% credible interval.
#' @param ... Unused.
#'
#' @return \code{tibble} with values or summary, \code{NULL} if not fixed effects were used.
#' @export
#'
#' @examples
#' #' \donttest{
#' br_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration", fixed="Time")
#' fixef(br_fit)
#' }
fixef.cumhist <- function(object, summary=TRUE, probs=c(0.055, 0.945), ...){
  if (object$data$fixedN == 0) {
    message("No fixed effects were specified for the model.")
    return()
  }

  # distribution parameter names
  param_names <- list("gamma" = c("shape", "scale"),
                      "normal" = "mu",
                      "lognormal" = "mu")

  # extracting estimates
  bF <- rstan::extract(object$stanfit, pars="bF")$bF

  terms <-
    # laying out ordered distribution / fixed term parameter grid
    expand.grid(DistributionParameter=1:dim(bF)[2], Term=1:dim(bF)[3]) %>%
    group_by(DistributionParameter, Term) %>%
    summarise(DistributionParameter = rep(DistributionParameter, dim(bF)[1]),
              Term = rep(Term, dim(bF)[1]),
              .groups="keep") %>%
    ungroup() %>%

    # adding in the estimates
    mutate(Estimate = c(bF)) %>%

    # adding fixed term names
    mutate(Term = factor(Term, levels = 1:dim(bF)[3], labels = colnames(object$data$fixed_clear))) %>%

    # adding distribution parameter names
    mutate(DistributionParameter = factor(DistributionParameter, levels=1:dim(bF)[2], labels=param_names[[object$family]]))

  if (!summary) return(terms)

  # mean
  avg_terms <-
    terms %>%
    group_by(DistributionParameter, Term) %>%
    summarise(Estimate = mean(Estimate), .groups="drop")

  # quantiles
  if (!is.null(probs)){
    term_quantiles <-
      terms %>%
      group_by(DistributionParameter, Term) %>%
      tidyr::nest() %>%
      dplyr::mutate(CI = purrr::map(data, ~tibble::as_tibble(t(apply(as.matrix(.$Estimate), MARGIN=2, FUN=quantile, probs=probs))))) %>%
      select(-data) %>%
      tidyr::unnest(cols=CI)

    avg_terms <- dplyr::left_join(avg_terms, term_quantiles, by=c("DistributionParameter", "Term"))
  }
  avg_terms
}

#' @export
#' @keywords internal
fixef <- function(object, ...) { UseMethod("fixef") }
