#' The 'bistablehistory' package.
#'
#' @description A DESCRIPTION OF THE PACKAGE
#'
#' @docType package
#' @name bistablehistory-package
#' @aliases bistablehistory
#' @useDynLib bistablehistory, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom bayesplot mcmc_areas
#' @importFrom dplyr filter group_by mutate n row_number select ungroup
#' @importFrom future availableCores
#' @importFrom loo extract_log_lik loo relative_eff waic
#' @importFrom magrittr %>%
#' @importFrom rstan sampling
#' @importFrom tibble tibble
#'
#' @references
#' Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.19.3. https://mc-stan.org
#'
NULL
