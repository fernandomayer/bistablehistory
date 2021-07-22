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
#' @importFrom rstan sampling
#' @importFrom loo loo waic relative_eff psis E_loo
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_cols row_number n mutate group_by ungroup
#' @importFrom future availableCores
#' @importFrom bayesboot rudirichlet
#' @importFrom boot inv.logit
#'
#' @references
#' Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.21.2. https://mc-stan.org
#'
NULL
