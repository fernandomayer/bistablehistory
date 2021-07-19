#' Single run for binocular rivalry stimulus
#'
#' A single subject / single run dataset for binocular rivalry.
#'
#' @format A data frame with 76 rows and 6 variables:
#' \describe{
#'   \item{Observer}{Participant ID, all rows contain _"ap"_}
#'   \item{Group}{Display, all rows contain _"BR"_}
#'   \item{Block}{Run / block index, all rows contain _1_}
#'   \item{Time}{Time relative to the run onset in _seconds_}
#'   \item{State}{Index of a perceptually dominant state, _1_, _2_ - clear perceptual state, _3_ mixed / transition phase}
#'   \item{Duration}{Duration of a dominance phase in _seconds_. Note that the duration for the last dominance phase is curtailed and, therefore, set to zero.}
#' }
#' @source  \doi{10.1167/11.10.12}
"br_singleblock"


#' Multirun data for two participants, kinectic-depth effect display
#'
#' @format A data frame with 1186 rows and 5 variables:
#' \describe{
#'   \item{Observer}{Participant ID}
#'   \item{Block}{Run / block index}
#'   \item{State}{Factor variable for state with levels \code{-1} and \code{1} coding two clear perceptual states and \code{-2} the mixed / transition phase}
#'   \item{Time}{Time relative to the run onset in _seconds_}
#'   \item{Duration}{Duration of a dominance phase in _seconds_. Note that the duration for the last dominance phase is curtailed and, therefore, set to zero.}
#' }
#' @source  \doi{10.1167/11.10.12}
"kde_two_observers"
