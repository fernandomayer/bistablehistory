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
#' }
#' @source  \doi{10.1167/11.10.12}
"br_singleblock"
